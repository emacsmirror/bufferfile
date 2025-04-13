;;; bufferfile.el --- Rename/Delete/Copy Files and Associated Buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.4
;; URL: https://github.com/jamescherti/bufferfile.el
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; This package provides helper functions to delete and rename buffer files:
;; - bufferfile-rename: Renames the file visited by the current buffer and
;;   updates the buffer name for all associated buffers, including clones and
;;   indirect buffers. It also ensures that buffer-local features referencing
;;   the file, such as Eglot, are correctly updated to reflect the new file
;;   name.
;; - bufferfile-delete: Delete the file associated with a buffer and kill all
;;   buffers visiting the file, including clones/indirect buffers.
;; - bufferfile-copy: Copies the file visited by the current buffer to a new
;;   file.
;;
;; (The functions above also ensures that any modified buffers are saved prior
;; to executing operations like renaming, deleting, or copying.)
;;
;; Installation from MELPA
;; -----------------------
;; (use-package bufferfile
;;   :ensure t
;;   :straight (bufferfile
;;              :type git
;;              :host github
;;              :repo "jamescherti/bufferfile.el"))

;;; Code:

(require 'cl-lib)
(require 'hi-lock)

;; Customizations

(defgroup bufferfile nil
  "Delete or rename buffer files."
  :group 'bufferfile
  :prefix "bufferfile-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/bufferfile.el"))

(defcustom bufferfile-use-vc nil
  "If non-nil, enable using version control (VC) when available.
When this option is enabled and the file being deleted or renamed is under VC,
the renaming operation will be handled by the VC backend."
  :type 'boolean
  :group 'bufferfile)

(defcustom bufferfile-verbose nil
  "If non-nil, display messages during file renaming operations.
When this option is enabled, messages will indicate the progress
and outcome of the renaming process."
  :type 'boolean
  :group 'bufferfile)

;;; Variables

(defvar bufferfile-pre-rename-functions nil
  "Hook run before renaming a file.
Each function receives three arguments: (previous-path new-path list-buffers).")

(defvar bufferfile-post-rename-functions nil
  "Hook run after renaming a file.
Each function receives three arguments: (previous-path new-path list-buffers).")

(defvar bufferfile-pre-delete-functions nil
  "Hook run before deleting a file.
Each function receives two arguments: (path list_buffers).")

(defvar bufferfile-post-delete-functions nil
  "Hook run after deleting a file.
Each function receives two arguments: (path list_buffers).")

(defvar bufferfile-message-prefix "[bufferfile] "
  "Prefix used for messages and errors related to bufferfile operations.")

;;; Helper functions

(defun bufferfile--error (&rest args)
  "Signal an error with `bufferfile-message-prefix' followed by formatted ARGS.
ARGS are formatted as in `format'."
  (user-error "%s%s" bufferfile-message-prefix (apply #'format args)))

(defun bufferfile--message (&rest args)
  "Display a message with '[bufferfile]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat bufferfile-message-prefix (car args)) (cdr args)))

(defun bufferfile--get-list-buffers (filename)
  "Return a list of buffers visiting the specified FILENAME.

FILENAME is the absolute path of the file to check for associated buffers.

Iterates through all buffers and performs the following check: If a buffer is
visiting the specified FILENAME (based on the true file path), it is added to
the resulting list.

Returns a list of buffers that are associated with FILENAME."
  (let ((filename (file-truename filename))
        (list-buffers nil))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (let* ((base-buf (or (buffer-base-buffer buf) buf))
               (buf-filename (when base-buf (buffer-file-name base-buf))))
          (when (and buf-filename
                     (string-equal filename (file-truename buf-filename)))
            (push buf list-buffers)))))
    list-buffers))

(defun bufferfile--get-buffer-filename (&optional buffer)
  "Return the absolute file path of BUFFER.
If BUFFER is nil, use the current buffer.
Return nil if the buffer is not associated with a file."
  (unless buffer
    (setq buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((filename (buffer-file-name (buffer-base-buffer))))
        (unless filename
          (bufferfile--error "The buffer '%s' is not associated with a file"
                             (buffer-name)))

        (unless (file-regular-p filename)
          (bufferfile--error "The file '%s' does not exist on disk"
                             filename))

        (expand-file-name filename)))))

(defun bufferfile--read-dest-file-name (filename)
  "Prompt for a destination file name different from FILENAME."
  (let* ((basename (file-name-nondirectory filename))
         (new-filename (read-file-name
                        (format "%sCopy '%s' to: "
                                bufferfile-message-prefix
                                basename)
                        (file-name-directory filename)
                        nil
                        nil
                        basename
                        #'(lambda(filename)
                            (file-regular-p filename)))))
    (unless new-filename
      (bufferfile--error "A new file name must be specified"))

    (when (string= (file-truename filename)
                   (file-truename new-filename))
      (bufferfile--error
       "Ignored because the destination is the same as the source"))
    new-filename))

;;; Rename file

(defun bufferfile--rename-all-buffer-names (old-filename new-filename)
  "Update buffer names to reflect the renaming of a file.
OLD-FILENAME and NEW-FILENAME are absolute paths as returned by
`expand-file-name'.

For all buffers associated with OLD-FILENAME, update the buffer names to use
NEW-FILENAME.

This includes indirect buffers whose names are derived from the old filename."
  (setq old-filename (expand-file-name old-filename))
  (setq new-filename (expand-file-name new-filename))
  (let ((basename (file-name-nondirectory old-filename))
        (new-basename (file-name-nondirectory new-filename)))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when-let* ((base-buffer (buffer-base-buffer)))
            ;; Indirect buffer
            (let* ((base-buffer-filename
                    (let ((file-name (buffer-file-name base-buffer)))
                      (when file-name
                        (expand-file-name file-name)))))
              (when (and base-buffer-filename
                         (string= (file-truename new-filename)
                                  (file-truename base-buffer-filename)))
                (let ((indirect-buffer-name (buffer-name)))
                  (let* ((new-buffer-name (concat new-basename
                                                  (substring
                                                   indirect-buffer-name
                                                   (length basename)))))
                    (when (string-prefix-p basename indirect-buffer-name)
                      (when new-buffer-name
                        (rename-buffer new-buffer-name t)))))))))))))

(defun bufferfile-rename-file (filename
                               new-filename
                               &optional ok-if-already-exists)
  "Rename FILENAME to NEW-FILENAME.

This function updates:
- The filename name on disk,
- The buffer name,
- All the indirect buffers or other buffers associated with the old filename.

Hooks in `bufferfile-pre-rename-functions' and
`bufferfile-post-rename-functions' are run before and after the renaming
process.

Signal an error if a filename NEWNAME already exists unless OK-IF-ALREADY-EXISTS
is non-nil."
  (let (list-buffers)
    (when (and (not ok-if-already-exists)
               (file-exists-p new-filename))
      (unless (y-or-n-p
               (format
                "Destination file '%s' already exists. Do you want to overwrite it?"
                new-filename))
        (bufferfile--error
         "Rename failed: Destination filename already exists: %s"
         new-filename)))

    (setq list-buffers (bufferfile--get-list-buffers filename))

    (run-hook-with-args 'bufferfile-pre-rename-functions
                        filename
                        new-filename
                        list-buffers)

    (when bufferfile-use-vc
      (require 'vc))

    (if (and bufferfile-use-vc
             (vc-backend filename))
        (progn
          ;; Rename the file using VC
          (vc-rename-file filename new-filename)
          (when bufferfile-verbose
            (bufferfile--message
             "VC Rename: %s -> %s"
             (abbreviate-file-name filename)
             (abbreviate-file-name new-filename))))
      ;; Rename the file
      (rename-file filename new-filename ok-if-already-exists)
      (when bufferfile-verbose
        (bufferfile--message "Rename: %s -> %s"
                             (abbreviate-file-name filename)
                             (abbreviate-file-name new-filename))))

    (set-visited-file-name new-filename t t)

    ;; Update all buffers pointing to the old filename Broken
    (bufferfile--rename-all-buffer-names filename new-filename)

    (dolist (buf list-buffers)
      (with-current-buffer buf
        ;; Fix Eglot
        (when (and (fboundp 'eglot-current-server)
                   (fboundp 'eglot-shutdown)
                   (fboundp 'eglot-managed-p)
                   (fboundp 'eglot-ensure)
                   (eglot-managed-p))
          (let ((server (eglot-current-server)))
            (when server
              ;; Restart eglot
              (let ((inhibit-message t))
                (eglot-shutdown server))
              (eglot-ensure))))))

    (run-hook-with-args 'bufferfile-post-rename-functions
                        filename
                        new-filename
                        list-buffers)))

;;;###autoload
(defun bufferfile-rename (&optional buffer)
  "Rename the current file of that BUFFER is visiting.
This command updates:
- The file name on disk,
- the buffer name,
- all the indirect buffers or other buffers associated with the old file.

Hooks in `bufferfile-pre-rename-functions' and
`bufferfile-post-rename-functions' are run before and after the renaming
process."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (let* ((filename (bufferfile--get-buffer-filename))
           (original-buffer (or (buffer-base-buffer) (current-buffer))))
      (with-current-buffer original-buffer
        (when (buffer-modified-p)
          (let ((save-silently (not bufferfile-verbose)))
            (save-buffer)))

        (let ((new-filename (bufferfile--read-dest-file-name filename)))
          (bufferfile-rename-file filename new-filename t))))))

;;; Delete file

;;;###autoload
(defun bufferfile-delete (&optional buffer)
  "Kill BUFFER and delete file associated with it.
Delete the file associated with a buffer and kill all buffers visiting the file,
including indirect buffers or clones.
If BUFFER is nil, operate on the current buffer.

Hooks in `bufferfile-pre-delete-functions' and
`bufferfile-post-delete-functions' are run before and after the renaming
process."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let* ((buffer (or buffer (current-buffer)))
           (filename nil))
      (unless (buffer-live-p buffer)
        (bufferfile--error "The buffer '%s' is not alive"
                           (buffer-name buffer)))

      (setq filename (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
      (unless filename
        (bufferfile--error "The buffer '%s' is not visiting a file"
                           (buffer-name buffer)))
      (setq filename (expand-file-name filename))

      (when (yes-or-no-p (format "Delete file '%s'?"
                                 (file-name-nondirectory filename)))
        (when bufferfile-use-vc
          (require 'vc))
        (let ((vc-managed-file (when bufferfile-use-vc
                                 (vc-backend filename)))
              (list-buffers (bufferfile--get-list-buffers filename)))
          (dolist (buf list-buffers)
            (with-current-buffer buf
              (when (buffer-modified-p)
                (let ((save-silently t))
                  (save-buffer)))))

          (run-hook-with-args 'bufferfile-pre-delete-functions
                              filename list-buffers)

          (when vc-managed-file
            ;; Revert version control changes before killing the buffer;
            ;; otherwise, `vc-delete-file' will fail to delete the file
            (when (not (vc-up-to-date-p filename))
              (with-current-buffer buffer
                (if (fboundp 'vc-revert-file)
                    (vc-revert-file filename)
                  (bufferfile--error "vc-revert-file has been not declared")))))

          ;; Find file first
          (when-let* ((dir-buffer (find-file (file-name-directory filename))))
            (with-current-buffer dir-buffer
              (when (and (derived-mode-p 'dired-mode)
                         (fboundp 'dired-goto-file))
                (dired-goto-file filename)))
            (switch-to-buffer dir-buffer nil t))

          ;; Kill buffer
          (dolist (buf list-buffers)
            (with-current-buffer buf
              (when (and (fboundp 'eglot-current-server)
                         (fboundp 'eglot-shutdown)
                         (fboundp 'eglot-managed-p)
                         (eglot-managed-p))
                (let ((server (eglot-current-server)))
                  (when server
                    ;; Do not display errors such as:
                    ;; [jsonrpc] (warning) Sentinel for EGLOT
                    ;; (ansible-unused/(python-mode python-ts-mode)) still
                    ;; hasn't run, deleting it!
                    ;; [jsonrpc] Server exited with status 9
                    (let ((inhibit-message t))
                      (eglot-shutdown server))))))
            (kill-buffer buf))

          (when (file-exists-p filename)
            (if (and bufferfile-use-vc
                     vc-managed-file)
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (&rest _args) t)))

                  ;; VC delete
                  (vc-delete-file filename))
              ;; Delete
              (delete-file filename)))

          (when bufferfile-verbose
            (bufferfile--message "Deleted: %s" (abbreviate-file-name filename)))

          (run-hook-with-args 'bufferfile-post-delete-functions
                              filename
                              list-buffers))))))

;;; Copy

(defun bufferfile-copy (&optional buffer)
  "Copy the current file of that BUFFER is visiting."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (let* ((filename (bufferfile--get-buffer-filename))
           (original-buffer (or (buffer-base-buffer) (current-buffer))))
      (with-current-buffer original-buffer
        (when (buffer-modified-p)
          (let ((save-silently (not bufferfile-verbose)))
            (save-buffer)))

        (let ((new-filename (bufferfile--read-dest-file-name filename)))
          (when bufferfile-verbose
            (bufferfile--message "Copy: %s -> %s"
                                 (abbreviate-file-name filename)
                                 (abbreviate-file-name new-filename)))
          (copy-file filename new-filename t))))))

;;; Provide
(provide 'bufferfile)
;;; bufferfile.el ends here
