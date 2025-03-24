;;; bufferfile.el --- Delete or rename buffer file names efficiently -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.1
;; URL: https://github.com/jamescherti/bufferfile.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3"))
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
;; - (bufferwizard-rename-file): Renames the file that the current buffer is
;;   visiting. This command renames the file name on disk, adjusts the buffer
;;   name, and updates any indirect buffers or other buffers associated with the
;;   old file.
;; - (bufferwizard-delete-file): Delete the file associated with a buffer and
;;   kill all buffers visiting the file, including indirect buffers.

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

(defcustom bufferfile-rename-replace-existing nil
  "If non-nil, allow overwriting an existing file when renaming a buffer file."
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
Each function receives two arguments: (path list-buffers).")

(defvar bufferfile-post-delete-functions nil
  "Hook run after deleting a file.
Each function receives two arguments: (path list-buffers).")

(defvar bufferfile-message-prefix "[bufferfile] "
  "Prefix used for messages and errors related to bufferfile operations.")

;;; Helper functions

(defun bufferfile--error (&rest args)
  "Signal an error with `bufferfile-message-prefix' followed by formatted ARGS.
ARGS are formatted as in `format'."
  (error "%s%s" bufferfile-message-prefix (apply #'format args)))

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

;;; Rename file

(defun bufferfile--rename-all-buffer-names (old-filename new-filename)
  "Update buffer names to reflect the renaming of a file.
OLD-FILENAME and NEW-FILENAME are absolute paths as returned by `file-truename'.

For all buffers associated with OLD-FILENAME, update the buffer names to use
NEW-FILENAME.

This includes indirect buffers whose names are derived from the old filename."
  (let ((basename (file-name-nondirectory old-filename))
        (new-basename (file-name-nondirectory new-filename)))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((base-buffer (buffer-base-buffer)))
            (when base-buffer
              (let* ((base-buffer-file-name
                      (let ((file-name (buffer-file-name base-buffer)))
                        (when file-name
                          (file-truename file-name)))))
                (when (and base-buffer-file-name
                           (string= base-buffer-file-name new-filename))
                  (let ((indirect-buffer-name (buffer-name)))
                    (let* ((new-buffer-name (concat new-basename
                                                    (substring
                                                     indirect-buffer-name
                                                     (length basename)))))
                      (when (string-prefix-p basename indirect-buffer-name)
                        (when new-buffer-name
                          (rename-buffer new-buffer-name))))))))))))))

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
      (bufferfile--error
       "Rename failed: Destination filename already exists: %s"
       new-filename))

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
             filename new-filename)))
      ;; Rename the file
      (rename-file filename new-filename ok-if-already-exists)
      (when bufferfile-verbose
        (bufferfile--message "Rename: %s -> %s"
                             filename
                             new-filename)))

    (set-visited-file-name new-filename t t)

    ;; Update all buffers pointing to the old filename Broken
    (bufferfile--rename-all-buffer-names filename new-filename)

    (dolist (buf list-buffers)
      (with-current-buffer buf
        ;; Eglot checkers fail when files are renamed because they
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

        (let* ((basename (file-name-nondirectory filename))
               (new-filename (read-file-name
                              (format "%sRename '%s' to: "
                                      bufferfile-message-prefix
                                      basename)
                              (file-name-directory filename)
                              nil
                              nil
                              nil
                              #'(lambda(filename)
                                  (file-regular-p filename)))))
          (unless new-filename
            (bufferfile--error "A new file name must be specified"))

          (bufferfile-rename-file filename
                                  new-filename
                                  bufferfile-rename-replace-existing))))))

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

          ;; Special cases
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
            (bufferfile--message "Deleted: %s" filename))

          (run-hook-with-args 'bufferfile-post-delete-functions
                              filename
                              list-buffers))))))

(provide 'bufferfile)
;;; bufferfile.el ends here
