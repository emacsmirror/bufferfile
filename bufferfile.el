;;; bufferfile.el --- Helper functions to delete and rename buffer files. -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
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
;; The bufferfile Emacs package provides a collection of helper functions and
;; commands for managing buffers.
;;
;; The current version includes the following functions:
;; - `bufferfile-rename': Renames the file the current buffer is visiting. This
;;   command updates the file name on disk, adjusts the buffer name, and updates
;;   any indirect buffers or other buffers associated with the old file.
;; - `bufferfile-delete': Delete the file associated with a buffer and kill all
;;   buffers visiting the file, including indirect buffers or clones.

;;; Code:

(require 'cl-lib)
(require 'hi-lock)

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

(defvar bufferfile-before-rename-functions nil
  "List of functions to run before renaming a file.
Each function takes 3 argument: (list-buffers previous-path new-path).")

(defvar bufferfile-after-rename-functions nil
  "List of functions to run after renaming a file.
Each function takes 3 argument: (list-buffers previous-path new-path).")

(defvar bufferfile-before-delete-functions nil
  "List of functions to run before deleting a file.
Each function takes 2 argument: (list-buffers path).")

(defvar bufferfile-after-delete-functions nil
  "List of functions to run after deleting a file.
Each function takes 2 argument: (list-buffers path).")

;;; Helper functions

(defun bufferfile--message (&rest args)
  "Display a message with '[bufferfile]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[bufferfile] " (car args)) (cdr args)))

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

;;;###autoload
(defun bufferfile-rename (&optional buffer)
  "Rename the current file of that BUFFER is visiting.
This command updates:
- The file name on disk,
- the buffer name,
- all the indirect buffers or other buffers associated with the old file.

Hooks in `bufferfile-before-rename-functions' and
`bufferfile-after-rename-functions' are run before and after the renaming
process."
  (interactive)
  (when bufferfile-use-vc
    (require 'vc))
  (unless buffer
    (setq buffer (current-buffer)))
  (let* ((filename (let ((file-name (buffer-file-name (buffer-base-buffer))))
                     (when file-name
                       (file-truename file-name))))
         (original-buffer (when filename
                            (get-file-buffer filename))))
    (unless filename
      (error "The buffer '%s' is not associated with a file"
             (buffer-name)))

    (unless (file-regular-p filename)
      (error "The file '%s' does not exist on disk" filename))

    (unless original-buffer
      (error "Could not locate the buffer for '%s'"
             filename))

    (with-current-buffer original-buffer
      (when (buffer-modified-p)
        (let ((save-silently t))
          (save-buffer)))

      (let* ((basename (if filename
                           (file-name-nondirectory filename)
                         ""))
             (new-basename (read-string "New name: " basename))
             (list-buffers (bufferfile--get-list-buffers filename)))
        (unless (string= basename new-basename)
          (let ((new-filename (file-truename
                               (expand-file-name
                                new-basename (file-name-directory filename)))))
            (run-hook-with-args 'bufferfile-before-rename-functions
                                list-buffers filename new-filename)

            (if (and bufferfile-use-vc
                     (vc-backend filename))
                (progn
                  ;; Rename the file using VC
                  (vc-rename-file filename new-filename)
                  (when bufferfile-verbose
                    (bufferfile--message
                     "VC Rename: %s -> %s"
                     filename (file-name-nondirectory new-filename))))
              ;; Rename
              (rename-file filename new-filename 1)
              (when bufferfile-verbose
                (bufferfile--message "Rename: %s -> %s"
                                     filename
                                     (file-name-nondirectory new-filename))))

            (set-visited-file-name new-filename t t)

            ;; Update all buffers pointing to the old file Broken
            (bufferfile--rename-all-buffer-names filename new-filename)

            (dolist (buf list-buffers)
              (with-current-buffer buf
                ;; Eglot checkers fail when files are renamed because they
                (when (and (fboundp 'eglot-current-server)
                           (fboundp 'eglot-shutdown)
                           (fboundp 'eglot-managed-p)
                           (eglot-managed-p))
                  (let ((server (eglot-current-server)))
                    (when server
                      ;; Restart eglot
                      (let ((inhibit-message t))
                        (eglot-shutdown server))
                      (eglot-ensure))))))

            (run-hook-with-args 'bufferfile-after-rename-functions
                                list-buffers filename new-filename)))))))

;;; Delete file

;;;###autoload
(defun bufferfile-delete (&optional buffer)
  "Kill BUFFER and delete file associated with it.
Delete the file associated with a buffer and kill all buffers visiting the file,
including indirect buffers or clones.
If BUFFER is nil, operate on the current buffer.

Hooks in `bufferfile-before-delete-functions' and
`bufferfile-after-delete-functions' are run before and after the renaming
process."
  (interactive)
  (when bufferfile-use-vc
    (require 'vc))
  (let* ((buffer (or buffer (current-buffer)))
         (filename nil))
    (unless (buffer-live-p buffer)
      (error "The buffer '%s' is not alive" (buffer-name buffer)))

    (setq filename (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
    (unless filename
      (error "The buffer '%s' is not visiting a file" (buffer-name buffer)))
    (setq filename (file-truename filename))

    (when (yes-or-no-p (format "Delete file '%s'?"
                               (file-name-nondirectory filename)))
      (let ((vc-managed-file (when bufferfile-use-vc
                               (vc-backend filename)))
            (list-buffers (bufferfile--get-list-buffers filename)))
        (dolist (buf list-buffers)
          (with-current-buffer buf
            (when (buffer-modified-p)
              (let ((save-silently t))
                (save-buffer)))))

        (run-hook-with-args 'bufferfile-before-delete-functions
                            list-buffers filename)

        (when vc-managed-file
          ;; Revert version control changes before killing the buffer;
          ;; otherwise, `vc-delete-file' will fail to delete the file
          (when (not (vc-up-to-date-p filename))
            (with-current-buffer buffer
              (if (fboundp 'vc-revert-file)
                  (vc-revert-file filename)
                (error "vc-revert-file has been not declared")))))

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
                  ;; (ansible-unused/(python-mode python-ts-mode)) still hasn't
                  ;; run, deleting it!
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

        (run-hook-with-args 'bufferfile-after-delete-functions
                            list-buffers filename)))))

(provide 'bufferfile)
;;; bufferfile.el ends here
