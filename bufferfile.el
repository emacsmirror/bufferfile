;;; bufferfile.el --- Rename/Delete/Copy Files and Associated Buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.5
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
;;   :ensure t)

;;; Code:

(require 'cl-lib)
(require 'hi-lock)

;;; Customizations

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

(defcustom bufferfile-delete-switch-to 'parent-directory
  "Specifies the action taken after deleting a file and killing its buffer.

Possible values are:
- \\='parent-directory
  Open the parent directory containing the deleted file.

- \\='previous-buffer
  Switch back to the previous buffer.

- nil
  Take no automatic action; allow Emacs to determine the next buffer after
  deleting the file."
  :type '(choice (const :tag "Open parent directory" parent-directory)
                 (const :tag "Switch to previous buffer" previous-buffer)
                 (const :tag "Do nothing" nil))
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

;; Internal
(defvar-local bufferfile--dired-file-selected nil)

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

(defun bufferfile--read-dest-file-name (filename prompt-prefix)
  "Prompt for a destination file name different from FILENAME.
PROMPT-PREFIX: The text prepended to the user input prompt."
  (let* ((basename (file-name-nondirectory filename))
         (new-filename (read-file-name
                        (format "%s%s'%s' to: "
                                bufferfile-message-prefix
                                prompt-prefix
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

(defun bufferfile--read-dest-file-name-rename (filename ok-if-already-exists)
  "Prompt for a destination file name different from FILENAME.
Signal an error if a filename NEWNAME already exists unless OK-IF-ALREADY-EXISTS
is non-nil."
  (let ((new-filename (bufferfile--read-dest-file-name filename "Rename ")))
    (when (and (not ok-if-already-exists)
               (file-exists-p new-filename))
      (unless (y-or-n-p
               (format
                "Destination file '%s' already exists. Do you want to overwrite it?"
                new-filename))
        (bufferfile--error
         "Rename failed: Destination filename already exists: %s"
         new-filename)))

    new-filename))

(defun bufferfile--vc-delete-file (file)
  "Delete file and mark it as such in the version control system.
If called interactively, read FILE, defaulting to the current
buffer's file name if it's under version control."
  (interactive (list (read-file-name "VC delete file: " nil
                                     (when (vc-backend buffer-file-name)
                                       buffer-file-name)
                                     t)))
  (setq file (expand-file-name file))
  (let ((buf (get-file-buffer file))
        (backend (vc-backend file)))
    (unless backend
      (error "File %s is not under version control"
             (file-name-nondirectory file)))
    (unless (vc-find-backend-function backend 'delete-file)
      (error "Deleting files under %s is not supported in VC" backend))
    (when (and buf (buffer-modified-p buf))
      (error "Please save or undo your changes before deleting %s" file))
    (let ((state (vc-state file)))
      (when (eq state 'edited)
        (error "Please commit or undo your changes before deleting %s" file))
      (when (eq state 'conflict)
        (error "Please resolve the conflicts before deleting %s" file)))
    (unless (or (file-directory-p file) (null make-backup-files)
                (not (file-exists-p file)))
      (with-current-buffer (or buf (find-file-noselect file))
	      (let ((backup-inhibited nil))
	        (backup-buffer))))
    ;; Bind `default-directory' so that the command that the backend
    ;; runs to remove the file is invoked in the correct context.
    (let ((default-directory (file-name-directory file)))
      (vc-call-backend backend 'delete-file file))
    ;; If the backend hasn't deleted the file itself, let's do it for him.
    (when (file-exists-p file) (delete-file file))
    ;; Forget what VC knew about the file.
    (vc-file-clearprops file)
    ;; Make sure the buffer is deleted and the *vc-dir* buffers are
    ;; updated after this.
    (when (fboundp 'vc-resynch-buffer)
      (funcall 'vc-resynch-buffer file nil t))))

(defun bufferfile--get-dired-buffers-visiting (directory)
  "Get all Dired buffers visiting DIRECTORY."
  (let ((result nil))
    (when directory
      (let ((dir (file-truename directory)))
        (dolist (buf (buffer-list))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (when (and (derived-mode-p 'dired-mode)
                         (string= (file-truename default-directory)
                                  dir))
                (push buf result)))))))
    result))

(defun bufferfile--refresh-dired-buffers (directory &optional goto-file)
  "Refresh all Dired buffers visiting DIRECTORY.
GOTO-FILE, if provided, moves the cursor to this file in each refreshed Dired
buffer."
  (when goto-file
    (setq goto-file (expand-file-name goto-file)))
  (when directory
    (dolist (buf (bufferfile--get-dired-buffers-visiting directory))
      (with-current-buffer buf
        (when (fboundp 'dired-revert)
          (when bufferfile-verbose
            (bufferfile--message "dired-revert: %s" default-directory))
          (ignore-errors
            (funcall 'dired-revert)))

        (when (and goto-file
                   (fboundp 'dired-goto-file)
                   bufferfile--dired-file-selected)
          (when bufferfile-verbose
            (bufferfile--message "dired-goto-file: %s" goto-file))
          (funcall 'dired-goto-file goto-file))))))

;;; Rename file

(defun bufferfile--rename-all-buffers (old-filename new-filename)
  "Update buffer names and files they are visiting to reflect the renaming.
OLD-FILENAME and NEW-FILENAME are absolute paths as returned by
`expand-file-name'.

For all buffers associated with OLD-FILENAME, update the buffer names to use
NEW-FILENAME.

This includes indirect buffers whose names are derived from the old filename."
  (setq old-filename (expand-file-name old-filename))
  (setq new-filename (expand-file-name new-filename))

  (let ((indirect-buffers '()))
    ;; Update the file name associated with buffers visiting files
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((base-buffer (buffer-base-buffer)))
            (if base-buffer
                ;; Indirect buffer (clone)
                (when (buffer-file-name base-buffer)
                  (push buf indirect-buffers))
              ;; File-visiting buffer
              (when (and buffer-file-name
                         (string= (file-truename buffer-file-name)
                                  (file-truename old-filename)))
                (set-visited-file-name new-filename t t)))))))

    ;; Update the names of indirect (clone) buffers associated with buffers
    ;; visiting the renamed files.
    (when indirect-buffers
      (let ((basename (file-name-nondirectory old-filename))
            (new-basename (file-name-nondirectory new-filename)))
        (dolist (buf indirect-buffers)
          (with-current-buffer buf
            (when-let* ((base-buffer (buffer-base-buffer)))
              (let ((base-buffer-filename
                     (let ((file-name (buffer-file-name base-buffer)))
                       (when file-name
                         (expand-file-name file-name))))
                    (indirect-buffer-name (buffer-name)))
                (when (and base-buffer-filename
                           (string= (file-truename base-buffer-filename)
                                    (file-truename new-filename)))
                  (when (string-prefix-p basename indirect-buffer-name)
                    (rename-buffer (concat new-basename
                                           (substring indirect-buffer-name
                                                      (length basename)))
                                   t)))))))))))

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
      (error "%sDestination file '%s' already exists"
             bufferfile-message-prefix new-filename))

    (setq list-buffers (bufferfile--get-list-buffers filename))

    (run-hook-with-args 'bufferfile-pre-rename-functions
                        filename
                        new-filename
                        list-buffers)

    (when bufferfile-use-vc
      (require 'vc))

    ;; Dired
    (let ((parent-dir-path (file-name-directory (expand-file-name filename)))
          (filename-truename (file-truename filename)))
      (dolist (buf (bufferfile--get-dired-buffers-visiting parent-dir-path))
        (with-current-buffer buf
          (when (fboundp 'dired-get-file-for-visit)
            (let ((file (funcall 'dired-get-file-for-visit)))
              (setq-local bufferfile--dired-file-selected
                          (and file (string= (file-truename file)
                                             filename-truename))))))))

    (if (and bufferfile-use-vc (vc-backend filename))
        (progn
          (when bufferfile-verbose
            (bufferfile--message
             "VC Rename: %s -> %s"
             (abbreviate-file-name filename)
             (abbreviate-file-name new-filename)))
          (when (and ok-if-already-exists (file-exists-p new-filename))
            ;; If the destination file exists, `vc-rename-file' cannot perform
            ;; the rename; the destination must be deleted first.
            (delete-file new-filename))
          ;; VC Rename
          (vc-rename-file filename new-filename))
      ;; Rename the file
      (rename-file filename new-filename ok-if-already-exists)
      (when bufferfile-verbose
        (bufferfile--message "Rename: %s -> %s"
                             (abbreviate-file-name filename)
                             (abbreviate-file-name new-filename))))

    ;; Update all buffers pointing to the old filename Broken
    (bufferfile--rename-all-buffers filename new-filename)

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

    ;; Refresh previous directory (special case: moving files)
    ;; TODO: check if the directory if filename is different from new-filename
    (let ((parent-dir-path (file-name-directory (expand-file-name filename))))
      (bufferfile--refresh-dired-buffers parent-dir-path))

    ;; Refresh the dired buffer
    (let ((parent-dir-path (file-name-directory (expand-file-name new-filename))))
      (bufferfile--refresh-dired-buffers parent-dir-path new-filename))

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
           (original-buffer (or (buffer-base-buffer) (current-buffer)))
           (ok-if-already-exists t))
      (with-current-buffer original-buffer
        (when (buffer-modified-p)
          (let ((save-silently (not bufferfile-verbose)))
            (save-buffer)))

        (let ((new-filename
               (bufferfile--read-dest-file-name-rename filename
                                                       ok-if-already-exists)))
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
        (let* ((vc-managed-file (when bufferfile-use-vc
                                  (vc-backend filename)))
               (list-buffers (bufferfile--get-list-buffers filename))
               (parent-dir-path (file-name-directory filename)))
          (unless parent-dir-path
            (error "Cannot find the parent directory of: %s" filename))
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
                  (bufferfile--error "'vc-revert-file' has not been declared")))))

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

          ;; Find file first
          (cond
           ((eq bufferfile-delete-switch-to 'parent-directory)
            (let ((parent-dir-buffer (find-file parent-dir-path)))
              (when (buffer-live-p parent-dir-buffer)
                (with-current-buffer parent-dir-buffer
                  (when (and (derived-mode-p 'dired-mode)
                             (fboundp 'dired-goto-file))
                    (dired-goto-file filename)))
                (switch-to-buffer parent-dir-buffer nil t))))

           ((eq bufferfile-delete-switch-to 'previous-buffer)
            (previous-buffer)))

          (when (file-exists-p filename)
            (if (and bufferfile-use-vc
                     vc-managed-file)
                ;; VC delete
                (bufferfile--vc-delete-file filename)
              ;; Delete
              (delete-file filename)))

          (when bufferfile-verbose
            (bufferfile--message "Deleted: %s" (abbreviate-file-name filename)))

          ;; Refresh dired buffers
          (bufferfile--refresh-dired-buffers parent-dir-path)

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

        (let ((new-filename (bufferfile--read-dest-file-name filename
                                                             "Copy ")))
          (when bufferfile-verbose
            (bufferfile--message "Copy: %s -> %s"
                                 (abbreviate-file-name filename)
                                 (abbreviate-file-name new-filename)))
          (copy-file filename new-filename t))))))

;;; Provide
(provide 'bufferfile)
;;; bufferfile.el ends here
