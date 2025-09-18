;;; lh-files.el --- files related functions/commands -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Version: 0.0.4 Alpha
;; URL: https://github.com/lambdart/lex
;; Keywords: files directory directories
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; This library provides more functions/commands to the `files' space.
;; Many of the file functions take one or more arguments that are file
;; names. A file name is a string. Most of these functions expand file
;; name arguments using the function ‘expand-file-name’, so that ‘~’ is
;; handled correctly, as are relative file names (including ‘../’ and the
;; empty string).
;;
;;; Code:

(require 'files)
(require 'recentf)
(require 'loadhist)

;;;###autoload
(defun file-type (file-name)
  "Show FILE-NAME type information in echo area."
  ;; read the file name (using the minibuffer)
  (interactive "fFile: ")
  ;; set output buffer
  ;; set file name full path
  ;; set file executable
  (let* ((buffer (get-buffer-create "*File Type*"))
         (file-name (expand-file-name file-name))
         (executable "file")
         (command nil))
    ;; verify if file executable was found
    (when (executable-find executable)
      ;; format the final command
      (setq command (format "%s \"%s\"" executable file-name))
      ;; execute it
      (shell-command command (or buffer nil)))))

(defun recent-file-collection ()
  "Return recent files (candidates) collection."
  ;; cleanup recent files
  (recentf-cleanup)
  ;; return abbreviate recent files list
  (mapcar 'abbreviate-file-name recentf-list))

;;;###autoload
(defun find-recent-file (file)
  "Find recent FILE."
  ;; read file
  (interactive
   (list (completing-read "Find recent file: "
                          (recent-file-collection) nil t)))
  ;; open recent file
  (find-file file))

;;;###autoload
(defun set-default-directory (cwd)
  "Set new current working directory (CWD)."
  (interactive
   (list (expand-file-name (read-file-name "cwd: "))))
  ;; set local default directory (cwd)
  (setq-local default-directory cwd))

;;;###autoload
(defun execute-file (executable &optional args)
  "Execute arbitrary EXECUTABLE file using `start-process'.

If \\[universal-argument] is used, display a secondary
prompt asking for additional ARGS - arguments."

  (interactive
   (list
    ;; get executable file
    (read-file-name "File: " nil nil t)
    ;; get arguments, if prefix - \\[universal-argument] - was used
    (if current-prefix-arg
        (read-string "Args: "))))
  (let*
      ;; set only the file name (remove full path)
      ((name (file-name-nondirectory executable))
       ;; set default directory
       (default-directory (file-name-directory executable))
       ;; use a pipe, or t to use a pty
       (process-connection-type t))
    (cond
     ;; test if file is a directory
     ((file-directory-p executable)
      (message "Directories are not executable files"))
     ;; test if its not possible to access the directory
     ((not (file-accessible-directory-p default-directory))
      (message "Directory not accessible"))
     ;; test if the file is a executable
     ((file-executable-p executable)
      (apply 'start-process name nil executable args))
     ;; default, not a executable file
     (t (message "File %s is not executable" name)))))

;;;###autoload
(defun delete-file-at-point ()
  "Delete file at point (filename or region)."
  (interactive)
  (let ;; get file at point
      ((file (thing-at-point 'filename t)))
    ;; switch/case equivalent
    (cond
     ;; case no file found: nothing to do,
     ;; display a message and leave
     ((eq file nil)
      (message "No file at point was found"))
     ;; case: file is a directory
     ((file-directory-p file)
      ;; deleting directories can be dangerous
      ;; don't it at point
      (message "File is a directory, use `delete-directory'"))
     ;; case: file exist: delete it (asks for confirmation)
     ((file-exists-p file)
      (if (not (y-or-n-p (format "Delete file: %s ? " file)))
          ;; no, just clean echo area
          (message "")
        ;; else, (yes): delete file and display message
        (delete-file file)))
     ;; default: call delete-file interactively
     (t (message "File does not exists")))))


(provide 'lh-files)

;;; lh-files.el ends here
