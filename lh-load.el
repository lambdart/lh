;;; lex-load.el --- load and load-path related functions/commands -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Version: 0.0.4 Alpha
;; URL: https://github.com/lambdart/lex
;; Keywords: load-path load
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
;; Functions and commands to manipulate `load-path' interactively
;; and other procedures related to `load' features like `reload-library'.
;; The operations tries to follow the CRUD convention,
;; the macro `safe-load-file' can be used to verify if the
;; file exist before the load takes place.
;;
;; It's also possible to include a directory tree (subdirs) to `load-path'
;; using the `safe-add-subdirs-to-load-pat' and `safe-add-dirs-to-load-path',
;; by it's not recommended.
;;
;; See the `load-path' official documentation to proper configure
;; the directory including using subdirs.el in combination
;; to $EMACSLOADPATH environment variable.
;;
;; The functions/commands names follows the code convection:
;; ([prefix-]action-target-object).
;;
;;; Code:

(require 'files)
(require 'loadhist)

;;;###autoload
(defmacro safe-load-file (file)
  "Load FILE if exists."
  `(if (not (file-exists-p ,file))
       (message "File not found")
     (load (expand-file-name ,file) t nil nil)))

;;;###autoload
(defmacro safe-add-dirs-to-load-path (dirs)
  "Add DIRS (directories) to `load-path'."
  `(dolist (dir ,dirs)
     (setq dir (expand-file-name dir))
     (when (file-directory-p dir)
       (unless (member dir load-path)
         (push dir load-path)))))

(defun safe-add-subdirs-to-load-path (dir)
  "Add DIR and sub-directories to `load-path'."
  (let ((default-directory
          (expand-file-name dir user-emacs-directory)))
    (when (file-directory-p default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

;;;###autoload
(defun add-dir-to-load-path (dir)
  "Add arbitrary DIR (string) to `load-path'.
If call interactively asks for the directory using
the \\[minibuffer]."
  ;; interactively maps DIR argument.
  (interactive
   (list (expand-file-name
          (read-directory-name
           "Dir: " (concat user-emacs-directory "site-lisp/") nil 'confirm))))
  (let (
        ;; verify if the dir comes from the
        ;; interactive form which returns a (cons) list)
        (dir (if (equal (type-of dir) 'cons)
                 (car dir)
               dir)))
    ;; remove last / (if necessary)
    (setq dir (replace-regexp-in-string "\/$" "" dir))
    ;; switch/case
    (cond
     ;; verify if directory exists
     ((not (file-exists-p dir))
      (message "Directory not found"))
     ;; verify if it's already a member
     ((member dir load-path)
      (message "Already present"))
     ;; finally push (add) and logs
     (t
      (push dir load-path)
      (message "Dir %s added to load-path" dir)))))

;;;###autoload
(defun del-dir-from-load-path (dir)
  "Remove an DIR from `load-path'."
  ;; maps dir using load-path collection
  (interactive
   (list (completing-read "Dir: " load-path nil t "/")))
  ;; if is a member remove it
  (when (member dir load-path)
    (if (not (y-or-n-p (format "Remove %s" dir)))
        (message "See ya..")
      ;; delete dir (update) from load-path
      (setq load-path (delete dir load-path))
      ;; log message
      (message "Directory %s removed from load-path" dir))))

;;;###autoload
(defun clean-load-path ()
  "Delete duplicates from load-path."
  (interactive)
  (delete-dups load-path)
  (message "Load-path cleaned."))

;;;###autoload
(defun list-load-path ()
  "List `load-path' in buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Load-Path*"))
        (inhibit-read-only t))
    ;; verify buffer creation
    (if (not buffer) nil
      (with-current-buffer buffer
        ;; clean buffer
        (erase-buffer)
        ;; insert load path directories
        (dolist (path load-path)
          (insert (format "%s\n" path)))
        ;; set load path to read only
        (read-only-mode 1))
      ;; display the buffer
      (display-buffer buffer))))

;;;###autoload
(defun reload-library (library &optional force)
  "Reload target LIBRARY.
When optional argument FORCE is non-nil, unload feature
and its dependents."
  (interactive (list (read-library-name)
                     current-prefix-arg))
  ;; unload library
  (funcall 'unload-feature
           (read library)
           (or force nil))
  ;; load it
  (load-library library))

(provide 'lh-load)

;;; lh-load.el ends here
