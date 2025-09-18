;;; lex-compile.el --- compile related functions/commands -*- lexical-binding: t -*-
;;
;; Maintainer: lambdart
;; Version: 0.0.4 Alpha
;; URL: https://github.com/lambdart/lex
;; Keywords: compile byte-compile collection make
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
;; This library provides interactive compile related functions and commands,
;; its use the compile-history to provide a collection
;; to be selected using the `minibuffer' completions system.
;;
;; Emacs Lisp has a “compiler” that translates functions written in Lisp
;; into a special representation called “byte-code” that can be executed
;; more efficiently. The compiler replaces Lisp function definitions with
;; byte-code. When a byte-code function is called, its definition is
;; evaluated by the “byte-code interpreter”.
;;
;;; Code:

(require 'compile)
(require 'subr-x)
(require 'cl-seq)

;;;###autoload
(defun add-compile-command (command)
  "Add compile COMMAND to `compile-history' list."
  ;; maps COMMAND argument interactively
  (interactive
   (list
    (read-string "Compile command: "
                 (format "make -C %s all" default-directory))))
  ;; push the command with is not already a list member
  (unless (member command compile-history)
    (push command compile-history)
    ;; clean the list (avoid duplicates)
    (delete-dups compile-history)))

;;;###autoload
(defun compile-command-history (command)
  "Run compile COMMAND using `compile-history' as candidates source."
  ;; maps COMMAND argument interactively
  (interactive
   (list
    (completing-read "Command: "
                     compile-history
                     nil
                     'confirm
                     ""
                     `(compile-history))))
  ;; compile using the right command
  (or (string-empty-p command)
      (compile command)))

;;;###autoload
(defun byte-compile-current-file ()
  "Save and byte compile the current file."
  (interactive)
  ;; when the buffer is associate to a file
  (when buffer-file-name
    (cond
     ;; indicates if it's possible to byte-compile
     ((eq major-mode 'emacs-lisp-mode)
      ;; save the buffer before compile
      (save-buffer)
      (byte-compile-file buffer-file-name))
     ;; default
     (t (message "Was not possible to compile the file: %s"
                 buffer-file-name)))))

;;;###autoload
(defun byte-compile-library (dir)
  "Byte compile a library, 'el' files from an arbitrary DIR."
  ;; called interactively
  (interactive (list
                (expand-file-name
                 (read-directory-name
                  "Dir: "
                  (concat user-emacs-directory "site-lisp/") nil 'confirm))))
  ;; get files from directory
  (let ((files (directory-files dir t)))
    (dolist (file files)
      (when (equal (file-name-extension file) "el")
        (byte-compile-file file)))))

;;;###autoload
(defun byte-compile-libraries (dir)
  "Recompile all libraries under the root DIR."
  (interactive
   (list (expand-file-name
          (read-directory-name "Dir: "
                               (concat user-emacs-directory "site-lisp") nil 't))))
  ;; get libraries directories
  (let ((libraries
         (cl-remove-if-not #'file-directory-p
                           (directory-files dir t "^[^.]"))))
    ;; for each library compile
    (dolist (dir libraries)
      (byte-compile-library dir))))

(provide 'lh-compile)

;;; lh-compile.el ends here
