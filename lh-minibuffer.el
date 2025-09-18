;;; lh-minibuffer.el --- minibuffer related extentions -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Version: 0.0.4 Alpha
;; URL: https://github.com/lambdart/lex
;; Keywords: minibuffer interactive command
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
;; This library provides more commands to handle generic actions to
;; the `minibuffer' (with is a terrible name for a command prompt),
;; Some commands are related to `minibuffer' window selection, others
;; with copy/kill/insert/describe the top candidate.
;;
;; A “minibuffer” is a special buffer that Emacs commands use to read
;; arguments more complicated than the single numeric prefix argument.
;; These arguments include file names, buffer names, and command names (as
;; in ‘M-x’). The minibuffer is displayed on the bottom line of the frame,
;; in the same place as the echo area (*note The Echo Area::), but only
;; while it is in use for reading an argument.
;;
;;; Code:

(require 'delsel)
(require 'minibuffer)
(require 'help-fns)
(require 'icomplete)

(defmacro minibuffer-action (&rest body)
  "Check `minibuffer' if active and evaluate BODY forms (action)."
  `(unless (not (minibufferp))
     ;; set completion (clean it from any properties)
     (let ((completion (substring-no-properties
                        (car completion-all-sorted-completions))))
       ;; when completion evaluate the rest of the body (the action code)
       ;; otherwise do nothing
       (when completion ,@body))))

;;;###autoload
(defun minibuffer-kill-current-completion ()
  "Put `minibuffer' completion at the top of the `kill-ring'."
  (interactive)
  (minibuffer-action (kill-new `,completion)))

;;;###autoload
(defun minibuffer-insert-completion-at-point ()
  "Insert `minibuffer' top completion at point."
  (interactive)
  (minibuffer-action
   (let ((beg (if (window-minibuffer-p)
                  (minibuffer-prompt-end)
                (nth 0 completion-in-region--data)))
         (end (if (window-minibuffer-p)
                  (point-max)
                (nth 1 completion-in-region--data))))
     ;; delete region at point
     (delete-region beg end)
     ;; insert completion
     (insert `,completion))))

;;;###autoload
(defun minibuffer-insert-completion-in-buffer ()
  "Insert `minibuffer' completion in current active buffer."
  (interactive)
  (minibuffer-action
   (unless buffer-read-only
     (with-minibuffer-selected-window
       (insert `,completion)
       (minibuffer-keyboard-quit)))))

;;;###autoload
(defun minibuffer-describe-current-completion ()
  "Describe symbol using top-most `minibuffer' completion candidate."
  (interactive)
  (minibuffer-action
   (save-excursion
     (let ((symbol (read `,completion)))
       (when (symbolp symbol)
         (describe-symbol symbol)
         (message nil))))))

;;;###autoload
(defun select-minibuffer-window ()
  "Go to the active minibuffer, if available."
  (interactive)
  (let ((window (active-minibuffer-window)))
    (and window (select-window window nil))))

;;;###autoload
(defun select-completions-window ()
  "Go to the active completions window, if available."
  (interactive)
  (let ((window (get-buffer-window "*Completions*")))
    (and window (select-window window nil))))

;;;###autoload
(defun goto-minibuffer-or-call-it ()
  "Go to minibuffer window or call `execute-extended-command'."
  (interactive)
  (let ((window (active-minibuffer-window)))
    (if (not window)
        (call-interactively 'execute-extended-command)
      (select-window window nil))))

(provide 'lh-minibuffer)

;;; lh-minibuffer.el ends here
