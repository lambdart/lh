;;; lh-term.el --- term mode related functions/commands -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Version: 0.0.4 Alpha
;; URL: https://github.com/lambdart/lex
;; Keywords: term terminal emulator
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
;; This library provides little hacks for the terminal emulator.
;;
;;; Code:

(eval-when-compile
  (require 'cl-macs))

(require 'term)
(require 'subr-x)

(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "The key list that will be unbind."
  :type 'list
  :group 'term)

(defcustom term-bind-key-alist
  '(("C-c C-c" . term-interrupt-subjob)
    ("C-c C-e" . term-send-esc)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-return)
    ("C-y" . term-paste)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("<C-backspace>" . term-send-backward-kill-word)
    ("M-r" . term-send-reverse-search-history)
    ("M-d" . term-send-delete-word)
    ("M-," . term-send-raw)
    ("M-." . comint-dynamic-complete))
  "The key alist that will rebounded."
  :type 'alist
  :group 'term)

;;;###autoload
(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

;;;###autoload
(defun term-send-return ()
  "Use `term-send-raw-string' instead of `term-send-input'.
Because `term-send-input' duplicates input when
you type Ctrl-M in the in buffer terminal
emulator."
  (interactive)
  (term-send-raw-string "\C-m"))

;;;###autoload
(defun term-send-backward-kill-word ()
  "Backward kill word in term mode."
  (interactive)
  (term-send-raw-string "\C-w"))

;;;###autoload
(defun term-send-forward-kill-word ()
  "Kill word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

;;;###autoload
(defun term-send-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))

;;;###autoload
(defun term-send-forward-word ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))

;;;###autoload
(defun term-send-reverse-search-history ()
  "Search history reverse."
  (interactive)
  (term-send-raw-string "\C-r"))

;;;###autoload
(defun term-send-delete-word ()
  "Delete word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

;;;###autoload
(defun term-send-quote ()
  "Quote the next character in `term-mode'.
Similar to how `quoted-insert' works in a regular buffer."
  (interactive)
  (term-send-raw-string "\C-v"))

;;;###autoload
(defun term-send-M-x ()
  "Type META-X in `term-mode' side effect invoke the `minibuffer'."
  (interactive)
  (term-send-raw-string "\ex"))

;;;###autoload
(defun term-send-C-x  ()
  "Send CTRL-X."
  (interactive)
  (term-send-raw-string "\C-X"))

;;;###autoload
(defun term-setup-keystroke ()
  "Keystroke setup of `term-char-mode'.
By default, the key bindings of `term-char-mode' conflict
with user's keystroke. So this function unbinds some keys
with `term-raw-map', and binds some keystroke with `term-raw-map'."
  ;; unbind base key that conflict with user's keys-tokes.
  (dolist (key term-unbind-key-list)
    (define-key term-raw-map (read-kbd-macro key) nil))
  ;; bind `term-bind-key-alist' keys
  (dolist (element term-bind-key-alist)
    (define-key term-raw-map
                (read-kbd-macro (car element))
                (cdr element))))

;;;###autoload
(defun term-kill-buffer-hook ()
  "Function to be added in the `kill-buffer-hook' list."
  ;; get the current buffer associate process
  (let ((process (get-buffer-process (current-buffer))))
    (if (not (eq major-mode 'term-mode)) nil
      (and process (term-kill-subjob)))))

(defun term-uniq-name (name)
  "Return an unique NAME if the string name it's equal to \=term."
  (if (not (string-equal name "term")) name
    (let ((n 0)
          (regex (concat "^\*" name)))
      (dolist (buffer (buffer-list))
        (setq n (+ n (if (string-match-p regex (buffer-name buffer)) 1 0))))
      (if (= n 0) name (format "%s<%d>" name (+ n 1))))))

;;;###autoload
(defun term-make (name &optional shell)
  "Call `make-term' with the right arguments.
Asks for the NAME of the created terminal buffer interactively.
Get shell from the SHELL environment variable directly."
  (interactive
   (list
    (read-string "Term: ")
    (when current-prefix-arg
      (let ((shell (completing-read "Shell: "
                                    '("fish" "sh" "bash")
                                    nil t)))
        (if (string-empty-p shell) nil shell)))))
  (let* ((name (term-uniq-name (if (string-empty-p name) "term" name)))
         (buffer (make-term name (or shell (getenv "SHELL")))))
    ;; extra check
    (if (not (buffer-live-p buffer)) nil
      ;; change buffer mode
      (with-current-buffer buffer
        ;; verify if term-mode and term-char-mode are available
        (when (and (fboundp 'term-mode)
                   (fboundp 'term-char-mode))
          (funcall 'term-mode)
          (funcall 'term-char-mode))
        ;; switch to the term buffer
        (switch-to-buffer buffer)))))

(provide 'lh-term)

;;; lh-term.el ends here
