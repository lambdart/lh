;;; lh-buffer.el --- Buffer/Editing related functions -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Version: 0.0.4 Alpha
;; URL: https://github.com/lambdart/lex
;; Keywords: buffer editing text edit
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
;; This library provides edit capabilities that has the `buffer' text as target.
;; Just a simple.el extension using the right nomenclature and abstractions.
;;
;; A “buffer” is a Lisp object containing text to be edited. Buffers are
;; used to hold the contents of files that are being visited; there may
;; also be buffers that are not visiting files.
;;
;;; Code:

(require 'completion)

;;;###autoload
(defun switch-to-scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*scratch*"))) ; scratch buffer or create
    ;; write default message inside scratch buffer
    (with-current-buffer "*scratch*"
      ;; if buffer size is zero indicates new buffer
      (when (zerop (buffer-size))
        (insert (substitute-command-keys initial-scratch-message)))
      ;; change mode to the initial-major-mode of choice
      (if (eq major-mode 'fundamental-mode)
          (funcall initial-major-mode)))
    ;; finally switch to scratch buffer
    (switch-to-buffer buffer)))

(defun transpose-region (arg)
  "Transpose region by one line, ARG set the direction.
If ARG positive UP otherwise DOWN."
  (unless (region-active-p)
    (error "Reggion is not active"))
  ;; step 1: identifying the text to cut.
  (let ((beg (region-beginning))
        (end (region-end))
        (dir (if (> arg 0) 1 -1))) ;; direction down or up
    ;; step 2: cut and paste
    (let ((text (delete-and-extract-region beg end)))
      (forward-line dir) ;; direction
      (insert text)
      ;; step 3: restore mark if necessary
      (when (region-active-p)
        (setq deactivate-mark nil)
        (set-mark (+ (point) (- beg end)))))))

;;;###autoload
(defun indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-buffer))))

;;;###autoload
(defun duplicate-line-or-region (&optional n)
  "Duplicate current line or region N times.
If there's no region, the current line will be duplicated.
Otherwise, the selected region will be duplicated."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        (p (point))
        (i (or n 1)))
    (when (use-region-p)
      ;; update region positions
      (setq beg (region-beginning)
            end (region-end)))
    (let ((region (buffer-substring beg end))
          (deactivate-mark nil))
      (while (> i 0)
        (goto-char (line-end-position))
        (newline)
        (insert region)
        (goto-char p)
        (setq i (- i 1))))))

;;;###autoload
(defun quote-region ()
  "Quote the selected region."
  (interactive)
  (save-mark-and-excursion
    (when (region-active-p)
      (insert-pair 1 ?\" ?\"))))

(defun transpose-line (arg)
  "Transpose line ARG set the direction.
If ARG is positive UP else DOWN."
  ;; step 1: identifying the text to cut.
  (let ((orig (point))
        (beg (line-beginning-position))
        (end (1+ (line-end-position))))
    ;; step 2: cut and paste
    (let ((text (delete-and-extract-region beg end)))
      (forward-line arg)
      (insert text)
      ;; step 3: restore line position
      (forward-char (- orig end)))))

;;;###autoload
(defun transpose-lines-up ()
  "Transpose lines in down direction."
  (interactive)
  (if (region-active-p)
      (transpose-region 1)
    (transpose-line 1)))

;;;###autoload
(defun transpose-lines-down ()
  "Transpose lines in up direction."
  (interactive)
  (if (region-active-p)
      (transpose-region -1)
    (transpose-line -1)))

;;;###autoload
(defun transpose-word-left (n)
  "Transpose N words to the opposite direction (left)."
  (interactive "p")
  (transpose-words (- (or n 1))))

;;;###autoload
(defun copy-line (&optional arg)
  "Copy lines without kill then.
With prefix argument ARG, kill (copy) that many lines from point."
  (interactive "p")
  (let ((buffer-read-only t)
        (kill-read-only-ok t))
    (save-excursion
      (move-beginning-of-line nil)
      (kill-line arg))))

;;;###autoload
(defun copy-buffer-file-name (buffer-name)
  "Copy (`kill-new') BUFFER-NAME filename."
  (interactive "bBuffer: ")
  (let ((filename (buffer-file-name (get-buffer buffer-name))))
    (if (not filename)
        (message "Buffer %s has no file associated." buffer-name)
      (kill-new filename nil))))

;;;###autoload
(defun copy-buffer-name (buffer-name)
  "Copy (`kill-new') BUFFER-NAME."
  (interactive "bBuffer: ")
  (kill-new buffer-name nil))

;;;###autoload
(defun kill-region-or-backward-word ()
  "Kill region or `backward-kill-word'."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

;;;###autoload
(defun back-to-indent-or-line (arg)
  "Move point back to indentation or beginning of line.
With argument ARG not nil or 1, move forward ARG - 1 lines first."
  (interactive "p")
  (setq arg (or arg 1))
  ;; first forwards lines
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  ;; back to indentation or beginning if line
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;###autoload
(defun list-occurrences-at-point ()
  "Occur with symbol or region as its arguments."
  (interactive)
  (let*
      ;; get region or symbol
      ((bounds (if (use-region-p)
                   (cons (region-beginning) (region-end))
                 (bounds-of-thing-at-point 'symbol)))
       ;; get string if necessary
       (string (unless bounds (read-string "Occur: "))))
    (cond
     ;; region
     (bounds
      (occur (buffer-substring-no-properties (car bounds) (cdr bounds)))
      (deactivate-mark))
     ;; default string, symbol
     (t (occur string)))))

;;;###autoload
(defun complete-or-indent ()
  "Complete or indent."
  (interactive)
  (cond
   ;; if char isn't a space or tab: complete
   ((looking-at "\\_>")
    (complete nil))
   ;; default: indent
   (t (indent-according-to-mode))))

;;;###autoload
(defun complete-at-point-or-indent ()
  "This smart tab is a `minibuffer' compliant.
It acts as usual in the `minibuffer'.
Case mark is active, indents region.
Case point is at the end of a symbol, expands it.
Or indents the current line."
  (interactive)
  (cond
   ;; if minibuffer active: unless a valid completion:
   ((minibufferp)
    (unless (minibuffer-complete)
      (call-interactively 'completion-at-point)))
   ;; if mark is active: indent region
   (mark-active
    (indent-region (region-beginning) (region-end)))
   ;; if char isn't a space or tab: complete
   ((looking-at "\\_>")
    (call-interactively 'completion-at-point))
   ;; default: indent
   (t (indent-according-to-mode))))

(provide 'lh-buffer)

;;; lh-buffer.el ends here
