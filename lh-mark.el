;;; lh-mark.el --- mark related functions -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Version: 0.0.4 Alpha
;; URL: https://github.com/lambdart/lex
;; Keywords: mark mark-ring
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
;; Each buffer has a special marker, which is designated “the mark”.
;; When a buffer is newly created, this marker exists but does not point
;; anywhere; this means that the mark doesn’t exist in that buffer yet.
;;
;; Add more functions/commands to facility the usage of the marks in a
;; interactive way, i.e, provides candidates/completions and a quick
;; way to select then.
;;
;; TODO: add goto-mark-in-all-buffers
;;
;; The functions names follows the code convection:
;; ([prefix-]action-target-object).
;;
;;; Code:

(require 'simple)

(eval-when-compile
  (require 'cl-macs))

(defun parse-mark-line-to-string (pos)
  "Return line string at position POS."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((line (car (split-string (thing-at-point 'line) "[\n\r]"))))
      (remove-text-properties 0 (length line) '(read-only) line)
      (if (string= "" line)
          "<EMPTY LINE>"
        line))))

(defun mark-ring-collection ()
  "Return parsed mark ring candidates."
  (cl-loop with marks = (if (mark t)
                            (cons (mark-marker) mark-ring)
                          mark-ring)
           for mark in marks
           with max-line-number = (line-number-at-pos (point-max))
           with width = (length (number-to-string max-line-number))
           for m = (format (concat "%" (number-to-string width) "d: %s")
                           (line-number-at-pos mark)
                           (string-trim (parse-mark-line-to-string mark)))
           unless (and recip (assoc m recip))
           collect (cons m mark) into recip
           finally return recip))

(defun mark-ring-global-marks ()
  "Collection of buffer marks."
  (let ((marks '()))
    (dolist (buffer (buffer-list) (apply #'append marks))
      (with-current-buffer buffer
        (when (not (eq mark-ring nil))
          (setf marks (cons mark-ring marks)))))))

(defun read-mark (collection)
  "Read mark from COLLECTION."
  (if (not collection)
      (progn (message "Mark ring is empty") nil)
    (completing-read "Go to mark: " collection nil t)))

;;;###autoload
(defun find-local-mark ()
  "Find mark and jump to it."
  (interactive)
  (let* ((collection (mark-ring-collection))
         (mark-position (read-mark collection)))
    (cond ((or (not mark-position) (string-empty-p mark-position))
           (message "Mark not found"))
          (t (goto-char (cdr (assoc mark-position collection)))))))

;; TODO: global mark ring

(provide 'lh-mark)

;;; lh-mark.el ends here
