;;; lh-uu.el --- invoke unix utilities -*- lexical-binding: t -*-
;;
;; Author: lambdart <<lambdart@protonmail.com>>
;; Maintainer: lambdart
;; Version: 0.0.4 Alpha
;; URL: https://github.com/lambdart/lex
;; Keywords: unix utilities processes process
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
;; This library add new functions to invoke unix utilities
;; from the Virtual Lisp Machine interpreter space (Emacs).
;;
;;; Code:

(require 'files)

(eval-when-compile
  (require 'cl-macs nil t))

(defgroup lh-uu nil
  "Unix utilities."
  :group 'extensions
  :group 'convenience)

(defcustom lh-opacity .7
  "Opacity default value.
The opacity value is a number/float from 0 to 1,
with zero being fully transparent and one (1 - 100%)
being fully opaque."
  :type 'float
  :group 'lh-uu
  :safe t)

(defcustom lh-window-opacity .7
  "X window opacity default value."
  :type 'float
  :group 'lh-uu
  :safe t)

(defcustom lh-transset "transset"
  "Transset command."
  :type 'string
  :group 'lh-uu
  :safe t)

(defcustom lh-transset-args "-a"
  "Default arguments/switches for the `lh-transset' program."
  :type 'string
  :group 'lh-uu
  :safe t)

(defcustom lh-slock "slock"
  "X screen locker program."
  :type 'string
  :group 'lh-uu
  :safe t)

(defcustom lh-scrot "scrot"
  "Command line screen capture utility."
  :type 'string
  :group 'lh-uu
  :safe t)

(defcustom lh-scrot-dir
  (expand-file-name "scrot" user-emacs-directory)
  "The folder where captured screen images will be saved."
  :type 'string
  :group 'lh-uu
  :safe t)

(defcustom lh-cmd-fmt "%s vol %s%d"
  "Command format."
  :type 'string
  :group 'lh-uu
  :safe t)

(defcustom lh-mixer "mixer"
  "Mixer program – set/display soundcard mixer values."
  :type 'string
  :group 'lh-uu
  :safe t)

(defcustom lh-mixer-factor 0.05
  "Volume up/down factor."
  :type 'float
  :group 'lh-uu
  :safe t)

;;;###autoload
(defun set-transparency (opacity &optional args)
  "Set OPACITY transparency passing ARGS to `lh-transset' program."
  ;; maps function arguments when called interactively
  (interactive
   (list
    ;; set opacity
    (read-number "Opacity: " lh-opacity)
    ;; verify universal argument (implicit)
    (when current-prefix-arg
      (read-string "Args: " lh-transset-args))))
  ;; verify if 'transset' executable is available
  (if (not (executable-find lh-transset))
      (message "Program %s not found" lh-transset)
    ;; invoke the command 'transset' asynchronous
    (async-shell-command
     (format "%s %s %.1f" lh-transset (or args lh-transset-args) opacity))))

;;;###autoload
(defun set-frame-transparency ()
  "Set transparency in current frame."
  (interactive)
  (call-interactively 'set-transparency))

;;;###autoload
(defun set-window-transparency (opacity)
  "Set OPACITY transparency in selected X window (including EMACS)."
  ;; map opacity argument when invoked interactively
  (interactive
   (list (read-number "Opacity: " lh-window-opacity)))
  ;; set transparency using 'transset' -c argument
  (set-transparency opacity "-c"))

;;;###autoload
(defun capture-screen (&optional dest)
  "Capture screen (an image) and save at DEST directory."
  (interactive
   (list
    ;; if prefix, read directory name
    (when current-prefix-arg
      (read-directory-name "Dir: " nil default-directory t))))
  ;; body:
  (let ((default-directory (expand-file-name
                            (or dest lh-scrot-dir)))
        (scrot (executable-find lh-scrot)))
    (cond
     ;; if screen capture utility
     (scrot
      (funcall 'start-process scrot nil scrot)
      (message nil)
      (message "Image saved at %s" default-directory))
     ;; default
     (t
      (message "Command %s not found" lh-scrot)))))

;;;###autoload
(defun lock-screen ()
  "Lock screen using `lh-slock' utility."
  (interactive)
  (if (not (executable-find lh-slock))
      (message "Command %s not found" lh-slock)
    (async-shell-command lh-slock)))

(defvar mixer-command "mixer"
  "Mixer command.")

(defvar mixer-device "vol"
  "Mixer device name.")

(defun mixer-command (&optional arg)
  "Return a formatted mixer command with the proper ARG."
  (apply #'format `("%s %s%s"
                    ,mixer-command
                    ,mixer-device
                    ,(or arg ""))))

;;;###autoload
(defun get-volume ()
  "Show volume."
  (cadr (split-string
         (shell-command-to-string
          (mixer-command))
         "[ \=\:\f\t\n\r\v]+")))

;;;###autoload
(defun show-volume ()
  "Show volume."
  (interactive)
  (message "Volume: %s" (get-volume)))

;;;###autoload
(defun set-volume (volume &optional direction)
  "Set VOLUME."
  (interactive (list (read-number "Volume: " .5)))
  (let ((up-or-down (if direction direction "")))
    (mapc #'funcall
          `((lambda ()
              (shell-command-to-string
               (mixer-command (format "=%s%s" ,up-or-down ,volume))))
            (lambda ()
              (show-volume))))))

(defun up-or-down (&optional direction)
  "Return the right DIRECTION argument format."
  (cond
   ((eq direction 'up)   "+")
   ((eq direction 'down) "-")
   (t "")))

;;;###autoload
(defun raise-volume (&optional n)
  "Increase volume by a factor of 5.
If \\[universal-argument] is used, display a prompt
asking for the volume value - N."
  (interactive (list (and current-prefix-arg
                          (read-number "Factor: " 0.05))))
  (set-volume (abs (or n lh-mixer-factor))
              (up-or-down 'up)))

;;;###autoload
(defun lower-volume (&optional n)
  "Lower volume by a factor of 5.
If \\[universal-argument] is used, display a prompt
asking for the volume value - N."
  (interactive (list (when current-prefix-arg
                       (read-number "Factor: " 0.05))))
  ;; parse factor and set the volume
  (set-volume (abs (or n lh-mixer-factor))
              (up-or-down 'down)))

;;;###autoload
(defun mute-audio ()
  "Mute volume."
  (interactive)
  (set-volume 0))

;;;###autoload
(defun export-pdf-to-text (pdf txt)
  "Convert a PDF to TXT file.
When \\[universal-argument] is used, asks for the
text file output name."
  ;; map function arguments
  (interactive
   (list (read-file-name "File: " nil nil t)
         (when current-prefix-arg
           (read-file-name "Fout:" nil nil))))
  (async-shell-command
   (concat (format "pdftotext %S" (expand-file-name pdf)) (or txt nil))))

(provide 'lh-uu)

;;; lh-uu.el ends here
