;;; katawa-ivy-exwm.el --- katawa-ivy with EXWM support -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (exwm "0.18"))
;; URL: https://github.com/akirak/katawa.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides `katawa-ivy-exwm' command, which is like `katawa-ivy'
;; but paste the result via the clipboard if the current buffer is an EXWM
;; window.

;; This feature is inspired by and based on an implementation of
;; exwmx-sendstring.el:
;; https://github.com/tumashu/exwm-x/blob/master/exwmx-sendstring.el

;;; Code:

(require 'katawa-ivy)
(require 'exwm-input)

(defcustom katawa-ivy-exwm-class-paste-keys-alist
  nil
  "Alist of window class and paste keys."
  :type '(alist :key-type string
                :value-type (vector character))
  :group 'katawa-ivy-exwm)

(defcustom katawa-ivy-exwm-default-paste-keys
  [?\C-v]
  "Default paste key sequence for X applications in EXWM."
  :type '(vector character)
  :group 'katawa-ivy-exwm)

(defun katawa-ivy-exwm--get-paste-keys ()
  "Get a paste key sequence for the current EXWM buffer."
  (or (cdr (assoc exwm-class-name katawa-ivy-exwm-class-paste-keys-alist))
      katawa-ivy-exwm-default-paste-keys))

;;;###autoload
(defun katawa-ivy-exwm ()
  "Insert Japanese text into the current buffer or X window."
  (interactive)
  (let ((buffer (current-buffer)))
    (ivy-read "katawa: "
              (lambda (string)
                (unless (string-empty-p string)
                  (katawa-get-some-candidates string)))
              :caller 'katawa-ivy-exwm
              :history 'katawa-ivy-history
              :action (lambda (input)
                        (if (derived-mode-p 'exwm-mode)
                            (let ((inhibit-read-only t))
                              (kill-new input)
                              ;; https://github.com/ch11ng/exwm/issues/413#issuecomment-386858496
                              (exwm-input--set-focus (exwm--buffer->id buffer))
                              (mapc #'exwm-input--fake-key (katawa-ivy-exwm--get-paste-keys))
                              (pop kill-ring))
                          (insert input)))
              :matcher (lambda (_ cands) cands)
              :dynamic-collection t)))

(provide 'katawa-ivy-exwm)
;;; katawa-ivy-exwm.el ends here
