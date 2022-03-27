;;; avy-riben.el --- Jump to Japanese text using avy -*- lexical-binding: t -*-

;; Copyright (C) 2022 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (riben "0.1") (avy "0.5"))
;; URL: https://github.com/akirak/emacs-dumb-japanese

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an alternative to migemo.

;;; Code:

(defgroup avy-riben nil
  "Jump to Japanese text using avy."
  :group 'avy
  :group 'riben)

(require 'riben-decode)
(require 'riben-google)
(require 'avy)

(defvar avy-timeout-seconds)

;;;###autoload
(defun avy-riben (input)
  "Jump to a visible portion of Japanese text using avy."
  (interactive (list (avy-riben--read-string)))
  (require 'avy)
  (avy-jump (avy-riben-re-builder input)))

(defun avy-riben-re-builder (input)
  "Return a pattern for Japanese according to INPUT in romaji."
  (thread-last
    (riben-decode-romaji input)
    (riben-google-from-hiragana)
    (mapcar (pcase-lambda (`(,original . ,alternatives))
              (apply #'list 'or original (car alternatives))))
    (apply #'list 'and)
    (rx-to-string)))

(defun avy-riben--read-string ()
  "Read a string."
  ;; `avy--read-candidates' would fire too many API requests for this use case,
  ;; so we had better implement an alternative.
  (let (str char)
    (while (setq char (read-char (format "Char (%s): " (or str ""))
                                 nil
                                 (and str avy-timeout-seconds)))
      (setq str (concat str (char-to-string char))))
    str))

(provide 'avy-riben)
;;; avy-riben.el ends here
