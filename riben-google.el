;;; riben-google.el --- Google online API backend for katawa -*- lexical-binding: t -*-

;; Copyright (C) 2018,2022 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>

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

;; This library provides a Google backend for transliterating text into Japanese.

;;; Code:

(require 'url-http)

(defcustom riben-google-url "https://www.google.com/transliterate"
  "Endpoint url."
  :group 'riben
  :type 'string)

(defcustom riben-google-timeout 2
  ""
  :group 'riben
  :type 'number)

(cl-defun riben-google--request (input &key (langpair "ja-Hira|ja"))
  "Return transliteration of an input string."
  (let ((buf (url-retrieve-synchronously (format "%s?langpair=%s&text=%s"
                                                 riben-google-url langpair input)
                                         'silent nil riben-google-timeout)))
    (unwind-protect
        (with-current-buffer buf
          (when (bound-and-true-p url-http-end-of-headers)
            (delete-region (point-min) url-http-end-of-headers))
          (goto-char (point-min))
          (json-parse-buffer :array-type 'list :object-type 'alist))
      (kill-buffer buf))))

(defun riben-google-from-hiragana (input)
  (riben-google--request input))

(provide 'riben-google)
;;; riben-google.el ends here
