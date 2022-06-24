;;; riben-google.el --- Google online API backend -*- lexical-binding: t -*-

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

(defcustom riben-google-translate-url
  "https://translate.google.com/translate_a/single"
  ""
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

(cl-defun riben-google--translate (input &key
                                         (source-language "en")
                                         (target-language "ja"))
  ;; Please install go-translate.
  (require 'gts-engine-google)
  (let ((buffer (url-retrieve-synchronously
                 (format "%s?%s"
                         riben-google-translate-url
                         (string-join `("client=t"
                                        "ie=UTF-8"
                                        "oe=UTF-8"
                                        ,(concat "sl=" source-language)
                                        ,(concat "tl=" target-language)
                                        ,(concat "q=" (url-hexify-string input))
                                        "dt=bd"
                                        "pc=1"
                                        "otf=1"
                                        "srcrom=1"
                                        "ssel=0"
                                        "tsel=0"
                                        ,(concat "tk="
                                                 (gts-google-tkk '(430675 . 2721866130) input)))
                                      "&"))
                 'silent nil)))
    (unwind-protect
        (with-current-buffer buffer
          (set-buffer-multibyte t)
          (goto-char (point-min))
          (when (bound-and-true-p url-http-end-of-headers)
            (delete-region (point) url-http-end-of-headers))
          (json-parse-buffer :array-type 'list
                             :null-object nil))
      (kill-buffer buffer))))

(defun riben-google-translate-part-of-speech (type result)
  "Select candidates that match a part of speech.

This function returns candidates that match a part of speech,
denoted by TYPE. RESULT should be a response tree returned by
`riben-google-translate-retrieve'."
  (thread-last
    result
    (nth 1)
    (assoc type)
    (cdr)
    (car)))

(provide 'riben-google)
;;; riben-google.el ends here
