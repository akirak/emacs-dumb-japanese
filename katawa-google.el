;;; katawa-google.el --- Convert from hiragana to kanji using the Google CGI Service -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (dash "2.12") (request "0.3.0"))

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

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'request)
(require 'katawa-decode)

(defvar katawa-google-url "http://www.google.com/transliterate")

(defun katawa-google--request (hiragana)
  "Transliterate HIRAGANA using the service by Google."
  (let ((resp (request katawa-google-url
                       :params `(("langpair". "ja-Hira|ja")
                                 ("text" . ,hiragana))
                       :parser (lambda ()
                                 (let ((json-array-type 'list))
                                   (json-read)))
                       :sync t
                       :timeout 2)))
    (pcase (request-response-status-code resp)
      (200 (cl-loop for (src results) in (request-response-data resp)
                    collect (cons (decode-coding-string src 'utf-8)
                                  (--map (decode-coding-string it 'utf-8) results))))
      (code (error "Returned HTTP %d from %s" code katawa-google-url)))))

(cl-defun katawa-google--combine-candidates (l
                                             &optional
                                             (n-accepted 30)
                                             (n-limit 40)
                                             (n-each 4))
  "Combine segmented candidates into a list of concatenated candidates.

L is a list of list of candidates.

If the total number of combinations is not greater than N-ACCEPTED, no items
are not reduced. Otherwise, the first N-EACH items are taken from each segment.

N-LIMIT candidates are returned at maximum."
  (letrec ((f (lambda (candidates &rest rest)
                (if rest
                    (cl-loop for s in (apply f rest)
                             append (cl-loop for c in candidates
                                             collect (concat c s)))
                  candidates))))
    (if (and (> (reduce '* (mapcar 'seq-length l)) n-accepted)
             (> (seq-length l) 1))
        (cl-remove-duplicates (append (apply f (--map (-take 2 (cdr it)) l))
                                      (-take n-limit
                                             (apply f (--map (-take n-each it) l))))
                              :test #'string-equal
                              :from-end t)
      (apply f (--map (append (cdr it) (list (car it))) l)))))

;;;###autoload
(defun katawa-google-from-hiragana (hiragana)
  "Transliterate HIRAGANA and return a list of candidates."
  (katawa-google--combine-candidates
   (katawa-google--request hiragana)))

;;;###autoload
(defun katawa-google-from-romaji (romaji)
  "Transliterate ROMAJI and return a list of candidates."
  (katawa-google-from-hiragana (katawa-decode-romaji romaji)))

(provide 'katawa-google)
;;; katawa-google.el ends here
