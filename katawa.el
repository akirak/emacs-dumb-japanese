;;; katawa.el --- Yet another way to write Japanese -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (dash "2.12") (request "0.3.0"))
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

;; katawa.el is yet another way to writing Japanese in Emacs.  It is not a real
;; input method for Emacs, but it provides a simple API which lets you produce
;; Japanese text in Emacs.

;; katawa.el may become extensible in the future, but it currently supports
;; only one backend: a wrapper around [Google CGI API for Japanese Input][google_cgiapi].
;; It doesn't require an offline dictionary on your computer, but it requires
;; an internet connection.

;; katawa.el provides the following functions:
;;
;; - `katawa-insert' interactively reads a Roman representation of Japanese,
;;   transliterates it into a full representation including kanji, and inserts
;;   the output.
;; - `katawa-select' displays candidates via `completing-read' and returns
;;   the selection.
;; - `katawa-get-some-candidates' is a non-interactive function which returns
;;   a list of candidates transliterated from the input.
;;
;; This package also ships with an Ivy interface for writing Japanese as well as
;; a company backend.  The company backend is experimental.

;;; Code:

(require 'katawa-decode)
(require 'katawa-google)
(require 'japan-util)

;;;###autoload
(defcustom katawa-backend 'google
  "The backend to transliterate Japanese."
  :type '(choice (const :tag "Google CGI API" google))
  :group 'katawa)

(defcustom katawa-add-katakana-candidate t
  "Add a candidate with all of the input transliterated into Katakana."
  :type 'boolean
  :group 'katawa)

(defun katawa--candidates-from-hiragana (hiragana)
  "Transliterate HIRAGANA into a full notation of Japanese."
  (let ((result1 (cond
                  ((eq katawa-backend 'google) (katawa-google-from-hiragana hiragana))
                  (t (error "No backend is configured")))))
    (if katawa-add-katakana-candidate
        (append result1 (list (japanese-katakana hiragana)))
      result1)))

;;;###autoload
(defun katawa-get-some-candidates (src)
  "Transliterate Japanese text SRC using a configured backend.

SRC is normally romaji, i.e. a Roman representation of Japanese, but hiragana
and even a full notation of Japanese including kanji work at least with
the Google backend.

A list of some candidates is returned.

For a backend, see `katawa-backend' variable."
  (katawa--candidates-from-hiragana (katawa-decode-romaji src)))

;;;###autoload
(defun katawa-select (src)
  "Transliterate Japanese text SRC and select a candidate via `completing-read'.

The selected candidate is returned as a string."
  (let* ((hiragana (katawa-decode-romaji src)))
    (completing-read (format "Translate %s: " hiragana)
                     (katawa--candidates-from-hiragana hiragana))))

;;;###autoload
(defun katawa-insert (romaji)
  "Interactively transliterates ROMAJI and insert the output text."
  (interactive "sEnter Japanese romaji to transliterate: ")
  (insert (katawa-select romaji)))

(provide 'katawa)
;;; katawa.el ends here
