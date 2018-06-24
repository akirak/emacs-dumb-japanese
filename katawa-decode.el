;;; katawa-decode.el --- Decode from Japanese romaji to hiragana

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (ivy "0.10") (dash "2.12"))
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

;; This library is transliterates Roman notations of Japanese, e.g. romaji,
;; into a corresponding hiragana notations.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'mule-util)

(defconst katawa-decode--alpha-vowels (string-to-list "aiueo"))

(defun katawa-decode--make-row-table (elements)
  "Create a zipped alist indexed by vowels, i.e. \"aiueo\".

ELEMENTS is a string or a list of strings whose each element is a decoded string
in the gyou."
  (-zip katawa-decode--alpha-vowels
        (cond ((listp elements) elements)
              ((stringp elements) (mapcar 'char-to-string
                                          (string-to-list elements))))))

(defconst katawa-decode--row-table-a
  (katawa-decode--make-row-table "あいうえお"))

(defconst katawa-decode--char-table-1
  (-zip (string-to-list "kstnhmrgzdbpxywfvj")
        (mapcar 'katawa-decode--make-row-table
                '("かきくけこ"
                  "さしすせそ"
                  "たちつてと"
                  "なにぬねの"
                  "はひふへほ"
                  "まみむめも"
                  "らりるれろ"
                  "がぎぐげご"
                  "ざじずぜぞ"
                  "だぢづでど"
                  "ばびぶべぼ"
                  "ぱぴぷぺぽ"
                  "ぁぃぅぇぉ"
                  ("や" "い" "ゆ" "いぇ" "よ")
                  ("わ" "うぃ" "う" "うぇ" "を")
                  ("ふぁ" "ふぃ" "ふ" "ふぇ" "ふぉ")
                  ("う゛ぁ" "う゛ぃ" "う゛" "う゛ぇ" "う゛ぉ")
                  ("じゃ" "じ" "じゅ" "じぇ" "じょ")))))

(defconst katawa-decode--char-table-2
  `((?s . ((?h . ,(katawa-decode--make-row-table '("しゃ" "し" "しゅ" "しぇ" "しょ")))))
    (?c . ((?h . ,(katawa-decode--make-row-table '("ちゃ" "ち" "ちゅ" "ちぇ" "ちょ")))))
    (?t . ((?s . ,(katawa-decode--make-row-table '("つぁ" "ち" "つ" "つぇ" "つぉ")))))))

;;;###autoload
(defcustom katawa-decode-symbol-table
  '((?\. . "。")
    (?\, . "、"))
  "List of symbols to translate."
  :type '(repeat (cons integer string))
  :group 'katawa)

;;;###autoload
(defun katawa-decode-romaji (src)
  "Transliterate romaji, i.e. Roman representation of Japanese, into hiragana.

SRC is a string which contains romaji."
  (cl-flet* ((is-alpha (c) (and (>= c ?a) (<= c ?z)))
             (is-vowel (c) (memq c katawa-decode--alpha-vowels))
             (is-consonant (c) (and (is-alpha c) (not (is-vowel c))))
             (lookup-tree (alist &rest keys)
                          (progn (while keys
                                   (setq alist (alist-get (pop keys) alist)))
                                 alist))
             (go (cs)
                 (pcase cs
                   ;; End of the input sequence
                   ('() nil)
                   (`(?- . ,rest)
                    (cons "ー" rest))
                   ;; Non-alphabet characters are passed through
                   ((and `(,c . ,rest)
                         (guard (not (is-alpha c))))
                    (cons (alist-get c katawa-decode-symbol-table
                                     (char-to-string c))
                          rest))
                   ;; Consume "nn" as input
                   (`(?n ?n . ,rest)
                    (cons "ん" rest))
                   ;; Single vowel (a/i/u/e/o)
                   ((and `(,v . ,rest)
                         (let j (alist-get v katawa-decode--row-table-a))
                         (guard j))
                    (cons j rest))
                   ;; The same consonants in succession generates 撥音.
                   ((and `(,c ,c . ,rest)
                         (guard (is-consonant c)))
                    (cons "っ" (cons c rest)))
                   ;; consonant + vowel
                   ((and `(,c ,v . ,rest)
                         (let r (lookup-tree katawa-decode--char-table-1
                                             c v))
                         (guard r))
                    (cons r rest))
                   ;; ends with ya, yu, ye, or yo.
                   ((and `(,c ?y ,v . ,rest)
                         (guard (is-vowel v))
                         (let a (alist-get v '((?a . "ゃ")
                                               (?u . "ゅ")
                                               (?e . "ぇ")
                                               (?o . "ょ"))))
                         (guard a)
                         (let r (lookup-tree katawa-decode--char-table-1
                                             c ?i))
                         (guard r))
                    (cons (concat r a) rest))
                   ;; two consonants + vowel
                   ((and `(,c1 ,c2 ,v . ,rest)
                         (let r (lookup-tree katawa-decode--char-table-2
                                             c1 c2 v))
                         (guard r))
                    (cons r rest))
                   ;; Consume "n" as input
                   (`(?n . ,rest)
                    (cons "ん" rest))
                   ;; did not match: pass through
                   (`(,c . ,rest) (cons (char-to-string c)
                                        rest)))))
    (cl-loop for (out . rest) = (go (string-to-list src)) then (go rest)
             while out
             concat out)))

(provide 'katawa-decode)
;;; katawa-decode.el ends here
