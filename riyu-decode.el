;;; riyu-decode.el --- Decode from Japanese romaji to hiragana

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

;; This library is transliterates Roman notations of Japanese, e.g. romaji,
;; into a corresponding hiragana notations.

;;; Code:

(require 'map)
(require 'cl-lib)
(require 'mule-util)

(defconst riyu-decode--alpha-vowels
  (string-to-list "aiueo"))

(defmacro riyu-decode--zip (keys values &optional nosort)
  `(let ((keys (copy-sequence ,keys))
         (values (copy-sequence ,values))
         items)
     (while (and keys values)
       (push (cons (pop keys) (pop values))
             items))
     (if ,nosort
         items
       (nreverse items))))

(defun riyu-decode--row-alist (elements &optional prepend)
  "Return a hash table indexed by vowels, i.e. \"aiueo\".

ELEMENTS is a string or a list of strings whose each element is a decoded string
in the gyou."
  (let ((keys (cl-etypecase prepend
                (null (string-to-list riyu-decode--alpha-vowels))
                (list (mapcar `(lambda (c)
                                 (append ',prepend (list c)))
                              (string-to-list riyu-decode--alpha-vowels))))))
    (riyu-decode--zip keys elements)))

(defun riyu-decode--singletons (string)
  (mapcar #'char-to-string (string-to-list string)))

(defconst riyu-decode--row-table-a
  (riyu-decode--row-alist
   (riyu-decode--singletons "あいうえお")))

(defconst riyu-decode--char-table-1
  (let* ((values (append (mapcar #'riyu-decode--singletons
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
                                   "ぁぃぅぇぉ"))
                         '(("や" "い" "ゆ" "いぇ" "よ")
                           ("わ" "うぃ" "う" "うぇ" "を")
                           ("ふぁ" "ふぃ" "ふ" "ふぇ" "ふぉ")
                           ("う゛ぁ" "う゛ぃ" "う゛" "う゛ぇ" "う゛ぉ")
                           ("じゃ" "じ" "じゅ" "じぇ" "じょ"))))
         (pairs (riyu-decode--zip (string-to-list "kstnhmrgzdbpxywfvj")
                                    values))
         (tbl (make-hash-table :size (* 5 (length pairs))
                               :test #'equal)))
    (pcase-dolist (`(,consonant . ,entries) pairs)
      (pcase-dolist (`(,key . ,value)
                     (riyu-decode--row-alist entries (list consonant)))
        (puthash key value tbl)))
    tbl))

(defconst riyu-decode--char-table-2
  (let* ((pairs `(((?s ?h) . ("しゃ" "し" "しゅ" "しぇ" "しょ"))
                  ((?c ?h) . ("ちゃ" "ち" "ちゅ" "ちぇ" "ちょ"))
                  ((?t ?s) . ("つぁ" "ち" "つ" "つぇ" "つぉ"))))
         (tbl (make-hash-table :size (* 5 (length pairs))
                               :test #'equal)))
    (pcase-dolist (`(,consonant . ,entries) pairs)
      (pcase-dolist (`(,key . ,value)
                     (riyu-decode--row-alist entries consonant))
        (puthash key value tbl)))
    tbl))

;;;###autoload
(defcustom riyu-decode-punct-table
  '((?\. . "。")
    (?\, . "、"))
  "List of punctuations to transliterate."
  :type '(repeat (cons integer string))
  :group 'katawa)

;;;###autoload
(defun riyu-decode-romaji (input)
  "Return a hiragana transliteration of romaji."
  (let ((chars (string-to-list input)))
    (cl-flet*
        ((is-alpha (c) (and (>= c ?a) (<= c ?z)))
         (is-vowel (c) (memq c riyu-decode--alpha-vowels))
         (is-consonant (c) (and (is-alpha c) (not (is-vowel c)))))
      (with-temp-buffer
        (while chars
          (pcase chars
            ;; End of the input sequence
            ('() nil)
            (`(?- . ,rest)
             (insert "ー")
             (setq chars rest))
            ;; Non-alphabet characters are passed through
            ((and `(,c . ,rest)
                  (guard (not (is-alpha c))))
             (insert (alist-get c riyu-decode-punct-table
                                (char-to-string c)))
             (setq chars rest))
            ;; Consume "nn" as input
            (`(?n ?n . ,rest)
             (insert "ん")
             (setq chars rest))
            ;; Single vowel (a/i/u/e/o)
            ((and `(,v . ,rest)
                  (let j (map-elt riyu-decode--row-table-a v))
                  (guard j))
             (insert j)
             (setq chars rest))
            ;; The same consonants in succession generates 撥音.
            ((and `(,c ,c . ,rest)
                  (guard (is-consonant c)))
             (insert "っ")
             (setq chars (cons c rest)))
            ;; consonant + vowel
            ((and `(,c ,v . ,rest)
                  (let r (map-elt riyu-decode--char-table-1 (list c v)))
                  (guard r))
             (insert r)
             (setq chars rest))
            ;; ends with ya, yu, ye, or yo.
            ((and `(,c ?y ,v . ,rest)
                  (guard (is-vowel v))
                  (let a (alist-get v '((?a . "ゃ")
                                        (?u . "ゅ")
                                        (?e . "ぇ")
                                        (?o . "ょ"))))
                  (guard a)
                  (let r (map-elt riyu-decode--char-table-1 (list c ?i)))
                  (guard r))
             (insert (concat r a))
             (setq chars rest))
            ;; two consonants + vowel
            ((and `(,c1 ,c2 ,v . ,rest)
                  (let r (map-elt riyu-decode--char-table-2 (list c1 c2 v)))
                  (guard r))
             (insert r)
             (setq chars rest))
            (`(,c . ,rest)
             (insert (char-to-string c))
             (setq chars rest))))
        (buffer-string)))))

(provide 'riyu-decode)
;;; riyu-decode.el ends here
