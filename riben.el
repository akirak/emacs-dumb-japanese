;;; riben.el --- A dumb Japanese input method -*- lexical-binding: t -*-

;; Copyright (C) 2022 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (posframe "1.1"))
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

;; This package is an input method for Japanese. It is backed by a Google API
;; and does not require installation of native libraries or dictionaries.

;; To use it, switch to "japanese-riben" input method using
;; `toggle-input-method' or turn on `riben-mode' in a buffer you need to write
;; Japanese.

;;; Code:

(require 'riben-google)
(require 'riben-decode)
(require 'riben-posframe)
(require 'text-property-search)

(defgroup riben nil
  "A dumb Japanese input method."
  :prefix "riben-"
  :group 'i18n)

(defconst riben-decode-regexp
  (rx-to-string `(+ (any "0-9a-z"
                         ,(thread-last
                            riben-decode-punctuation-alist
                            (mapcar (lambda (cell) (char-to-string (car cell))))
                            (apply #'concat))))))

(defvar riben-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap self-insert-command] #'riben-self-insert-command)
    (define-key m "q" #'riben-mode-disable)
    m))

(defcustom riben-dispatch-trigger-alist
  '((?\, . "、")
    (?. . "。")
    (?? . "？")
    (?! . "！")
    (?\s))
  "Alist of keys that dispatch transliteration.

This is a cons cell of (CHARACTER . STRING) where CHARACTER is a
key entered in `riben-self-insert-command' and STRING is a text
that replaces the character.

This is usually a pair of terminating punctuations. As a special
case, the string can be nil, which only triggers transliterations
and vanishes the space."
  :type '(alist :key-type (character :tag "Character that triggers an event")
                :value-type (choice :tag "Replacement"
                                    string
                                    (const nil))))

(defcustom riben-continue-commands
  '(delete-backward-char)
  "List of commands that does not terminate input."
  :type '(repeat symbol))

(defvar riben--counter 0)
(defvar riben--remaining-candidates nil)
(defvar riben--segment nil)
(defvar riben--match-data nil)

;;;###autoload
(define-minor-mode riben-mode
  "A dumb Japanese input method."
  :lighter "Riben "
  (when riben-mode
    (setq deactivate-current-input-method-function
          #'riben-mode-disable)))

;;;###autoload
(register-input-method "japanese-riben" "Japanese" #'riben-mode
                       "日笨" "Riben ")

(defun riben-mode-disable ()
  (interactive)
  (riben-mode -1))

(defun riben-self-insert-command (n &optional c)
  (interactive "p")
  (unless (or (eq last-command #'riben-self-insert-command)
              (memq last-command riben-continue-commands))
    (cl-incf riben--counter))
  (self-insert-command n c)
  (put-text-property (- (point) n)
                     (point)
                     'riben--counter
                     riben--counter)
  (pcase (assq (char-after (1- (point))) riben-dispatch-trigger-alist)
    (`nil
     (when (thing-at-point-looking-at riben-decode-regexp 3)
       (let ((beg (match-beginning 0))
             (end (match-end 0)))
         (catch 'riben-self-insert
           (while (< beg end)
             (if (eq (get-char-property beg 'riben--counter) riben--counter)
                 (let* ((orig (buffer-substring-no-properties beg end))
                        (new (riben-decode-romaji orig)))
                   (unless (equal new orig)
                     (delete-region beg end)
                     (goto-char beg)
                     (insert new)
                     (put-text-property beg (point) 'riben--counter riben--counter)
                     (put-text-property beg (1+ beg) 'riben-original orig))
                   (throw 'riben-self-insert t))
               (cl-incf beg)))))))
    (`(,_ . ,c)
     (if (eq (get-char-property (- (point) 2) 'riben--counter) riben--counter)
         (progn
           (backward-delete-char n)
           (when c
             (insert c)
             (backward-char))
           (when (eq riben--counter (get-char-property (1- (point)) 'riben--counter))
             (riben-dispatch (when c 1))))
       (when c
         (backward-delete-char n)
         (when c
           (insert c)))))))

(defun riben-dispatch (&optional arg)
  (interactive)
  (if (region-active-p)
      (riben-on-region (region-beginning) (region-end))
    (when-let* ((counter (or (get-char-property (point) 'riben--counter)
                             (and (> (point) 1)
                                  (get-char-property (1- (point)) 'riben--counter))))
                (prop-match (save-excursion
                              (or (text-property-search-backward
                                   'riben--counter counter t)
                                  (text-property-search-forward
                                   'riben--counter counter t)))))
      (riben--dispatch (prop-match-beginning prop-match)
                       (prop-match-end prop-match)
                       counter arg))))

(defun riben-on-region (begin end)
  (interactive "r")
  (riben--dispatch begin end (cl-incf riben--counter)))

(defun riben--dispatch (begin end counter &optional forward-char)
  (let ((input (buffer-substring begin end)))
    (setq riben--remaining-candidates (riben-google-from-hiragana input))
    (cl-labels
        ((confirm
           (replace)
           (when replace
             (let* ((start (marker-position (car riben--match-data)))
                    (original (thread-last
                                (number-sequence start (marker-position
                                                        (nth 1 riben--match-data)))
                                (mapcar (lambda (i)
                                          (get-char-property i 'riben-original)))
                                (delq nil)
                                (apply #'concat))))
               (set-match-data riben--match-data)
               (replace-match replace)
               (put-text-property start (point) 'riben--counter counter)
               (put-text-property start (1+ start) 'riben-original original)
               (next))))
         (next
           ()
           (if-let (x (pop riben--remaining-candidates))
               (progn
                 (setq riben--segment (car x))
                 (if (string-match-p (rx bol (+ space) eol) riben--segment)
                     (next)
                   (search-forward riben--segment)
                   (setq riben--match-data (match-data))
                   (riben-posframe-complete (cadr x) #'confirm
                                            :point (match-beginning 0))))
             (when (numberp forward-char)
               (forward-char forward-char))
             (when (eq this-command #'riben-self-insert-command)
               (cl-incf riben--counter)))))
      (goto-char begin)
      (next))))

(provide 'riben)
;;; riben.el ends here
