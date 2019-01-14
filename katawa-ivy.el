;;; katawa-ivy.el --- Ivy interface to katawa -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (ivy "0.10") (dash "2.12") (request "0.3.0"))
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

;; This library provides an Ivy interface to katawa.  It allows you to write and
;; edit Japanese text in Emacs with the help of Google CGI API for Japanese
;; Input.

;;; Code:

(require 'subr-x)
(require 'katawa)
(require 'katawa-google)
(require 'ivy)

(defvar katawa-ivy-history nil)

(defun katawa-ivy ()
  "Transliterate text into Japanese via an Ivy interface."
  (interactive)
  (ivy-read "katawa: "
            (lambda (string)
              (unless (string-empty-p string)
                (katawa-get-some-candidates string)))
            :caller 'katawa-ivy
            :history 'katawa-ivy-history
            :action #'insert
            :matcher (lambda (_ cands) cands)
            :dynamic-collection t))

(defun katawa-ivy--fix-region (start end)
  "Re-transliterate or edit text in a region starting at START and ending at END."
  (let ((str (buffer-substring-no-properties start end)))
    (when (< start end)
      (ivy-read (format "katawa fix %s: " str)
                (lambda (string)
                  (unless (string-empty-p string)
                    (katawa-get-some-candidates string)))
                :caller 'katawa-ivy-fix
                :history 'katawa-ivy-history
                :initial-input str
                :action (lambda (s)
                          (kill-region start end)
                          (goto-char start)
                          (insert s))
                :matcher (lambda (_ cands) cands)
                :dynamic-collection t))))

(defun katawa-ivy-fix (start end)
  "Re-transliterate or edit text in a region.

If a region from START to END is active, the selected text is re-transliterated
via an interface similar to that of `katawa-ivy'.  The selected region is
replaced with the new candidate after selection.

If no region is active, a certain portion of text before the cursor is selected
instead.  The selected region is usually a consecutive text of uni-byte
characters which does not include spaces and sentence delimiters (\".!?\")."
  (interactive "r")
  (cond
   ((region-active-p)
    (katawa-ivy--fix-region start end))
   ((thing-at-point 'word)
    (apply #'katawa-ivy--fix-region
           (save-excursion
             (list (progn (re-search-backward "[[:multibyte:][:space:].!?]"
                                              nil 'noerror)
                          (unless (looking-at "\w")
                            (goto-char (1+ (point))))
                          (point))
                   (progn (re-search-forward "[[:multibyte:][:space:].!?]"
                                             nil 'noerror)
                          (when (looking-at "[[:multibyte:][:space:]]")
                            (goto-char (1- (point))))
                          (point))))))
   (t (katawa-ivy))))

(defun katawa-ivy-fix-at-point (start end)
  "Re-transliterate or edit a segment under the cursor.

This is another version of `katawa-ivy-fix' which analyses a Japanese text
around the cursor and rewrite the segment at point.

START is the beginning of a region, and END is the end of the region."
  (interactive "r")
  (cond
   ((region-active-p)
    (katawa-ivy--fix-region start end))
   ((thing-at-point 'word)
    (let* ((str (thing-at-point 'line))
           (segments (mapcar #'car (katawa-google--request str)))
           (col (- (point) (point-at-bol)))
           (seg-region (cl-loop for seg in segments
                                with a = 0
                                for next-col = (+ a (length seg))
                                if (> next-col col)
                                return (let ((start (+ (point-at-bol) a)))
                                         (list start (+ start (length seg))))
                                else
                                do (setq a next-col))))
      (if (< (length seg-region) 2)
          (error "Invalid arguments: %s" (prin1-to-string seg-region))
        (apply #'katawa-ivy--fix-region seg-region))))
   (t (katawa-ivy))))

(provide 'katawa-ivy)
;;; katawa-ivy.el ends here
