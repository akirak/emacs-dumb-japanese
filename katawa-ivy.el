;;; katawa-ivy.el --- Ivy interface to katawa -*- lexical-binding: t -*-

;; Copyright (C) 2018,2020 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.2
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

(defconst katawa-ivy-target-regexp (rx (+ bow
                                (+? (not nonascii))
                                (or (+ (any punct))
                                    eow))))

;;;###autoload
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

(defun katawa-ivy-on-region (start end)
  "Re-transliterate or edit text in a region starting at START and ending at END."
  (interactive "R")
  (let ((str (buffer-substring-no-properties start end)))
    (when (< start end)
      (ivy-read (format "Transliterate \"%s\": " str)
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

;;;###autoload
(defun katawa-ivy-at-point ()
  "Re-transliterate or edit text in a region.

If a region from START to END is active, the selected text is re-transliterated
via an interface similar to that of `katawa-ivy'.  The selected region is
replaced with the new candidate after selection.

If no region is active, a certain portion of text before the cursor is selected
instead.  The selected region is usually a consecutive text of uni-byte
characters which does not include spaces and sentence delimiters (\".!?\")."
  (interactive)
  (if (region-active-p)
      (katawa-ivy-on-region (region-beginning) (region-end))
    (let ((initial (point))
          (bound (point)))
      (when (looking-back (rx bow (+ (not nonascii))))
        (beginning-of-line)
        (while (re-search-forward (rx (+ (or blank nonascii)))
                                  bound t)))
      (if (and (eq initial (point))
               (not (looking-at katawa-ivy-target-regexp)))
          (katawa-ivy)
        (let ((beg (point))
              (end (save-excursion
                     (re-search-forward katawa-ivy-target-regexp
                                        (line-end-position)))))
          (goto-char initial)
          (katawa-ivy-on-region beg end))))))

(provide 'katawa-ivy)
;;; katawa-ivy.el ends here
