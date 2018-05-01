;;; company-katawa.el --- Company backend for katawa -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (dash "2.12") (request "0.3.0") (company "0.9.4"))
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

;; This library provides a company backend for katawa, a Japanese transliteration
;; API. It behaves like an input method, except that it is not implemented as
;; a real input method.
;;
;; To activate the Japanese transliteration, add `company-katawa-backend' to
;; `company-backends' and activate `company-katawa-mode' in buffers in which
;; you want to use the backend.

;;; Code:

(require 'company)
(require 'katawa)

;;;###autoload
(define-minor-mode company-katawa-mode
  "Activate katawa Japanese transliteration via company in the buffer.")

;;;###autoload
(defun company-katawa-backend (command &optional arg &rest ignored)
  "Company backend for katawa.

COMMAND, ARG, and IGNORED follow the conventions of company."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-katawa-backend))
    (prefix (when (and (looking-at "\\_>")
                       (looking-back "[a-z][a-z,]*" nil t))
              (match-string 0)))
    (candidates (when company-katawa-mode
                  (katawa-get-some-candidates arg)))))

(provide 'company-katawa)
;;; company-katawa.el ends here
