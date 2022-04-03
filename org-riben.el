;;; org-riben.el --- Org integration -*- lexical-binding: t -*-

(require 'riben)
(require 'org)

(defgroup org-riben nil
  ""
  :prefix "org-riben-"
  :group 'riben)

(defcustom org-riben-yomigana-property "JAPANESE_YOMIGANA"
  "Name of the property that holds the original input."
  :type 'string)

;;;###autoload
(defun org-riben-annotate-yomigana ()
  "Add the yomigana for the heading as a property.

By adding this function to `org-capture-prepare-finalize-hook',
you can automatically record the yomigana when you fire
`org-capture'."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (when (string-match-p (rx (category japanese))
                          (buffer-substring-no-properties
                           (point) (line-end-position)))
      (re-search-forward org-heading-regexp)
      (pcase (thread-first
               (match-data t)
               (seq-drop 4)
               (seq-take 2))
        (`(,begin ,end)
         (goto-char begin)
         (let (result
               last-pos
               value)
           (catch 'yomigana
             (while (< (point) end)
               (if value
                   (push value result)
                 (push (buffer-substring-no-properties
                        (or last-pos begin)
                        (1- (point)))
                       result))
               (setq value (get-char-property (point) 'riben-original)
                     last-pos (point))
               (if-let (pos (next-char-property-change (point) end))
                   (goto-char pos)
                 (throw 'yomigana nil))))
           (org-entry-put nil org-riben-yomigana-property
                          (string-join (nreverse result)))))))))

(defun org-riben-complete (prompt &optional scope)
  (let* ((alist (org-map-entries (lambda ()
                                   (list (org-entry-get nil org-riben-yomigana-property)
                                         (point-marker)
                                         (org-format-outline-path
                                          (org-get-outline-path t t))))
                                 (concat org-riben-yomigana-property "={.+}")
                                 scope))
         (items (mapcar (lambda (cell)
                          (propertize (car cell)
                                      'invisible t
                                      'point-marker (nth 1 cell)))
                        alist))
         (ann-func (make-symbol "org-riben-annotation"))
         (input (progn
                  (fset ann-func (apply-partially (lambda (alist string)
                                                    (caddr (assoc string alist)))
                                                  alist))
                  (completing-read
                   prompt
                   `(lambda (string pred action)
                      (if (eq action 'metadata)
                          '(metadata . ((category . riben)
                                        (annotation-function . ,ann-func)))
                        (complete-with-action action ',items string pred)))))))
    (if-let (cell (assoc input alist))
        (cadr cell)
      input)))

;;;###autoload
(defun org-riben-jump-to-heading ()
  "Jump to a heading by yomigana."
  (interactive)
  (let ((completion (org-riben-complete "Heading: ")))
    (when (markerp completion)
      (org-goto-marker-or-bmk completion))))

(provide 'org-riben)
;;; org-riben.el ends here
