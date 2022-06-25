;;; riben-katakana.el --- An alternative Katakana input mode -*- lexical-binding: t -*-

(require 'riben)

;;;###autoload
(register-input-method "japanese-riben-katakana" "Japanese" #'riben-katakana-mode
                       "日语ひらがな" "Riben Katakana ")

(defvar riben-katakana-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap self-insert-command] #'riben-katakana-self-insert-command)
    m))

;;;###autoload
(define-minor-mode riben-katakana-mode
  "A Katakana input mode."
  :lighter "Riben Katakana "
  (when riben-katakana-mode
    (riben-turn-off-the-other-modes 'riben-katakan-mode)
    (setq deactivate-current-input-method-function
          #'riben-katakana-mode-disable)))

(defun riben-katakana-mode-disable ()
  (interactive)
  (riben-katakana-mode -1))

(defun riben-katakana-self-insert-command (n &optional c)
  (interactive "p")
  (self-insert-command n c)
  (when (thing-at-point-looking-at riben-decode-regexp 3)
    (let* ((beg (match-beginning 0))
           (end (match-end 0))
           (orig (buffer-substring-no-properties beg end))
           (new (riben-decode-romaji orig)))
      (unless (equal orig new)
        (delete-region beg end)
        (goto-char beg)
        (insert (japanese-katakana new))))))

(provide 'riben-katakana)
;;; riben-katakana.el ends here
