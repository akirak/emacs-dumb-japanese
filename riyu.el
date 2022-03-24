;;; riyu.el --- A dictionary-less Japanese input method -*- lexical-binding: t -*-

(defvar riyu-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap self-insert-command] #'riyu-self-insert-command)
    m))

(defcustom riyu-trigger-alist
  '((?\, . ",")
    (?. . "。")
    (?\s))
  "")

(defvar riyu-counter 0)

(define-minor-mode riyu-mode
  ""
  :lighter "Riyu ")

(defun riyu-self-insert-command (n &optional c)
  (interactive "p")
  (unless (eq last-command #'riyu-self-insert-command)
    (cl-incf riyu-counter))
  (self-insert-command n c)
  (put-text-property (- (point) n)
                     (point)
                     'riyu-counter
                     riyu-counter)
  (pcase (assq (char-after (1- (point))) riyu-trigger-alist)
    (`nil
     (when (thing-at-point-looking-at (rx (+ (any "a-z")))
                                      3)
       (let ((beg (match-beginning 0))
             (end (match-end 0)))
         (when (eq (get-char-property beg 'riyu-counter) riyu-counter)
           (let* ((orig (buffer-substring-no-properties beg end))
                  (new (katawa-decode-romaji orig)))
             (unless (equal new orig)
               (delete-region beg end)
               (insert new)
               (put-text-property beg (point) 'riyu-counter riyu-counter)
               (put-text-property (1- (point)) (point) 'riyu-original orig)))))))
    (`(,_ . ,c)
     (backward-char)
     (if c
         (progn
           (insert c)
           (delete-char 1)
           (backward-char 1))
       (delete-char 1))
     (unwind-protect (riyu-dispatch)
       (when c
         (forward-char 1))))))

(defun riyu-dispatch ()
  (interactive)
  (if (region-active-p)
      (riyu-dispatch-on-region (region-beginning) (region-end))
    (when-let* ((counter (or (get-char-property (point) 'riyu-counter)
                             (get-char-property (1- (point)) 'riyu-counter)))
                (prop-match (save-excursion
                              (or (text-property-search-backward
                                   'riyu-counter
                                   counter
                                   t)
                                  (text-property-search-forward
                                   'riyu-counter
                                   counter
                                   t)))))
      (let* ((begin (prop-match-beginning prop-match))
             (end (prop-match-end prop-match))
             (input (buffer-substring-no-properties begin end))
             (candidates (katawa-google-from-hiragana
                          input)))
        (when candidates
          (let ((replace (completing-read (format "\"%s\": " input) candidates))
                (original (thread-last
                            (number-sequence begin end)
                            (mapcar (lambda (pos)
                                      (get-char-property pos 'riyu-original)))
                            (delq nil)
                            (apply #'concat))))
            (delete-region begin end)
            (goto-char begin)
            (insert replace)
            (put-text-property begin (point) 'riyu-counter counter)
            (put-text-property begin (point) 'riyu-original original)))))))

(defun riyu-dispatch-on-region (begin end)
  (interactive "r")
  (let* ((input (buffer-substring-no-properties begin end))
         (candidates (katawa-google-from-hiragana
                      (katawa-decode-romaji input))))
    (when candidates
      (let ((replace (completing-read (format "\"%s\": " input) candidates)))
        (delete-region begin end)
        (goto-char begin)
        (insert replace)
        (cl-incf riyu-counter)
        (put-text-property begin (point) 'riyu-counter riyu-counter)))))

(provide 'riyu)
;;; riyu.el ends here
