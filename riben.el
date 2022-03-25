;;; riben.el --- A dictionary-less Japanese input method -*- lexical-binding: t -*-

(require 'riben-google)
(require 'riben-decode)

(defvar riben-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap self-insert-command] #'riben-self-insert-command)
    (define-key m [remap newline] #'riben-newline)
    (define-key m "q" #'riben-mode-disable)
    m))

(defcustom riben-trigger-alist
  '((?\, . ",")
    (?. . "。")
    (?\s))
  "")

(defcustom riben-continue-commands
  '(delete-backward-char)
  ""
  :type '(repeat symbol))

(defvar riben--counter 0)
(defvar riben--remaining-candidates nil)
(defvar riben--segment nil)
(defvar riben--match-data nil)

;;;###autoload
(define-minor-mode riben-mode
  ""
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

(defun riben-newline ()
  (interactive)
  (if (eq riben--counter (get-char-property (1- (point)) 'riben--counter))
      (cl-incf riben--counter)
    (newline nil t)))

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
  (pcase (assq (char-after (1- (point))) riben-trigger-alist)
    (`nil
     (when (thing-at-point-looking-at (rx (+ (any "a-z"))) 3)
       (let ((beg (match-beginning 0))
             (end (match-end 0)))
         (when (eq (get-char-property beg 'riben--counter) riben--counter)
           (let* ((orig (buffer-substring-no-properties beg end))
                  (new (riben-decode-romaji orig)))
             (unless (equal new orig)
               (delete-region beg end)
               (goto-char beg)
               (insert new)
               (put-text-property beg (point) 'riben--counter riben--counter)
               (put-text-property beg (1+ beg) 'riben-original orig)))))))
    (`(,_ . ,c)
     (backward-delete-char n)
     (when c
       (insert c)
       (backward-char))
     (when (eq riben--counter (get-char-property (1- (point)) 'riben--counter))
       (riben-dispatch (when c 1))))))

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
                 (search-forward riben--segment)
                 (setq riben--match-data (match-data))
                 (riben-posframe-complete (cadr x) #'confirm
                                         :point (match-beginning 0)))
             (when (numberp forward-char)
               (forward-char forward-char)))))
      (goto-char begin)
      (next))))

(provide 'riben)
;;; riben.el ends here
