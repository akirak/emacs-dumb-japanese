;;; riyu.el --- A dictionary-less Japanese input method -*- lexical-binding: t -*-

(require 'riyu-google)
(require 'riyu-decode)
(require 'mule-cmds)

(defvar riyu-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap self-insert-command] #'riyu-self-insert-command)
    (define-key m [remap newline] #'riyu-newline)
    (define-key m "q" #'riyu-mode-disable)
    m))

(defcustom riyu-trigger-alist
  '((?\, . ",")
    (?. . "。")
    (?\s))
  "")

(defcustom riyu-continue-commands
  '(delete-backward-char)
  ""
  :type '(repeat symbol))

(defvar riyu--counter 0)
(defvar riyu--remaining-candidates nil)
(defvar riyu--segment nil)
(defvar riyu--match-data nil)

;;;###autoload
(define-minor-mode riyu-mode
  ""
  :lighter "Riyu "
  (when riyu-mode
    (setq deactivate-current-input-method-function
          #'riyu-mode-disable)))

;;;###autoload
(register-input-method "japanese-riyu" "Japanese" #'riyu-mode
                       "日笨" "Riyu ")

(defun riyu-mode-disable ()
  (interactive)
  (riyu-mode -1))

(defun riyu-newline ()
  (interactive)
  (if (eq riyu--counter (get-char-property (1- (point)) 'riyu--counter))
      (cl-incf riyu--counter)
    (newline nil t)))

(defun riyu-self-insert-command (n &optional c)
  (interactive "p")
  (unless (or (eq last-command #'riyu-self-insert-command)
              (memq last-command riyu-continue-commands))
    (cl-incf riyu--counter))
  (self-insert-command n c)
  (put-text-property (- (point) n)
                     (point)
                     'riyu--counter
                     riyu--counter)
  (pcase (assq (char-after (1- (point))) riyu-trigger-alist)
    (`nil
     (when (thing-at-point-looking-at (rx (+ (any "a-z"))) 3)
       (let ((beg (match-beginning 0))
             (end (match-end 0)))
         (when (eq (get-char-property beg 'riyu--counter) riyu--counter)
           (let* ((orig (buffer-substring-no-properties beg end))
                  (new (riyu-decode-romaji orig)))
             (unless (equal new orig)
               (delete-region beg end)
               (goto-char beg)
               (insert new)
               (put-text-property beg (point) 'riyu--counter riyu--counter)
               (put-text-property beg (1+ beg) 'riyu-original orig)))))))
    (`(,_ . ,c)
     (backward-delete-char n)
     (when c
       (insert c)
       (backward-char))
     (when (eq riyu--counter (get-char-property (1- (point)) 'riyu--counter))
       (riyu-dispatch (when c 1))))))

(defun riyu-dispatch (&optional arg)
  (interactive)
  (if (region-active-p)
      (riyu-on-region (region-beginning) (region-end))
    (when-let* ((counter (or (get-char-property (point) 'riyu--counter)
                             (and (> (point) 1)
                                  (get-char-property (1- (point)) 'riyu--counter))))
                (prop-match (save-excursion
                              (or (text-property-search-backward
                                   'riyu--counter counter t)
                                  (text-property-search-forward
                                   'riyu--counter counter t)))))
      (riyu--dispatch (prop-match-beginning prop-match)
                      (prop-match-end prop-match)
                      counter arg))))

(defun riyu-on-region (begin end)
  (interactive "r")
  (riyu--dispatch begin end (cl-incf riyu--counter)))

(defun riyu--dispatch (begin end counter &optional forward-char)
  (let ((input (buffer-substring begin end)))
    (setq riyu--remaining-candidates (riyu-google-from-hiragana input))
    (cl-labels
        ((confirm
           (replace)
           (when replace
             (let* ((start (marker-position (car riyu--match-data)))
                    (original (thread-last
                                (number-sequence start (marker-position
                                                        (nth 1 riyu--match-data)))
                                (mapcar (lambda (i)
                                          (get-char-property i 'riyu-original)))
                                (delq nil)
                                (apply #'concat))))
               (set-match-data riyu--match-data)
               (replace-match replace)
               (put-text-property start (point) 'riyu--counter counter)
               (put-text-property start (1+ start) 'riyu-original original)
               (next))))
         (next
           ()
           (if-let (x (pop riyu--remaining-candidates))
               (progn
                 (setq riyu--segment (car x))
                 (search-forward riyu--segment)
                 (setq riyu--match-data (match-data))
                 (riyu-posframe-complete (cadr x) #'confirm
                                         :point (match-beginning 0)))
             (when (numberp forward-char)
               (forward-char forward-char)))))
      (goto-char begin)
      (next))))

(provide 'riyu)
;;; riyu.el ends here
