;;; riben-english.el --- Transliterate English words into Katakana -*- lexical-binding: t -*-

(require 'riben-google)
(require 'riben-katakana)

(defface riben-english-transient-face
  '((t (:inherit default :background "#333377")))
  "")

(defcustom riben-english-dispatch-hook nil
  ""
  :type 'hook)

;;;###autoload
(register-input-method "japanese-riben-english" "Japanese Katakana"
                       #'riben-english-mode
                       "カナ" "Riben-English ")

(defvar riben-english-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap self-insert-command] #'riben-english-self-insert)
    m))

;;;###autoload
(define-minor-mode riben-english-mode
  "An input method that transliterates English to Katakana."
  :lighter "Riben-English "
  (when riben-english-mode
    (setq deactivate-current-input-method-function
          #'riben-english-mode-disable)))

(defun riben-english-mode-disable ()
  (interactive)
  (riben-mode -1))

(defun riben-english-self-insert (n &optional c)
  (interactive "p")
  (self-insert-command n c)
  (pcase (char-after (1- (point)))
    ;; space
    (32
     (progn
       (backward-delete-char n)
       (riben-english-translate-to-katakana)))
    ;; period
    (46
     (progn
       (backward-delete-char n)
       (riben-english-translate)))
    (t
     (if-let (ov (riben-english--overlay (- (point) n)))
         (setf (overlay-end ov) (point))
       (let ((ov (make-overlay (- (point) n) (point))))
         (overlay-put ov 'face 'riben-english-transient-face)
         (overlay-put ov 'riben-english t))))))

(defun riben-english--overlay (&optional pos)
  (thread-last
    (overlays-at (1- (or pos (point))))
    (seq-find (lambda (ov)
                (overlay-get ov 'riben-english)))))

(defun riben-english-translate-to-katakana ()
  (interactive)
  (if (region-active-p)
      (save-excursion
        (riben-english--translate-to-katakana (region-beginning)
                                              (region-end)))
    (if-let (ov (riben-english--overlay))
        (let ((begin (overlay-start ov))
              (end (overlay-end ov)))
          (delete-overlay ov)
          (riben-english--translate-to-katakana begin end)
          (run-hooks 'riben-english-dispatch-hook))
      (user-error "No overlay at point"))))

(defun riben-english--translate-to-katakana (begin end)
  (let ((input (buffer-substring-no-properties begin end)))
    (delete-region begin end)
    (riben-english-insert input)))

(defun riben-english-translate ()
  (interactive)
  (if (region-active-p)
      (save-excursion
        (riben-english--translate (region-beginning)
                                  (region-end)))
    (if-let (ov (riben-english--overlay))
        (let ((begin (overlay-start ov))
              (end (overlay-end ov)))
          (delete-overlay ov)
          (riben-english--translate begin end)
          (run-hooks 'riben-english-dispatch-hook))
      (user-error "No overlay at point"))))

(defun riben-english--translate (begin end)
  (let ((input (buffer-substring-no-properties begin end)))
    (delete-region begin end)
    (riben-english-insert-translation input)))

(defvar riben-english-alist nil)

;;;###autoload
(defun riben-english-insert (input)
  (interactive "sEnglish: ")
  (if-let (existing (riben-english--lookup input))
      (insert existing)
    (pcase (riben-english--katakana-candidates input)
      (`(,sole)
       (riben-english--record-and-insert input sole))
      (`nil
       (let ((words (split-string input)))
         (if (> (length words) 1)
             (dolist (word words)
               (riben-english-insert word))
           (riben-english--record-and-insert
            input
            (riben-english--prompt-new-word input)))))
      (candidates
       (riben-posframe-complete candidates
                                (apply-partially #'riben-english--record-and-insert
                                                 input))))))

;;;###autoload
(defun riben-english-insert-translation (input)
  (interactive "sEnglish: ")
  (pcase (riben-english--all-candidates input)
    (`(,sole)
     (insert sole))
    (`nil
     (insert input))
    (candidates
     (riben-posframe-complete candidates #'insert))))

(defun riben-english--prompt-new-word (word)
  "Register a new WORD."
  (minibuffer-with-setup-hook
      (lambda ()
        (set-input-method 'riben-katakana))
    (read-string (format "%s: " word))))

(defun riben-english--record (input result)
  (push `(,input . ,result) riben-english-alist))

(defun riben-english--record-and-insert (input result)
  (riben-english--record input result)
  (insert result))

(defun riben-english--lookup (input)
  (cdr (assoc input riben-english-alist)))

(defun riben-english--all-candidates (input)
  (thread-last
    (riben-google--translate input)
    (cadr)
    (mapcar (lambda (cell) (nth 1 cell)))
    (-flatten-n 1)))

(defun riben-english--katakana-candidates (input)
  (thread-last
    (riben-google--translate input)
    (riben-google-translate-part-of-speech "noun")
    (cl-remove-if-not #'riben-english-p)))

(defun riben-english-p (string)
  (string-match-p (rx bos (+ (category japanese-katakana-two-byte)) eos)
                  string))

(provide 'riben-english)
;;; riben-english.el ends here
