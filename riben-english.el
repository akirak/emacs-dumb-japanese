;;; riben-english.el --- Transliterate English words into Katakana -*- lexical-binding: t -*-

(require 'riben-google)
(require 'riben-katakana)
(require 'riben-database)
(require 'emacsql)

(defgroup riben-english nil
  "A Japanese input method that translates English."
  :prefix "riben-english-"
  :group 'riben)

(defcustom riben-english-database-file
  (locate-user-emacs-file "riben/english.sqlite")
  ""
  :type 'file)

(defcustom riben-english-close-database-on-idle 300
  "Close the database connection on certain idle time in seconds."
  :type '(choice number boolean))

(defface riben-english-transient-face
  '((t (:inherit default :background "#333377")))
  "")

(defcustom riben-english-dispatch-hook nil
  ""
  :type 'hook)

(defvar riben-english-database-connection nil)

;;;; Macros
(defmacro riben-english-with-database (var &rest progn)
  (declare (indent 1))
  `(let ((,var (riben-english--open-database)))
     (condition-case nil
         (progn
           ,@progn)
       (progn
         (emacsql-close ,var)
         (setq riben-english-database-connection nil)))))

;;;; Japanese input

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
    (riben-turn-off-the-other-modes 'riben-english-mode)
    (when riben-english-close-database-on-idle
      (run-with-idle-timer riben-english-close-database-on-idle nil
                           #'riben-english-close-database))
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
    (_
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
      (user-error "No overlay at point")
      (run-hooks 'riben-english-dispatch-hook))))

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
      (user-error "No overlay at point")
      (run-hooks 'riben-english-dispatch-hook))))

(defun riben-english--translate (begin end)
  (let ((input (buffer-substring-no-properties begin end)))
    (delete-region begin end)
    (riben-english-insert-translation input)))

(defvar riben-english-alist nil)

;;;###autoload
(defun riben-english-insert (input)
  (interactive "sEnglish: ")
  (riben-english-with-database db
    (if-let (existing (riben-english--lookup db input))
        (insert existing)
      (pcase (riben-english--katakana-candidates input)
        (`(,sole)
         (riben-english--record-and-insert db input sole))
        (`nil
         (let ((words (split-string input)))
           (if (> (length words) 1)
               (dolist (word words)
                 (riben-english-insert word))
             (riben-english--record-and-insert
              db
              input
              (riben-english--prompt-new-word input)))))
        (candidates
         (riben-posframe-complete candidates
                                  (apply-partially #'riben-english--record-and-insert
                                                   db
                                                   input)))))))

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
        (riben-katakana-mode t))
    (read-string (format "%s: " word))))

(defun riben-english--all-candidates (input)
  (thread-last
    (riben-google--translate input)
    (cadr)
    (mapcar (lambda (cell) (nth 1 cell)))
    (apply #'append)))

(defun riben-english--katakana-candidates (input)
  (thread-last
    (riben-google--translate input)
    (riben-google-translate-part-of-speech "noun")
    (cl-remove-if-not #'riben-english-p)))

(defun riben-english-p (string)
  (string-match-p (rx bos (+ (category japanese-katakana-two-byte)) eos)
                  string))

;;;; Database

(defun riben-english--open-database ()
  (or (riben-english--live-connection)
      (let ((dir (file-name-directory riben-english-database-file))
            (new (not (file-exists-p riben-english-database-file))))
        (unless (file-directory-p dir)
          (make-directory dir))
        (let ((conn (funcall riben-database-backend riben-english-database-file)))
          (add-hook 'kill-emacs-hook #'riben-english-close-database)
          (condition-case _
              (progn
                (when new
                  (emacsql conn [:create-table-if-not-exists
                                 katakana ([(english text :primary-key)
                                            (katakana text)])]))
                (setq riben-english-database-connection conn))
            (error (emacsql-close conn)))))))

(defun riben-english--live-connection ()
  (when (and riben-english-database-connection
             (emacsql-live-p riben-english-database-connection))
    riben-english-database-connection))

(defun riben-english-close-database ()
  (when-let (conn (riben-english--live-connection))
    (emacsql-close conn)
    (setq riben-english-database-connection nil)))

;;;###autoload
(defun riben-english-register-katakana (english katakana)
  (interactive (let* ((english (read-string "English: "))
                      (katakana (riben-english--prompt-new-word english)))
                 (list english katakana)))
  (riben-english-with-database db
    (riben-english--record db english katakana)))

(defun riben-english--record (db input result)
  (emacsql db [:insert-or-replace :into katakana
                                  :values $v1]
           (vector input result)))

(defun riben-english--record-and-insert (db input result)
  (riben-english--record db input result)
  (insert result))

(defun riben-english--lookup (db input)
  (caar (emacsql db [:select katakana
                             :from katakana
                             :where (= english $s1)]
                 input)))

(provide 'riben-english)
;;; riben-english.el ends here
