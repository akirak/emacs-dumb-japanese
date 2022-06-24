;;; riben-posframe.el -- Completion interface using posframe.el -*- lexical-binding: t -*-

(require 'posframe)

(defconst riben-posframe-buffer "*riben posframe*")

(defcustom riben-posframe-access-keys
  (string-to-list "asdfghjkl")
  "List of shortcut keys used to select a candidate by index."
  :group 'riben
  :type '(repeat character))

(defvar riben-posframe--candidates nil)
(defvar riben-posframe--selection nil)
(defvar riben-posframe--callback nil)

(defvar riben-posframe-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down>") #'riben-posframe-next)
    (define-key map (kbd "C-n") #'riben-posframe-next)
    (define-key map (kbd "<up>") #'riben-posframe-previous)
    (define-key map (kbd "C-p") #'riben-posframe-previous)
    (define-key map (kbd "C-g") #'riben-posframe-cancel)
    (define-key map (kbd "C-f") #'riben-posframe-confirm)
    (define-key map (kbd "RET") #'riben-posframe-confirm)
    (dolist (i (number-sequence 0 (1- (length riben-posframe-access-keys))))
      (let ((cmd (intern (concat "riben-posframe-confirm-" (int-to-string i)))))
        (fset cmd `(lambda ()
                     (interactive)
                     (let ((n ,i))
                       (when (< n (length riben-posframe--candidates))
                         (setq riben-posframe--selection n)
                         (riben-posframe-confirm)))))
        (define-key map (vector (elt riben-posframe-access-keys i)) cmd)))
    map))

(defface riben-posframe-inactive-face
  '((t (:inherit default :background "#555555" :foreground "#cccccc")))
  "")

(defface riben-posframe-active-face
  '((t (:inherit default :background "#99aaff" :foreground "#111111")))
  "")

(defface riben-posframe-navigation-face
  '((t (:inherit default :background "#333333" :foreground "#dddddd")))
  "")

(defface riben-posframe-header-face
  `((t (:inherit default
                 :background ,(face-foreground 'default nil t)
                 :foreground ,(face-background 'default nil t))))
  "")

(cl-defun riben-posframe-complete (candidates callback &key point)
  (setq riben-posframe--candidates candidates
        riben-posframe--callback callback
        riben-posframe--selection 0)
  (let ((frame (posframe-show
                riben-posframe-buffer
                :string (riben-posframe--format-candidates)
                :foreground-color (face-foreground 'riben-posframe-inactive-face nil t)
                :background-color (face-background 'riben-posframe-inactive-face nil t)
                :height (length candidates)
                :width (+ 4 (* 2 (apply #'max (mapcar #'length candidates))))
                :position point)))
    (set-transient-map riben-posframe-map #'riben-posframe--keep-p
                       #'riben-posframe-exit)))

(defun riben-posframe--keep-p ()
  (memq this-command '(riben-posframe-next
                       riben-posframe-previous)))

(defun riben-posframe--format-candidates ()
  (let ((i 0))
    (with-temp-buffer
      (put-text-property (point-min) (point-min)
                         'face 'riben-posframe-header-face)
      (dolist (cand riben-posframe--candidates)
        (if (< i (length riben-posframe-access-keys))
            (insert (char-to-string (elt riben-posframe-access-keys i))
                    ": ")
          (insert "   "))
        (put-text-property (line-beginning-position) (point)
                           'face 'riben-posframe-navigation-face)
        (let ((start (point)))
          (insert cand "\n")
          (when (eq i riben-posframe--selection)
            (put-text-property start (point)
                               'face 'riben-posframe-active-face)))
        (cl-incf i))
      (buffer-string))))

(defun riben-posframe-previous (&optional arg)
  (interactive "P")
  (riben-posframe-next (if arg (- arg) -1)))

(defun riben-posframe-next (&optional arg)
  (interactive "P")
  (let ((arg (or arg 1))
        (max (1- (length riben-posframe--candidates))))
    (cl-incf riben-posframe--selection arg)
    (cond
     ((< riben-posframe--selection 0)
      (setq riben-posframe--selection 0))
     ((> riben-posframe--selection max)
      (setq riben-posframe--selection max)))
    (riben-posframe--update)))

(defun riben-posframe--update ()
  (with-current-buffer riben-posframe-buffer
    (erase-buffer)
    (insert (riben-posframe--format-candidates)))
  (posframe-refresh riben-posframe-buffer))

(defun riben-posframe-exit ()
  (interactive)
  (posframe-hide riben-posframe-buffer))

(defun riben-posframe-confirm ()
  (interactive)
  (funcall riben-posframe--callback
           (elt riben-posframe--candidates riben-posframe--selection)))

(defun riben-posframe-cancel ()
  (interactive)
  (posframe-hide riben-posframe-buffer)
  (funcall riben-posframe--callback nil))

(provide 'riben-posframe)
;;; riben-posframe.el ends here
