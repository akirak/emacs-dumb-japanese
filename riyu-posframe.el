;;; riyu-posframe.el -- Completion interface using posframe.el -*- lexical-binding: t -*-

(require 'posframe)

(defconst riyu-posframe-buffer "*riyu posframe*")

(defcustom riyu-posframe-keys
  (string-to-list "asdfghjkl")
  ""
  :type '(repeat character))

(defvar riyu-posframe--candidates nil)
(defvar riyu-posframe--selection nil)
(defvar riyu-posframe--callback nil)

(defvar riyu-posframe-exit-commands '(riyu-posframe-confirm
                                      riyu-posframe-cancel))

(defvar riyu-posframe-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down>") #'riyu-posframe-next)
    (define-key map (kbd "C-n") #'riyu-posframe-next)
    (define-key map (kbd "<up>") #'riyu-posframe-previous)
    (define-key map (kbd "C-p") #'riyu-posframe-previous)
    (define-key map (kbd "C-g") #'riyu-posframe-cancel)
    (define-key map (kbd "C-f") #'riyu-posframe-confirm)
    (define-key map (kbd "RET") #'riyu-posframe-confirm)
    (dolist (i (number-sequence 0 (1- (length riyu-posframe-keys))))
      (let ((cmd (intern (concat "riyu-posframe-confirm-" (int-to-string i)))))
        (fset cmd `(lambda ()
                     (interactive)
                     (let ((n ,i))
                       (when (< n (length riyu-posframe--candidates))
                         (setq riyu-posframe--selection n)
                         (riyu-posframe-confirm)))))
        (define-key map (vector (elt riyu-posframe-keys i)) cmd)
        (push cmd riyu-posframe-exit-commands)))
    map))

(defface riyu-posframe-inactive-face
  '((t (:inherit default :background "#555555" :foreground "#cccccc")))
  "")

(defface riyu-posframe-active-face
  '((t (:inherit default :background "#99aaff" :foreground "#111111")))
  "")

(defface riyu-posframe-navigation-face
  '((t (:inherit default :background "#333333" :foreground "#dddddd")))
  "")

(defface riyu-posframe-header-face
  `((t (:inherit default
                 :background ,(face-foreground 'default nil t)
                 :foreground ,(face-background 'default nil t))))
  "")

(cl-defun riyu-posframe-complete (candidates callback &key point)
  (setq riyu-posframe--candidates candidates
        riyu-posframe--callback callback
        riyu-posframe--selection 0)
  (let ((frame (posframe-show
                riyu-posframe-buffer
                :string (riyu-posframe--format-candidates)
                :foreground-color (face-foreground 'riyu-posframe-inactive-face nil t)
                :background-color (face-background 'riyu-posframe-inactive-face nil t)
                :height (length candidates)
                :width (+ 4 (* 2 (apply #'max (mapcar #'length candidates))))
                :position point)))
    (set-transient-map riyu-posframe-map #'riyu-posframe--keep-p
                       #'riyu-posframe-confirm)))

(defun riyu-posframe--keep-p ()
  (not (memq this-command riyu-posframe-exit-commands)))

(defun riyu-posframe--format-candidates ()
  (let ((i 0))
    (with-temp-buffer
      (put-text-property (point-min) (point-min)
                         'face 'riyu-posframe-header-face)
      (dolist (cand riyu-posframe--candidates)
        (if (< i (length riyu-posframe-keys))
            (insert (char-to-string (elt riyu-posframe-keys i))
                    ": ")
          (insert "   "))
        (put-text-property (line-beginning-position) (point)
                           'face 'riyu-posframe-navigation-face)
        (let ((start (point)))
          (insert cand "\n")
          (when (eq i riyu-posframe--selection)
            (put-text-property start (point)
                               'face 'riyu-posframe-active-face)))
        (cl-incf i))
      (buffer-string))))

(defun riyu-posframe-previous (&optional arg)
  (interactive "P")
  (riyu-posframe-next (if arg (- arg) -1)))

(defun riyu-posframe-next (&optional arg)
  (interactive "P")
  (let ((arg (or arg 1))
        (max (1- (length riyu-posframe--candidates))))
    (cl-incf riyu-posframe--selection arg)
    (cond
     ((< riyu-posframe--selection 0)
      (setq riyu-posframe--selection 0))
     ((> riyu-posframe--selection max)
      (setq riyu-posframe--selection max)))
    (riyu-posframe--update)))

(defun riyu-posframe--update ()
  (with-current-buffer riyu-posframe-buffer
    (erase-buffer)
    (insert (riyu-posframe--format-candidates)))
  (posframe-refresh riyu-posframe-buffer))

(defun riyu-posframe-confirm ()
  (interactive)
  (posframe-hide riyu-posframe-buffer)
  (funcall riyu-posframe--callback
           (elt riyu-posframe--candidates riyu-posframe--selection)))

(defun riyu-posframe-cancel ()
  (interactive)
  (posframe-hide riyu-posframe-buffer)
  (funcall riyu-posframe--callback nil))

(provide 'riyu-posframe)
;;; riyu-posframe.el ends here
