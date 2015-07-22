(require 'powerline)

(defface wm-powerline-active1 '((t (:background "grey70" :foreground "grey10" :inherit mode-line)))
  "Powerline face 1."
  :group 'powerline)

(defface wm-powerline-active2 '((t (:background "grey30" :foreground "grey80" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface wm-powerline-inactive1
  '((t (:background "grey11" :inherit mode-line-inactive)))
  "Powerline face 1."
  :group 'powerline)

(defface wm-powerline-inactive2
  '((t (:background "grey20" :inherit mode-line-inactive)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-evil-insert-face
    '((((class color))
       (:background "green" :foreground "black" :weight bold))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defface powerline-evil-normal-face
    '((((class color))
       (:background "red" :foreground "black" :weight bold))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defface powerline-evil-visual-face
    '((((class color))
       (:background "yellow" :foreground "black" :weight bold))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defface powerline-evil-motion-face
    '((((class color))
       (:background "blue" :foreground "black" :weight bold))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defface powerline-evil-emacs-face
    '((((class color))
       (:background "blue violet" :foreground "black" :weight bold))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defface powerline-modified-flag-face
    '((((class color))
       (:weight bold :inherit 'powerline-active2))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defun powerline-evil-face (active)
    (let ((face (intern (concat "powerline-evil-" (symbol-name evil-state) "-face"))))
      (cond ((and active (facep face))
             face)
            (active 'powerline-active2)
            (t 'powerline-inactive2))))

  (defpowerline powerline-evil
    (concat (replace-regexp-in-string "[<> ]" "" (eval (evil-state-property evil-state :tag))) " "))

(defun powerline-evil-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'wm-powerline-active1 'wm-powerline-inactive1))
                          (face2 (if active 'wm-powerline-active2 'wm-powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
			  (pl-evil-face (ignore-errors (powerline-evil-face active)))
			  (powerline-evil (ignore-errors (powerline-evil pl-evil-face 'l)))
                          (lhs (list ;; NORMAL> master > foobar.txt > Fundamental >                 [ ] < 5: 0 < all

				;; NORMAL>
				powerline-evil
				(and powerline-evil (funcall separator-left pl-evil-face face1))

				;; master>
				(powerline-vc face1 'r)
				(when (bound-and-true-p nyan-mode)
				  (powerline-raw (list (nyan-create)) face1 'l))
				(funcall separator-left face1 face2)

				;; UUU foobar.txt>
				(powerline-raw "%*" face2 'l)
				(when powerline-display-mule-info
				  (powerline-raw mode-line-mule-info face2))
				(powerline-buffer-id face2 'l)
				(when (and (boundp 'which-func-mode) which-func-mode)
				  (powerline-raw which-func-format face2 'l))
				(powerline-raw " " face2)
				(funcall separator-left face2 mode-line)
				(when (boundp 'erc-modified-channels-object)
				  (powerline-raw erc-modified-channels-object mode-line 'l))

				;; Fundamental>
				(powerline-major-mode mode-line 'l)
				(powerline-process mode-line)
				(powerline-minor-modes mode-line 'l)
				(powerline-narrow mode-line 'l)
				))
                          (rhs (list (powerline-raw global-mode-string mode-line 'r)
                                     (funcall separator-right mode-line face2)
				     (unless window-system
				       (powerline-raw (char-to-string #xe0a1) face2 'l))
				     (powerline-raw "%4l" face2 'l)
				     (powerline-raw ":" face2 'l)
				     (powerline-raw "%3c" face2 'r)
				     (funcall separator-right face2 face1)
				     (powerline-raw " " face1)
				     (powerline-raw "%6p" face1 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face1 face2)))))
		     (concat (powerline-render lhs)
			     (powerline-fill mode-line (powerline-width rhs))
			     (powerline-render rhs)))))))

(provide 'powerline-evil-themes)
