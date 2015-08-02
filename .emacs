;=====================================================================
; General Settings
;=====================================================================
(add-to-list 'load-path "~/.emacs.d/elisp")

; disable menu bar
(menu-bar-mode -1)

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))


; Show line number
;(setq linum-format "%4d\u2502")
(setq linum-format "%4d|")
(global-linum-mode 0)

; disable line number for some major-modes
(setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode))
(setq linum-disable-starred-buffers 't)

; overwrite the function defined in 'linum.el'
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
              (and linum-disable-starred-buffers (string-match "*" (buffer-name))))
    (linum-mode 1)))

; enable highlight line mode
(add-hook 'find-file-hook
	  (lambda ()
	    (hl-line-mode 1)
	    (set-face-background 'hl-line "gray25")))

; Show matched parents
(show-paren-mode t)

; turn on auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; turn on electirc pair mode
; (electric-pair-mode t)

;;disable splash screen and minibuffer messages
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)

(setq default-major-mode 'text-mode)

; font settings
;; no font for text-mode

;;; set meta key for Mac OS
(cond
 ((string-equal system-type "darwin")
  (progn
    (customize-set-variable 'ns-command-modifier 'meta)))
 ((string-equal system-type "gnu/linux")
  (progn
    (if window-system
	(progn
	  ;; Setting English Font
	  (set-default-font "DejaVu Sans Mono-10")

	  ;; Chinese Font
	  (dolist (charset '(kana han symbol cjk-misc bopomofo))
	    (set-fontset-font (frame-parameter nil 'font)
			      charset
					;(font-spec :family "WenQuanYi Micro Hei Mono" :size 12)
			      (font-spec :family "WenQuanYi Micro Hei Mono")
			      ))
	  (setq face-font-rescale-alist '(("WenQuanYi Micro Hei Mono" . 1.2))))))))

;; turn on save place so that when opening a file, the cursor will be at the last position.
(require 'saveplace)
;(setq save-place-file (concat user-emacs-directory "saveplace.el") ) ; use standard emacs dir
(setq-default save-place t)

;;; scroll by one line.
(setq scroll-conservatively 1)
(setq scroll-step 1)

;;; enable mouse support
;; (xterm-mouse-mode 1)

;;; kill all buffers in a frame when killing a frame
;; (add-hook 'delete-frame-functions
;;           (lambda (frame)
;;             (let* ((window (frame-selected-window frame))
;;                    (buffer (and window (window-buffer window))))
;;               (when (and buffer (buffer-file-name buffer))
;;                 (kill-buffer buffer)))))

;(require 'ido)
;(setq ido-enable-flex-matching t)
;(setq ido-everywhere t)
;(ido-mode t)

;;; indent with spaces instead of tabs
;; Exceptions: Makefile and ChangeLog modes.
(add-hook 'find-file-hook '(lambda ()
  (if (and buffer-file-name
           (not (string-equal mode-name "Change Log"))
           (not (string-equal mode-name "Makefile"))
           (not (string-equal mode-name "GNUmakefile")))
      (setq indent-tabs-mode nil))))

;;; Utilities
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1) (debug t))
    `(eval-after-load ,file '(progn ,@body))))

(require 'utility-functions)
;--------------------------------------------------
; Global key bindings
;--------------------------------------------------

;; move window
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-h") 'windmove-left)

(define-key global-map (kbd "RET") 'newline-and-indent)

;============================================================
; Manually Handled Packages
;============================================================
;--------------------------------------------------
; Package installed
;--------------------------------------------------
; 1. eim -- Chinese wubi input method

;--------------------------------------------------
; Global key bindings
;--------------------------------------------------
;; use EIM
(add-to-list 'load-path "~/.emacs.d/elisp/eim")
(autoload 'eim-use-package "eim" "Another emacs input method")
;; Tooltip 暂时还不好用
(setq eim-use-tooltip nil)

(register-input-method
 "eim-wb" "euc-cn" 'eim-use-package
 "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")
(register-input-method
 "eim-wbpy" "euc-cn" 'eim-use-package
 "五笔拼音" "汉字五笔拼音输入法" "wbpy.txt")
(setq default-input-method "eim-wb")

;; 防止EIM直接上词
(eval-after-load "eim-wb"
  '(progn
    (eim-set-option 'max-length 8)
    ))
(add-hook 'eim-active-hook
	  '(lambda ()
	     (setq eim-page-length 7)))

;; 用 ; 暂时输入英文
(require 'eim-extra)
(global-set-key ";" 'eim-insert-ascii)

;--------------------------------------------------
; color scheme
;--------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/colors")
(load-theme 'zenburn t)
;(load-theme 'wilson t)

;============================================================
; Emacs package management
;============================================================
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
			   ))
  (package-initialize))

;--------------------------------------------------
; Package installed
;--------------------------------------------------
; 1. evil -- Extensible Vi layer for Emacs
;    - evil-leader -- <leader> support for evil
;    - evil-nerd-commenter -- Nerd-Commenter port
;    - evil-tabs -- Tabs support (need elscreen)
;    - evil-visualstar -- '*' command for visual selection
;    - sourrond -- port of vim 'surround' plugin
;    - ace-jump-mode -- Clone of vim 'EasyMotion' plugin
; 2. smex -- M-x interface with IDo-style fuzzy matching
; 3. multi-eshell -- enhance
; 4. magit -- work with 'git'
; --- 5. geiser -- like slime but for scheme ---
; 5. racket-mode -- major mode for racket
; 6. auto-complete -- auto completion
; 8. yasnippet-bundle
;
; 9. virtualenvwrapper -- for python virtualenv
; 10. elpy -- python IDE for emacs
; 11. auctex -- latex for emacs
; 12. neotree -- NERD-tree for emacs

(defvar prelude-packages
  '(evil evil-leader evil-nerd-commenter evil-matchit
	 evil-visualstar evil-surround ace-jump-mode
	 evil-numbers evil-search-highlight-persist
	 paredit evil-paredit
	 org evil-org
	 helm helm-projectile
	 exec-path-from-shell
	 ;smex persp-mode
         neotree expand-region evil-iedit-state
	 multi-term multi-eshell yasnippet magit
	 company ;irony ;company-irony ; irony for clang support
	 ;auto-complete auto-complete-clang-async
	 ;virtualenvwrapper ;elpy auctex
	 markdown-mode htmlize
         emacs-eclim
	 ;geiser ;racket-mode
         emmet-mode ; for editing html/xml

	 ; clojure[script] related.
	 cider
	 )
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package
  ;;
  ;;versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;(global-nlinum-mode)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 1. evil -- emulation of vim
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(autoload 'evil-local-mode "evil")
(global-evil-leader-mode) ; should comes before 'evil'
(evil-mode 1)

; Unbind keys, use emacs navigation keys for insert mode
(defun evil-undefine ()
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))

(eval-after-load "evil"
  '(progn
     ;; load additional modes
     (global-evil-matchit-mode 1)
     (global-evil-visualstar-mode 1)

     ;; load additional packages
     (require 'expand-region)
     (require 'evil-iedit-state)
     (define-key evil-iedit-state-map (kbd "TAB") 'iedit-toggle-selection)

     (define-key evil-insert-state-map (kbd "C-d") 'evil-undefine)
     (define-key evil-insert-state-map (kbd "C-e") 'evil-undefine)
     (define-key evil-insert-state-map (kbd "C-k") 'evil-undefine)

     (define-key evil-insert-state-map (kbd "C-y") 'evil-undefine)
     (define-key evil-normal-state-map (kbd "[b") 'evil-prev-buffer)
     (define-key evil-normal-state-map (kbd "]b") 'evil-next-buffer)
     (customize-set-variable 'evil-toggle-key "C-`")

     ;; expand-region shortcuts
     (define-key evil-normal-state-map (kbd "+") 'er/expand-region)
     (define-key evil-visual-state-map (kbd "+") 'er/expand-region)
     (define-key evil-visual-state-map (kbd "_") 'er/contract-region)

     ;; evil-iedit-state keybindings
     (define-key evil-normal-state-map (kbd "C-e") 'evil-iedit-state/iedit-mode)
     (define-key evil-visual-state-map (kbd "C-e") 'evil-iedit-state/iedit-mode)
     ))

; default evil state for some major modes
(setq evil-emacs-state-modes
      (append evil-emacs-state-modes
              '(geiser-repl-mode cider-repl-mode cider-stacktrace-mode
                                 eclim-project-mode)))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 2. evil-leader -- simulate evil <leader> feature
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(evil-leader/set-leader ",")

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 3. ace-jump -- i.e easymotion for vim
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 4. ibuffer settings
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; (defalias 'list-buffers 'ibuffer)
;;; persp-mode
;; (defalias 'list-buffers 'ibuffer)
;(iswitchb-mode 1)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 5. evil plugins
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; provide local buffer list for elscreen
;; (require 'jg-elscreen-buffer-list)

; evil-nerd-commenter
(evilnc-default-hotkeys)

; evil-visualstar
(require 'evil-visualstar)

; evil-number -- add C-a C-x for vim normal mode
;(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

; evil-paredit && paredit
(autoload 'evil-paredit-mode "evil-paredit")

; evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;;; evil-search-highlight-persist
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

(with-eval-after-load "evil-search-highlight-persist"
  (define-key global-map (kbd "C-l")
      '(lambda ()
  	 (interactive)
  	 (evil-search-highlight-persist-remove-all)
  	 (redraw-frame)))
  ;; (let ((orig-c-l (key-binding (kbd "C-l"))))
  ;;   (define-key global-map (kbd "C-l")
  ;;     `(lambda () (interactive)
  ;; 	 (evil-search-highlight-persist-remove-all)
  ;; 	 (,orig-c-l))))
  )

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'evil-paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'evil-paredit-mode)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 6. Extra vim configurations -- mappings
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; set keys for ace-jump
(evil-leader/set-key
  "f" 'ace-jump-char-mode
  "w" 'ace-jump-word-mode
  "l" 'ace-jump-line-mode)

(evil-leader/set-key "<SPC>" 'delete-trailing-whitespace)
(evil-leader/set-key "," 'evilnc-comment-operator)

; remove ctrl-m
(evil-leader/set-key "m"
  '(lambda () (interactive) (region-replace "" "")))

; collapse blank lines
(evil-leader/set-key "<RET>"
  '(lambda () (interactive) (region-replace "^\n\\{2,\\}" "\n")))

(defun evil-paste-select ()
  (interactive)
  (let ((begin (nth 3 evil-last-paste))
	(end (- (nth 4 evil-last-paste) 1)))
    (evil-visual-select begin end)))
(define-key evil-normal-state-map "gp" 'evil-paste-select)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 7. smex
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; From Emacs Wiki
; have smex call 'smex-initialize' when it's needed.
;; (global-set-key [(meta x)]
;; 		(lambda ()
;; 		  (interactive)
;; 		  (or (boundp 'smex-cache)
;; 		      (smex-initialize))
;; 		  (global-set-key [(meta x)] 'smex)
;; 		  (smex)))

;; (global-set-key [(shift meta x)]
;; 		(lambda ()
;; 		  (interactive)
;; 		  (or (boundp 'smex-cache)
;; 		      (smex-initialize))
;; 		  (global-set-key [(shift meta x)] 'smex-major-mode-commands)
;; 		  (smex-major-mode-commands)))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 8. persp-mode/ perspective
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil) ;; switch off animation of restoring window configuration
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
(eval-after-load "persp-mode"
  '(progn
     (customize-set-variable 'persp-nil-name "main")
     (customize-set-variable 'persp-auto-save-opt 0) ; disable auto-save
     (customize-set-variable 'persp-auto-resume-time 0)	; disable auto-resume
     (define-key persp-mode-map (kbd "C-z c") 'persp-switch)
     (define-key persp-mode-map (kbd "C-z n") 'persp-switch)
     (define-key persp-mode-map (kbd "C-z p") 'persp-switch)
     (define-key persp-mode-map (kbd "C-z r") 'persp-rename)
     (define-key persp-mode-map (kbd "C-z k") 'persp-kill)
     (define-key persp-mode-map (kbd "C-z a") 'persp-add-buffer)
     (define-key persp-mode-map (kbd "C-z t") 'persp-temporarily-display-buffer)
     (define-key persp-mode-map (kbd "C-z i") 'persp-import-buffers)
     (define-key persp-mode-map (kbd "C-z q") 'persp-remove-buffer)
     (define-key persp-mode-map (kbd "C-z w") 'persp-save-state-to-file)
     (define-key persp-mode-map (kbd "C-z l") 'persp-load-state-from-file)
     ))

;;; ibuffer support
(defalias 'list-buffers
  '(lambda (arg)
     (interactive "P")
     (with-persp-buffer-list () (ibuffer))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Neo Tree -- NERD-tree for emacs
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun neotree-expand-node-descendants (&optional arg)
  "Expand the line under the cursor and all descendants.
Optional argument ARG indicates that any cache should be flushed."
  (interactive "P")
  (let ((full-path (if arg arg (neo-buffer--get-filename-current-line))))
    (when (file-directory-p full-path)
      (neo-buffer--set-expand full-path t)
      ; recursive expand the nodes;
      (dolist (node (car (neo-buffer--get-nodes full-path)))
        (neotree-expand-node-descendants node)))))

(defun neotree-open-directory-recursively (&optional arg)
  "Expand a directory recursively"
  (interactive "P")
  (neotree-expand-node-descendants arg)
  (neo-buffer--refresh t))

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "j") 'neotree-next-line)
            (define-key evil-normal-state-local-map (kbd "k") 'neotree-previous-line)
            (define-key evil-normal-state-local-map (kbd "M") 'neotree-create-node)
            (define-key evil-normal-state-local-map (kbd "R") 'neotree-rename-node)
            (define-key evil-normal-state-local-map (kbd "D") 'neotree-delete-node)
            (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "C") 'neotree-change-root)
            (define-key evil-normal-state-local-map (kbd "U") 'neotree-change-root)
	    (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
            (define-key evil-normal-state-local-map (kbd "O") 'neotree-open-directory-recursively)))

(evil-leader/set-key "ne" 'neotree-toggle)
(global-set-key (kbd "<f8>") 'neotree-toggle)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; exec-path-from-shell: fix shell path on MAC
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 8. multi-eshell
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(autoload 'multi-eshell "multi-eshell")
(setq multi-eshell-shell-function '(eshell))
(setq multi-eshell-name "*eshell*")

; shell completion
(defun last-eshell-buffer (l)
  "Return most recently used eshell buffer."
  (when l
    (if (eq 'eshell-mode (with-current-buffer (car l) major-mode))
	(car l) (last-eshell-buffer (cdr l)))))

(defun get-eshell()
  "Switch to the eshell buffer last used, or create a new one if non exists, or if the current buffer is already a eshell"
  (interactive)
  (let ((b (last-eshell-buffer
	    (if (fboundp 'with-persp-buffer-list)
		(with-persp-buffer-list () (buffer-list))
	      (buffer-list)))))
    (if (or (not b) (eq 'eshell-mode major-mode))
	(progn (multi-eshell 1)
	       (when (fboundp 'persp-add-buffer)
		 (persp-add-buffer (current-buffer))))
      (switch-to-buffer b))))

(global-set-key (kbd "C-c e") 'get-eshell)
; add some key binding
(add-hook 'eshell-mode-hook
	  '(lambda()
	     (local-set-key "\C-cn" 'multi-eshell-switch-to-next-live-shell)
	     (local-set-key "\C-cp" 'multi-eshell-switch)
	     ))

;;; load customized eshell functions.
(require 'eshell-functions)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 9. multi-term
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(autoload 'multi-term "multi-term")
(setq multi-term-program "/bin/bash")
(setq term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-y" "M-x" "M-:"))
; setting some key bindings
(eval-after-load "multi-term"
  '(progn
     (add-to-list 'term-bind-key-alist '("C-c n" . multi-term-next))
     (add-to-list 'term-bind-key-alist '("C-c p" . multi-term-prev))
     ))

; set background color and foreground color to fit color-theme dark-blue2
;(setq term-default-bg-color "#233b5a")
;(setq term-default-fg-color "#fff8dc")

(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
	(car l)(last-term-buffer (cdr l)))))

(defun get-term()
  "Switch to the term buffer last used, or create a new one if non exists, or if the current buffer is already a term"
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
	(multi-term)
      (switch-to-buffer b))))

(defun get-term()
  "Switch to the term buffer last used, or create a new one if non exists, or if the current buffer is already a term"
  (interactive)
  (let ((b (last-term-buffer
	    (if (fboundp 'with-persp-buffer-list)
		(with-persp-buffer-list () (buffer-list))
	      (buffer-list)))))
    (if (or (not b) (eq 'term-mode major-mode))
	(progn (multi-term)
	       (when (fboundp 'persp-add-buffer)
		 (persp-add-buffer (current-buffer))))
      (switch-to-buffer b))))

(global-set-key (kbd "C-c t") 'get-term)
;(global-set-key (kbd "C-c n") 'multi-term-next)
;(global-set-key (kbd "C-c p") 'multi-term-prev)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 10. geiser (racket-mode)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; add key binding
(add-hook 'geiser-mode-hook
	  (lambda ()
	    ;(define-key geiser-mode-map (kbd "C-c C-c") 'geiser-eval-definition)
	    ;; (ac-geiser-setup)
	    ))

; fix 'Text Read-only' bug for manually defined REPL
(setq geiser-repl-read-only-prompt-p nil)


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 11. yasnippet (should come before auto-complete)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'yasnippet)
(yas-global-mode 1)

(defun yas-escape ()
  (interactive)
  (yas-abort-snippet)
  (evil-force-normal-state))


(eval-after-load "yasnippet"
  '(progn
     ;;; disable `yas-expand` binding to "TAB" key. trigger by company mode
     ;;; instead.
     (define-key yas-minor-mode-map (kbd "TAB") nil)

     (define-key yas-keymap (kbd "C-j") 'yas-next-field-or-maybe-expand)
     (define-key yas-keymap (kbd "C-k") 'yas-prev-field)
     (define-key yas-keymap (kbd "<escape>") 'yas-escape)
     ;; (define-key yas-keymap (kbd "TAB") 'yas-next-field-or-maybe-expand)
     (define-key yas-keymap (kbd "TAB") nil)

     (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))
     ))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; [X] 12. auto-complete
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ;use 'https://github.com/capitaomorte/auto-complete' insted
;; (add-to-list 'load-path "~/.emacs.d/elpa/auto-complete")
;; (require 'auto-complete-config)
;; (setq-default ac-sources '(ac-source-yasnippet
;; 			    ;ac-source-filename
;; 			    ;ac-source-words-in-all-buffer
;; 			    ac-source-abbrev
;; 			    ;ac-source-words-in-same-mode-buffers
;; 			    ac-source-dictionary))

;; ;(ac-config-default)
;; (global-auto-complete-mode t)
;; (add-hook 'auto-complete-mode-hook
;; 	  '(lambda ()
;; 	     (define-key ac-completing-map (kbd "RET") 'ac-complete)
;; 	     (define-key-with-fallback ac-completing-map
;; 	       (kbd "C-j")
;; 	       yas-next-field
;; 	       yas-triggers-in-field)))

;; ;;; set the trigger key so that it can work together with yasnippet on
;; ;;; tab key, if the word exists in yasnippet, pressing tab will cause
;; ;;; yasnippet to activate, otherwise, auto-complete will
;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")

;; (ac-linum-workaround)



;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 13. Company -- a good alternative of auto-complete
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; disable company mode for below major modes.
(setq company-global-modes '(not eshell-mode term-mode shell-mode))
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load "company-template"
  '(progn
     (define-key company-template-nav-map (kbd "C-j") 'company-template-forward-field)
     (define-key company-template-nav-map [tab] nil)
     (define-key company-template-nav-map "<tab>" nil)
     (define-key company-template-nav-map (kbd "TAB") nil)))

;;; interfere yasnippet with company mode
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key "\t" 'tab-indent-or-complete)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 14. virtualenvwrapper
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; currently, I have no use of python.
;; (autoload 'virtualenvwrapper "virtualenvwrapper")
;; (venv-initialize-interactive-shells) ;; if you want interactive shell support
;; (venv-initialize-eshell) ;; if you want eshell support
;; (setq venv-location '("~/localenv"))

;; (add-hook 'python-mode-hook (lambda ()
;;                               (venv-workon "localenv")))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 15. elpy
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;(elpy-enable)
;(elpy-use-ipython)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 16. auctex
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Set default viewer for different output
;; (defun set-default-view-program ()
;;    (setq TeX-view-program-list
;; 	 '(("SumatraPDF" "SumatraPDF.exe %o")
;; 	   ("Gsview" "gsview32.exe %o")
;; 	   ("Okular" "okular --unique %o")
;; 	   ("Acroread" "acroread %o")
;; 	   ("Evince" "evince %o")
;; 	   ("Firefox" "firefox %o")
;; 	   ("Zathura" "zathura %o")))
;;    (setq TeX-view-program-selection
;; 	 '((output-pdf "Okular")
;; 	   (output-dvi "Okular"))))
;; (add-hook 'LaTeX-mode-hook 'set-default-view-program)
;; (add-hook 'TeX-mode-hook 'set-default-view-program)
(with-eval-after-load "tex"
  (setq TeX-view-program-list
        '(("SumatraPDF" "SumatraPDF.exe %o")
          ("Gsview" "gsview32.exe %o")
          ("Okular" "okular --unique %o")
          ("Acroread" "acroread %o")
          ("Evince" "evince %o")
          ("Firefox" "firefox %o")
          ("Zathura" "zathura %o")))
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil
                 (latex-mode doctex-mode)))
  (add-to-list 'TeX-command-list
               '("XeTeX" "%`xetex%(mode)%' %t" TeX-run-TeX nil
                 (plain-tex-mode ams-tex-mode texinfo-mode)))
  (setq TeX-view-program-selection
        '((output-pdf "Okular")
          (output-dvi "Okular"))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 17. irony -- libclang support
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; irony support

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(when (package-installed-p 'irony)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 18. emacs-clang-complete-async
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ;(require 'auto-complete-clang-async)
;; (autoload 'ac-clang-launch-completion-process  "auto-complete-clang-async")

;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "~/.emacs.d/emacs-clang-complete-async/clang-complete")
;;   ;(setq ac-sources '(ac-source-clang-async ac-source-yasnippet))
;;   (setq ac-sources '(ac-source-clang-async))
;;   (ac-clang-launch-completion-process)
;; )

;; (defun my-ac-config ()
;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup))

;; (my-ac-config)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 2. paredit mode
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; enabled by evil-paredit

; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
; (add-hook 'racket-mode-hook           #'enable-paredit-mode)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 20. fill column indicator
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(when (package-installed-p 'fill-column-indicator)
  (setq-default fill-column 80)
  (setq fci-rule-character-color "red"))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 21. emmet-mode
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(when (package-installed-p 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  )

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 22. helm
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'helm-config)

(when (package-installed-p 'helm)
  ;; change default prefix key
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  ;; helm-M-x
  (setq helm-M-x-fuzzy-match t)
  (global-set-key (kbd "M-x") 'helm-M-x)

  ;; helm-kill-ring
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;; helm-mini
  (global-set-key (kbd "C-x b") 'helm-mini)
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t)

  ;; helm-find-files
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  )

;;; -----------------------------
;;; helm-projectile
(when (package-installed-p 'helm-projectile)
  (projectile-global-mode)
  (helm-projectile-on)
  )

;============================================================
; Filetype specified configuration
;============================================================

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 1. c mode
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (c-set-style "K&R")
	     (setq c-basic-offset 4)))

; Add cscope support
(add-to-list 'load-path "~/.emacs.d/elisp/xcscope")
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (require 'xcscope)
	     (cscope-setup)
	     ))
(add-hook 'cscope-list-entry-hook 'evil-emacs-state)

;; (define-key evil-normal-state-map (kbd "C-\\") 'cscope-find-global-definition-no-prompting)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 2. python mode
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ;;; disable company mode because we use elpy instead.
;; (add-hook 'python-mode-hook
;; 	  '(lambda ()
;; 	     (company-mode 0)))

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 2. emacs-lisp mode
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; (add-hook 'emacs-lisp-mode-hook
;; 	  'ac-emacs-lisp-mode-setup)

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 3. java mode (eclim)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun company-emacs-eclim-local ()
  (make-local-variable 'company-backends)
  (company-emacs-eclim-setup))
(when (package-installed-p 'emacs-eclim)
  (require 'eclim)
  (global-eclim-mode)
  (require 'company-emacs-eclim)
  (add-hook 'java-mode-hook
            'company-emacs-eclim-local))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 4. org mode
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(setq org-directory "~/org")

;;; load org wiki settings (not loading this if on windows)
(when (not (eq system-type 'windows-nt))
  (add-hook 'after-init-hook (lambda() (require 'org-projects))))

(with-eval-after-load "org"
  ;;; evil-org-mode
  (require 'evil-org))

; Enable literal links
(defun turn-on-literal-links ()
  "enable literal links."
  (interactive)
  (org-remove-from-invisibility-spec '(org-link))
  (org-restart-font-lock))

(add-hook 'org-mode-hook 'turn-on-literal-links)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)

;;; settings for org mode capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;;; test capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/newgtd.org" "Tasks")
         "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
	("l" "Link" plain (file (concat org-directory "/links.org"))
         "- %?\n %x\n")
	("n" "Note" entry (file (concat org-directory "/notes.org"))
         "**  %?\n%x\nAdded:%U\n")))

(setq org-refile-targets '(("newgtd.org" :maxlevel . 1)
			   ("someday.org" :level . 1)
                           ("done.org" :level . 1)))


;;; setup agenda
(setq org-agenda-files `(,(concat org-directory "/newgtd.org")))
(setq org-agenda-custom-commands
      '(("H" "Office and Home Lists"
	 ((agenda)
	  (tags-todo "OFFICE")
	  (tags-todo "HOME")
	  (tags-todo "COMPUTER")
	  (tags-todo "DVD")
	  (tags-todo "READING")))
	("D" "Daily Action List"
	 ((agenda "" ((org-agenda-ndays 1)
		      (org-agenda-sorting-strategy
		       '((agenda time-up priority-down tag-up)))
		      (org-deadline-warning-days 0)))))))

;;; babel settings
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (python . t)
   (C . t)
   (awk . t)
   (ditaa . t)
   (gnuplot . t)
   (latex . t)
   (matlab . t)
   (scheme . t)))

;;; auto-complete: company support
(defun add-pcomplete-to-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook 'add-pcomplete-to-capf)

;============================================================
; Additional Hacks
;============================================================

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 1. Enable copy-paste with X clipboard (need xsel)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
 (when (getenv "DISPLAY")
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      ;; I prefer using the "clipboard" selection (the one the
      ;; typically is used by c-c/c-v) before the primary selection
      ;; (that uses mouse-select/middle-button-click)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
	xsel-output )))
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
  ;; Idea from
  ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
  ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
 ))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 2. Change the looking of modeline
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; use powerline now, and customized theme for vim.
(when (package-installed-p 'powerline)
  (require 'powerline)
  (require 'powerline-evil-themes)
  (powerline-evil-theme))

;============================================================
; Customizing functions and commands
;============================================================

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; git-grep, use it on git repos will fasten the search
; 'M-x git-grep' to invoke

(defcustom git-grep-command "git --no-pager grep --no-color --line-number <C> <R>"
  "The command to run with M-x git-grep.")

(defun git-grep (regexp)
  "Search for the given regexp using `git grep' in the current directory."
  (interactive "sRegexp: ")
  (unless (boundp 'grep-find-template) (grep-compute-defaults))
  (let ((old-command grep-find-template))
    (grep-apply-setting 'grep-find-template git-grep-command)
    (rgrep regexp "*" "")
    (grep-apply-setting 'grep-find-template old-command)))

;============================================================
; Settings by Emacs Groups
;============================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake company-capf
		  (company-dabbrev-code company-gtags company-etags company-keywords)
		  company-oddmuse company-files)))
 '(eclim-eclipse-dirs
   (quote
    ("/Applications/eclipse" "/usr/lib/eclipse" "/usr/local/lib/eclipse" "/usr/share/eclipse" "/opt/eclipse-java")))
 '(evil-toggle-key "C-`")
 '(fci-rule-color "#383838")
 '(global-eclim-mode t)
 '(inhibit-startup-screen t)
 '(ns-command-modifier (quote meta) t)
 '(persp-auto-resume-time 0)
 '(persp-auto-save-opt 0)
 '(persp-nil-name "main")
 '(scheme-program-name "racket"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-search-highlight-persist-highlight-face ((t (:background "red"))))
 '(iedit-occurrence ((t (:inherit region)))))
(put 'dired-find-alternate-file 'disabled nil)
