;;; Code:
;; -------------------- Startup Customization --------------------
;;; Commentary:
;; removes the menu bar (the apple thing), but I don't
;; wanna remove this because it helps get an overview for
;; whatever mode you're in + I have it hidden on my mac
;; unless  you hover over it.  So we're good.
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;; cleaning up the title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;; no backup  file creation
(setq make-backup-files nil)

;; don't need the toolbar - removing this
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))


;; I actually need the scrollbar because this helps get a
;; view of the buffer's length. I plan to remove this and
;; get the mini map scroll thing from aaron bieber's config!
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; inhibit startup message
(setq inhibit-startup-message t)


;; full screen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; open todo at startup
(setq initial-buffer-choice "~/Documents/todo/todo.org")


;; removing the scroll bar
(scroll-bar-mode -1)

;; ----------------------------------------



(package-initialize)


;; obligatory melpa initialization
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; add garbage collection later
;; read about it first


;; give this out
(provide 'init)

;; a themes folder is necessary
;;  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


;; -------------------- Package Setup machinery --------------------


(setq package-list
      '(use-package ivy counsel swiper racket-mode))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)


;; -------------------- sml mode --------------------

(use-package sml-mode
  :ensure t
  :defer t)

(autoload 'sml-mode  "sml-mode" "Mode for editing SML." t)
(setenv "PATH" (concat "/usr/local/Cellar/smlnj/110.84/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/local/Cellar/smlnj/110.84/bin"  exec-path))
(setq sml-program-name "sml")
(global-font-lock-mode 1)

;; -------------------- smart parens --------------------

(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (show-smartparens-global-mode t)))

(add-hook 'prog-mode-hook #'smartparens-mode)

(defun nuke-all-buffers ()
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  (delete-other-windows))


;; -------------------- Org mode settings --------------------

;; remove the org footer details. 
(setq org-html-postamble nil)

;; -------------------- Org bullet --------------------

;; start with simple org bullet mode
(use-package org-bullets
  :ensure t
  :defer t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))


;; -------------------- Key Bindings --------------------

;; escape escape c opens the init file.
(global-set-key (kbd "\e\ec")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(fset 'yes-or-no-p 'y-or-n-p)

;; -------------------- Mode line customizations --------------------

;; add a clock in modeline
(display-time-mode 1)
                                                                              
;; add column number in modeline
(column-number-mode 1)


;; -------------------- Ivy, Counsel, Swiper --------------------

;; (use-package ivy :demand
;;   :bind
;;   :ensure t
;;   :config
;;   (ivy-mode 1))

;; (use-package counsel
;;   :ensure t)


;; (use-package swiper
;;   :ensure t
;;   :config
;;   (progn (global-set-key "\C-s" 'swiper)))

;; -------------------- Racket mode --------------------

(use-package racket-mode
  :ensure t
  :config (setq racket-program "/usr/local/bin/racket"))


;; -------------------- Cpp tramp environment --------------------

(defun cppenv ()
  (interactive)
  (let ((default-directory "/ssh:husky1999@login.ccs.neu.edu:"))
    (shell)))


;; ------------------ ACL2 -----------------------------------------------------+

(defvar acl2-skip-shell nil)
(setq acl2-skip-shell t)

;; pete's sent form shortcuts
(load "/Users/atharvashukla/Documents/acl2s/send-form.lisp")

;; setting the acl2s stuff
(load "/Users/atharvashukla/Documents/acl2-sources/emacs/emacs-acl2.el")

;; -------------------- Font --------------------

;; bigger font on startup
(set-face-attribute 'default nil :height 200)

; (set-frame-font "PxPlus IBM VGA9" nil t)



;; -------------------- Which Key --------------------

(use-package which-key
  :ensure t
  :defer 2
  :config
  (which-key-mode))


 ;; -------------------- Latex --------------------

(use-package auctex
  :ensure t
  :defer t
  :config
  (setq TeX-engine 'xetex)
  (setq latex-run-command "xelatex"))

(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))

;; -------------------- HELM --------------------


(use-package helm
  :ensure t)


; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action

(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; (global-set-key (kbd "C-x C-m") 'helm-M-x)
;; (global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
;; (global-set-key (kbd "C-h C-l") 'helm-locate-library)

;; (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(global-set-key (kbd "C-c h o") 'helm-occur)

;; (helm-linum-relative-mode 1)			       


;; (setq helm-split-window-in-side-p           t
;;       helm-buffers-fuzzy-matching           t
;;       helm-move-to-line-cycle-in-source     t
;;       helm-ff-search-library-in-sexp        t
;;       helm-ff-file-name-history-use-recentf t)

;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"
;;		       ))

;(define-key helm-command-map (kbd "o")     'helm-occur)
;(define-key helm-command-map (kbd "g")     'helm-do-grep)
;(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
;(define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)


;; -------------------- Yasnippet --------------------

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)


;; -------------------- Flycheck --------------------

 ;;flycheck

(use-package flycheck
:ensure t
:init (global-flycheck-mode t))


;; -------------------- God mode --------------------

;; (use-package god-mode  
;;   :ensure t)

;; (global-set-key (kbd "<escape>") 'god-local-mode)
;; escape is not a good idea actually ^


;; -------------------- EVIL --------------------
;; not ready yet

;; -------------------- AVY --------------------
;; (use-package avy
;;   :ensure t
;;   :bind ("M-s" . avy-goto-char))


;; -------------------- Company mode --------------------

(use-package company
  :ensure t
  :defer t
  :config (require 'company)
  
  (add-hook 'after-init-hook 'global-company-mode))



;; todo: 1) minimap, 2) 

;; -------------------- Custom --------------------

(put 'downcase-region 'disabled nil)

;; -------------------- THEME(s) --------------------

;; (use-package zenburn-theme
;;   :ensure t)

;; (use-package gruvbox-theme
;;  :ensure t
;;  :config (load-theme 'gruvbox-dark-medium))

;; (use-package monokai-theme
;;   :ensure t)



;; had buffer-move here

;;; init.el ends here



;;;; -------
