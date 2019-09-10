;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq gc-cons-threshold 50000000)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
(tool-bar-mode -1)
(column-number-mode 1)
;; (display-time-mode 1)
;; (scroll-bar-mode -1)
;; (set-face-attribute 'default nil :height 160)
(delete-selection-mode t)


(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)
(size-indication-mode t)
(setq-default cursor-type 'bar)
(set-cursor-color "#595959")

(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-enable-flex-matching t)


(use-package gruvbox-theme
  :ensure t
  :defer t
  :config (load-theme 'gruvbox-dark-soft))

(use-package proof-general
  :no-require t
  :ensure t
  :defer t)

(use-package which-key
  :defer t
  :ensure t
  :config
  (which-key-mode +1))

(use-package company-coq
  :ensure t
  :defer t
  :init (add-hook 'coq-mode-hook #'company-coq-mode))

(winner-mode 1)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(use-package ace-window
  :ensure t
  :defer t
  :init (progn (global-set-key [remap other-window] 'ace-window)))

(use-package beacon
  :ensure t
  :defer t
  :init (beacon-mode 1))

(use-package esup
  :ensure t
  :defer t
  :config (autoload 'esup "esup" "Emacs Start Up Profiler." nil))


(defun enable-gruvbox () (interactive) (load-theme 'gruvbox-dark-soft t))
(defun disable-gruvbox () (interactive) (disable-theme 'gruvbox-dark-soft))

(global-set-key (kbd "\e\et") (emacs-init-time))
(global-set-key (kbd "\e\es") 'scroll-bar-mode)
(global-set-key (kbd "\e\ec") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "<f6>") 'enable-gruvbox)
(global-set-key (kbd "<f7>") 'disable-gruvbox)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(setq org-html-postamble nil)
(use-package org-bullets
  :ensure t
  :defer t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package yasnippet
  :ensure t
  :defer t
  :init (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :defer t)

(use-package smartparens-config
  :ensure smartparens
  :defer t
  :config
  (progn
    (show-smartparens-global-mode t)))
(add-hook 'prog-mode-hook #'smartparens-mode)


(use-package rainbow-delimiters
  :ensure t
  :defer t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package multiple-cursors
  :ensure t
  :defer t)

(use-package exec-path-from-shell
  :ensure  t
  :config (exec-path-from-shell-initialize))

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(use-package sml-mode
  :ensure t
  :mode (("\\.sml\\'" . sml-mode))
  :config
  (progn
    (autoload 'sml-mode  "sml-mode" "Mode for editing SML." t)
    (setq exec-path (cons "/usr/local/smlnj/bin"  exec-path))
    (setq sml-program-name "sml")))

(use-package racket-mode
  :ensure t
  :config
   (progn
    (setq racket-program "/Applications/Racket v7.4/bin/racket")
    (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)))


(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(use-package auctex
  :ensure t
  :defer t
  :config
  (progn
    (setq TeX-engine 'xetex)
    (setq latex-run-command "xelatex")
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
    (setq exec-path (append exec-path '("/Library/TeX/texbin/")))))


(defvar acl2-skip-shell nil)
(setq acl2-skip-shell t)
;; (load "emacs-acl2")
;; (load "send-form")


;; ------------------------------


