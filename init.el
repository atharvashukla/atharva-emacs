;; activate the packages
(package-initialize)
;; obligatory melpa initialization
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; install the missing packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; use-package necessary
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default)))
 '(package-selected-packages
   (quote
    (ace-window esup ## gruvbox-theme smartparens yasnippet-snippets yasnippet org-bullets auctex sml-mode racket-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; don't show the startup message
(setq inhibit-startup-message t)
;; maximize the emacs  window to full screen on startup
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; make the default font size bigger on startup
;; (including the modeline)
;; (set-face-attribute 'default nil :height 200)
;; open the "todo.org" file on startup to show Tasks 
;; (setq initial-buffer-choice "~/Documents/todo/todo.org")
;; make the titlebar transparent (so it looks invisible / matches
;; the color of the theme)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))
;; remove the icon on the title bar (cleaner)
(setq ns-use-proxy-icon nil)
;; remove the title from the title bar (even cleaner)
(setq frame-title-format nil)
;; remove the big toolbar 
(tool-bar-mode -1)
;; show me the column numbers
;; good for making comment lines as long as the heading
;; using a prefix argument of "column" length
(column-number-mode 1)
;; the following ui settings are time consuming.. investigate workarounds
;; and also try to "defer" them ontp later activation
;; (display-time-mode 1) <<- this is taking 0.002 sec.. why?
;; (scroll-bar-mode -1) 0.042sec!?


(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key
 (kbd "\e\ec")
 (lambda () (interactive)
   (find-file "~/.emacs.d/init.el"))) ;; <-- this is where my init is located

(use-package racket-mode
  :ensure t
  :config (setq racket-program "/usr/local/bin/racket")) ;; <--- my racket path
(add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)

(defvar acl2-skip-shell nil)
(setq acl2-skip-shell t)
;; FOR anyone using my init, these are paths to:
;; ... send-form is Pete's file for nice emacs bindings for acl2 mode
(load "/Users/atharvashukla/Documents/acl2/books/acl2s/send-form.lisp")
;; this is distributed with acl2:
(load "/Users/atharvashukla/Documents/acl2/emacs/emacs-acl2.el")

(use-package sml-mode
  :ensure t
  :defer t)
(autoload 'sml-mode  "sml-mode" "Mode for editing SML." t)
;; path to my sml installation (using homebrew)
(setq exec-path (cons "/usr/local/Cellar/smlnj/110.84/bin"  exec-path))
(setq sml-program-name "sml")
(global-font-lock-mode 1)

;; (use-package auctex
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq TeX-engine 'xetex)
;;   (setq latex-run-command "xelatex"))
;; (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
;; (setq exec-path (append exec-path '("/Library/TeX/texbin/")))

(setq org-html-postamble nil)
(use-package org-bullets
  :ensure t
  :defer t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (show-smartparens-global-mode t)))
(add-hook 'prog-mode-hook #'smartparens-mode)


;; gruvbox theme, had to comment
;; out the latest neoframe stuff (bug)
;; see github bug filing for updates
;; https://github.com/greduan/emacs-theme-gruvbox/issues/133
;; comment out the neotree stuff
(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox-dark-soft))


;; IDO mode: Interactively DO things
;; comes prepacked with emacs

(setq ido-everywhere t)
(ido-mode 1)

;; guesses the context -- see `ffap'
(setq ido-use-filename-at-point 'guess)
;; more flexible matching...
;; items that simply contain all chars
;; in the specified sequence will match
(setq ido-enable-flex-matching t)


;; emacs startup profiler
(use-package esup
  :ensure t
  :config (autoload 'esup "esup" "Emacs Start Up Profiler." nil))

;; +-----------------------+
;; | WINDOW CONFIGURATIONS |
;; +-----------------------+

;; Guide:
;;
;; winner-mode: C-c left (undo)
;;              C-c right (redo
;; wind-move: Shift-<Arrow Key>
;; ace-window C-o <num>


;; WINNER MODE
;; ===========
;;> undo window stuff
;; comes prepacked with emacs.
;; The ‘fboundp’ test is for those XEmacs
;; installations that don’t have winner-mode available.
;;  getting back to a delicate WindowConfiguration is
;; difficult. This is where Winner Mode comes in:
;; With it, going back to a previous session is easy
;; (when (fboundp 'winner-mode)
(winner-mode 1)
;;  )


;; WIND MOVE
;; =========
;; move  from window to window using Shift and the arrow keys.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; ACE WINDOW
;; ==========
;; assigns a number to each window so that you
;; can jump to it by C-o <num> (instead of repeated
;; C-o ... which is annoying)
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)))



;; -----------

;; end of init.el
