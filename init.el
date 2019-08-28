;; manually add sml + acl2 files in lisp, emacs dirs


;; activate the packages
(package-initialize)
;; obligatory melpa initialization
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; fetch the list of packages available

;; (unless package-archive-contents
;;   (package-refresh-contents))

;; install the missing packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auctex multiple-cursors rainbow-delimiters exec-path-from-shell esup gruvbox-theme smartparens yasnippet-snippets yasnippet org-bullets beacon ace-window racket-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; -----------------------------------------------------------------------------



;; don't show the startup message
(setq inhibit-startup-message t)
;; maximize the emacs  window to full screen on startup
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; make the default font size bigger on startup
;; (including the modelinpe)
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
;; (scroll-bar-mode -1) ;; 0.042sec!?
;; setting to a key command to do it manually when needed
(global-set-key (kbd "\e\es") 'scroll-bar-mode)

;; When Delete Selection mode is enabled, typed text replaces the
;; selection if the selection is active. Otherwise, typed text
;; is just inserted at point regardless of any selection.
(delete-selection-mode 1)

;; no irritating scratch message
(setq initial-scratch-message "")

;; -----------------------------------------------------------------------------


;; y/p instead of full yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; esc+esc+c to open the init file
(global-set-key
 (kbd "\e\ec")
 (lambda () (interactive)
   (find-file "~/.emacs.d/init.el")))

;; RACKET 

(use-package racket-mode
  :ensure t
  :config (setq racket-program "/usr/local/bin/racket"))
(add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)


;; SML

;; pulled from sml git repo  and added sml-model.el to lisp dir. 
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "sml-mode")
(autoload 'sml-mode  "sml-mode" "Mode for editing SML." t)
(setq exec-path (cons "/usr/local/Cellar/smlnj/110.84/bin"  exec-path))
(setq sml-program-name "sml")



;; ACL2

;; stop the shell from popping up every time I start emacs  
(defvar acl2-skip-shell nil)
(setq acl2-skip-shell t)
;; the main acl2 emacs support  
;; (load "emacs-acl2")
;; ;; pete's send-form binding  
;; (load "send-form")


;; LATEX

;; (use-package auctex
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq TeX-engine 'xetex)
;;   (setq latex-run-command "xelatex"))

(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))

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

;; menlo
(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 110
                    :weight 'normal
                    :width 'normal)

(setq-default cursor-type 'bar)
(set-cursor-color "#595959")



;; gruvbox theme, had to comment
;; out the latest neoframe stuff (bug)
;; see github bug filing for updates
;; https://github.com/greduan/emacs-theme-gruvbox/issues/133
(use-package gruvbox-theme
  :ensure t
  :defer t
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

;; emacs startup profiler - using it to see what commands take time
(use-package esup
  :ensure t
  :config (autoload 'esup "esup" "Emacs Start Up Profiler." nil))

;; disable the annoying bell ring sound
(setq ring-bell-function 'ignore)
;; show file size in modeline
(size-indication-mode t)



;; setting the path for macos
(use-package exec-path-from-shell
  :ensure  t
  :config (exec-path-from-shell-initialize))

;; add as a hook for lisp mode:
;; TO-FIX: multi-line comments in Racket mode
;; still disaply the colors of the parenthesis
;; which is distracting - they should be greyed out.
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; MULTIPLE CURSORS
(use-package multiple-cursors
  :ensure t)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)


;; ----- FROZEN -----

;; +-----------------------+
;; | WINDOW CONFIGURATIONS |
;; +-----------------------+

;; Guide:
;;
;; winner-mode: C-c left (undo)
;;              C-c right (redo)
;; wind-move: Shift-<Arrow Key>
;; ace-window C-o <num>


;; WINNER MODE
;; ===========
;; undo window stuff (comes prepacked with emacs.)
;; The ‘fboundp’ test is for those XEmacs  installations that don’t have
;; winner-mode available. getting back to a delicate WindowConfiguration
;; is difficult. This is where Winner Mode comes in: With it, going back
;; to a previous session is easy
;; (when (fboundp 'winner-mode)
(winner-mode 1)
;; )

;; WIND MOVE
;; =========
;; move from window to window using Shift and the arrow keys.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; ACE WINDOW
;; ==========
;; assigns a number to each window so that you
;; can jump to it by C-o <num> (instead of repeated
;; C-o ... which is annoying)
(use-package ace-window
  :ensure t
  :init (progn (global-set-key [remap other-window] 'ace-window)))


;;; END of Window Configurations ;;;

;; --- CURSOR TWEAKS ---
;; 1. a light follows the cursor after big movements
(use-package beacon
  :ensure t
  :init (beacon-mode 1))

;; --- end of cursor tweaks ---
;; activate the packages


(set-face-attribute 'default nil :height 200)


(defun enable-gruvbox ()
  (interactive)
  (load-theme 'gruvbox-dark-soft t))

(global-set-key (kbd "<f6>") 'enable-gruvbox)


(defun disable-gruvbox ()
  (interactive)
  (disable-theme 'gruvbox-dark-soft))


(global-set-key (kbd "<f7>") 'disable-gruvbox)
