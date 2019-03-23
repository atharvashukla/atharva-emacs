;; activate the packages
(package-initialize)

;; obligatory melpa initialization
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; list of packages
(setq package-list '(use-package))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; use-package necessary
(require 'use-package)

;; expanding the main  org file
(org-babel-load-file (expand-file-name  "~/.emacs.d/atharvaemacs.org"))
