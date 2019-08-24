;;; theme.el --- Custom theme configuration
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:
(use-package moe-theme
  :ensure t
  :config (progn
            (moe-theme-set-color 'green)
            (moe-dark)))

(use-package molokai-theme
  :ensure t
  :disabled t
  :load-path "themes"
  :init
  (setq molokai-theme-kit t)
  :config
  (load-theme 'molokai t)
  )

(use-package monokai-theme
  :ensure t
  :disabled t
  :load-path "themes"
  :config
  (load-theme 'monokai t)
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :disabled t
  :load-path "themes"
  :config
  (load-theme 'sanityinc-tomorrow-bright t)
  )

(use-package badger-theme
  :ensure t
  :disabled t
  :config (load-theme 'badger t))
