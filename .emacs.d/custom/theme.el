;;; theme.el --- Custom theme configuration
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-Iosvkem t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package spaceline
  :ensure t
  :disabled t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

;; really really tried to get all the icons to work in terminal emacs
;; because i don't really want to use the gui, but i just could not
(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package moe-theme
  :ensure t
  :disabled t
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
