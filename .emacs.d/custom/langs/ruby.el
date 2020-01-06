;; ruby.el --- Custom ruby editing configuration
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:

(use-package rbenv
  :ensure t
  :demand t
  :commands (rbenv-use-corresponding)
  :config (global-rbenv-mode))

;; Installed from local path because there is a newer version from github
(use-package enh-ruby-mode
  :load-path "~/.emacs.d/Enhanced-Ruby-Mode"
  :defer t
  :hook (enh-ruby-mode . eldoc-mode)
  :mode (("\\.rb\\'"       . enh-ruby-mode)
         ("\\.ru\\'"       . enh-ruby-mode)
	 ("\\.jbuilder\\'" . enh-ruby-mode)
         ("\\.gemspec\\'"  . enh-ruby-mode)
         ("\\.rake\\'"     . enh-ruby-mode)
         ("Rakefile\\'"    . enh-ruby-mode)
         ("Gemfile\\'"     . enh-ruby-mode)
         ("Guardfile\\'"   . enh-ruby-mode)
         ("Capfile\\'"     . enh-ruby-mode)
         ("Vagrantfile\\'" . enh-ruby-mode))
  :init (setq enh-ruby-indent-level 2
                  enh-ruby-add-encoding-comment-on-save nil
                  enh-ruby-deep-indent-paren nil
                  enh-ruby-bounce-deep-indent t
                  enh-ruby-hanging-indent-level 2)
  :config (add-hook 'enh-ruby-mode-hook 'rbenv-use-corresponding))

;; (use-package ruby-mode
;;   :ensure t
;;   :defer t
;;   :disabled t
;;   :hook (ruby-mode . eldoc-mode)
;;   :mode (("\\.rb\\'"       . ruby-mode)
;;          ("\\.ru\\'"       . ruby-mode)
;; 	 ("\\.jbuilder\\'" . ruby-mode)
;;          ("\\.gemspec\\'"  . ruby-mode)
;;          ("\\.rake\\'"     . ruby-mode)
;;          ("Rakefile\\'"    . ruby-mode)
;;          ("Gemfile\\'"     . ruby-mode)
;;          ("Guardfile\\'"   . ruby-mode)
;;          ("Capfile\\'"     . ruby-mode)
;;          ("Vagrantfile\\'" . ruby-mode))
;;   :config (add-hook 'ruby-mode-hook 'rbenv-use-corresponding))

;; used emacs built in customization for some more settings
;; check .emacs
(use-package rubocop
  :ensure t
  :defer t
  :hook ((enh-ruby-mode . rubocop-mode)
         (ruby-mode . rubocop-mode)))
  ;; :config
  ;; (defvar rubocop-autocorrect-current-file-hook nil
  ;;   "Hook called after rubocop-autocorrect-current-file")
  ;; (add-hook 'rubocop-autocorrect-current-file-hook
  ;;           (lambda ()
  ;;             (revert-buffer))))

(use-package yard-mode
  :ensure t
  :hook ((enh-ruby-mode . yard-mode)
         (ruby-mode . yard-mode)))

(use-package robe
  :defer t
  :ensure t
  :hook ((enh-ruby-mode . robe-mode)
         (ruby-mode . robe-mode))
  :config
  (unbind-key "M-." robe-mode-map)
  (unbind-key "M-," robe-mode-map)
  (progn
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-robe))))

(use-package rspec-mode
  :ensure t
  :init
  (progn
    (setq rspec-use-rake-flag nil))
  :config
  (progn
    (defadvice rspec-compile (around rspec-compile-around activate)
      "Use BASH shell for running the specs because of ZSH issues."
      (let ((shell-file-name "/bin/bash"))
        ad-do-it))))

(bind-map my-ruby-map
  :keys ("M-m r")
  :evil-keys ("SPC r")
  :evil-states (normal motion visual)
  :major-modes (enh-ruby-mode
                ruby-mode
                js2-mode)
  :bindings ("'" 'robe-start
             "d" 'robe-doc
             "j" 'robe-jump
             "sr" 'robe-rails-refresh))

(use-package projectile-rails
    :ensure t
    :after projectile
    :diminish projectile-rails-mode
    :hook (projectile-mode . projectile-rails-global-mode))
