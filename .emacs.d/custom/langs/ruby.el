;;; ruby.el --- Custom ruby editing configuration
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
  :init (add-hook 'enh-ruby-mode-hook 'rbenv-use-corresponding)
  :config (global-rbenv-mode))

(use-package enh-ruby-mode
  :ensure t
  :defer t
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
                  enh-ruby-hanging-indent-level 2))

(use-package rubocop
  :ensure t
  :defer t
  :after rbenv-mode
  :init (add-hook 'enh-ruby-mode-hook 'rubocop-mode))

(use-package robe
  :defer t
  :ensure t
  :init (add-hook 'enh-ruby-mode-hook 'robe-mode)
  :config
  (progn
    (with-eval-after-load 'auto-complete
      (add-hook 'robe-mode-hook 'ac-robe-setup))))

(use-package rspec-mode
  :ensure t
  :after enh-ruby-mode
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
