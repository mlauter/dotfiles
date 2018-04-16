;;; package --- Summary
;;; Commentary:
;; -*-Emacs-Lisp-*-

;;; Code:

;; Load packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/"))))
;;; This won't really do anything until Emacs 25.1+
(defvar package-archive-priorities)
(setq package-archive-priorities '(("MELPA Stable" . 10)
                                   ("GNU"          . 5)
                                   ("MELPA"        . 0)))
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-2.3")
  (require 'use-package))

;; Make emacs more responsive
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 800 1024))) t)

;; Start the emacs server once
(require 'server)
(unless (server-running-p) (server-start))

(use-package shackle
  :ensure t
  :defer t
  :config (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))
  :init (shackle-mode 1))

;; rebind normal M-x to C-x M-x
(global-set-key (kbd "C-x M-x") 'execute-extended-command)
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x r" . helm-recentf)
         ("C-SPC" . helm-dabbrev)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list)
	 ("C-c g" . my-helm-grep-do-git-grep))
  :bind (:map helm-map
	      ("TAB" . helm-execute-persistent-action)
	      ("<tab>" . helm-execute-persistent-action)
	      ("C-z" . helm-select-action)
	      ("M-i" . helm-previous-line)
	      ("M-k" . helm-next-line)
	      ("M-I" . helm-previous-page)
	      ("M-K" . helm-next-page)
	      ("M-h" . helm-beginning-of-buffer)
	      ("M-H" . helm-end-of-buffer))
  :config (progn
	    (setq helm-buffers-fuzzy-matching t)
	    (setq helm-completion-in-region-fuzzy-match t)
	    (setq helm-mode-fuzzy-match t)
	    (setq-default helm-M-x-fuzzy-match t)
	    (setq helm-display-function 'helm-display-buffer-in-own-frame
	    	  helm-display-buffer-reuse-frame t
	    	  helm-use-undecorated-frame-option t)

            (helm-mode 1)
	    ;; In order for this to actually work need to setup git config:
	    ;; git config --global core.excludesfile ~/.gitignore_global
	    (defun my-helm-grep-do-git-grep (not-all)
	      (interactive "P")
	      (helm-grep-git-1 default-directory (null not-all)))))
(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds))
(use-package fzf
  :ensure t
  :bind ("C-x f" . my-fzf)
  ;; If we're in a git repo, initiate fzf from the root
  :config (progn
	    (defun my-fzf ()
		(interactive)
		(if (vc-git-registered buffer-file-name) (fzf-git) (fzf)))))

(global-set-key (kbd "C-c g") 'my-helm-grep-do-git-grep)

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(php-phpmd
					     php-phpcs))
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package drag-stuff
  :ensure t
  :bind (("M-<up>" . drag-stuff-up)
	 ("M-<down>" . drag-stuff-down)
	 ("M-<left>" . shift-left)
	 ("M-<right>" . shift-right)))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package smartparens
  :ensure t
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (sp-local-pair 'php-mode "/**" "*/")
  (progn
    (setq smartparens-strict-mode t)))

(use-package ggtags
  :ensure t
  :defer t
  :commands ggtags-mode
  :config
  (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1)))))

(use-package linum-relative
  :ensure t
  :init (linum-relative-global-mode))

;; evil
(use-package bind-map
  :ensure t
  :defer t)

(use-package evil-surround
  :ensure t
  :defer t
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :ensure t
  :defer t
  :config
  (evil-escape-mode 1)
  (progn
    (setq-default evil-escape-key-sequence "<ESC>")
    (setq-default evil-escape-key-sequence "jk")
    (setq-default evil-escape-delay 0.2)))

(use-package evil
  :ensure t
  :config

  (require 'bind-map)
  (require 'evil-surround)
  (require 'evil-escape)

  (setq evil-default-cursor t)
  (setq evil-default-state 'emacs)

  (bind-map my-base-leader-map
	    :keys ("M-m")
	    :evil-keys ("SPC")
	    :evil-states (normal motion visual)
	    :bindings ("tn" 'linum-mode
		       "a" 'avy-goto-word-or-subword-1
		       "f" 'my-fzf
		       "g" 'etsy-github-link
		       "rb" 'revert-buffer))

  (progn
    (define-key evil-normal-state-map "i" 'evil-emacs-state)
    ;; Color the evil tag - colors taken from spaceline
    (setq evil-normal-state-tag   (propertize " <N> " 'face '((:background "DarkGoldenrod2" :foreground "black")))
          evil-emacs-state-tag    (propertize " <E> " 'face '((:background "SkyBlue2"       :foreground "black")))
          evil-insert-state-tag   (propertize " <I> " 'face '((:background "chartreuse3"    :foreground "black")))
          evil-replace-state-tag  (propertize " <R> " 'face '((:background "chocolate"      :foreground "black")))
          evil-motion-state-tag   (propertize " <M> " 'face '((:background "plum3"          :foreground "black")))
          evil-visual-state-tag   (propertize " <V> " 'face '((:background "gray"           :foreground "black")))
          evil-operator-state-tag (propertize " <O> " 'face '((:background "sandy brown"    :foreground "black")))))
  (evil-mode 1))

(use-package avy
  :ensure t)
;; Meta

;; Function keys
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Key bindings
;; Switch C-a and M-m
(global-set-key (kbd "C-x C-r") 'comment-region)

;; Themes
(use-package molokai-theme
  :ensure t
  :disabled t
  :load-path "themes"
  :init
  (setq molokai-theme-kit t)
  :config
  (load-theme 'molokai t)
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
  :config (load-theme 'badger t))

;; Mouse
(xterm-mouse-mode t)
(mouse-wheel-mode 't)
(setq mouse-wheel-follow-mouse 't)

;; Misc

;; etsy github link
  (defun get-github-prefix (rel-path)
    (let ((prefix "https://github.etsycorp.com/")
          (path-parts (split-string rel-path "/")))
      (pcase (or (nth 0 path-parts) "")
        ("Sysops" (concat prefix (format "Sysops/%s/blob/master/" (nth 1 path-parts))))
        ("mlauter" (concat prefix (format "mlauter/%s/blob/master/" (nth 1 path-parts))))
        (_ (concat prefix (format "Engineering/%s/blob/master/" (nth 0 path-parts))))
        )))

  (defun get-github-suffix (rel-path)
    (let ((path-parts (split-string rel-path "/")))
      (pcase (or (nth 0 path-parts) "")
        ("Sysops" (mapconcat 'identity (nthcdr 2 path-parts) "/"))
        ("mlauter" (mapconcat 'identity (nthcdr 2 path-parts) "/"))
        (_ (mapconcat 'identity (nthcdr 1 path-parts) "/"))
        )))

  (defun etsy-github-link ()
    "Show the file's etsy-github-link in the minibuffer."
    (interactive)
    (let ((local-path (expand-file-name (or (buffer-file-name) "")))
          (current-line (format-mode-line "%l")))
      (if (string-prefix-p "/home/mlauter/development/" local-path)
          (let ((trimmed-path (string-remove-prefix "/home/mlauter/development/" local-path)))
            (let ((github-prefix (get-github-prefix trimmed-path))
                  (github-suffix (get-github-suffix trimmed-path)))
              (message (concat github-prefix github-suffix "#L" current-line))))
        (message "Not an etsy path"))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Indent switch cases correctly
(add-hook 'c-mode-common-hook
          (lambda ()
	    (c-set-offset 'case-label '+)))

;; Language support
;; no tabs
(setq indent-tabs-mode nil)

;; php and webmode
(use-package php-mode
  :ensure t
  :init
  (add-hook 'php-mode-hook #'ggtags-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.tpl\\'" "\\.mustache\\'" "\\.jsx\\'")
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t))

;; js
;; Ruby
(use-package enh-ruby-mode
  :ensure t
  :defer t
  :mode (("\\.rb\\'"       . enh-ruby-mode))
  :config (progn
	    (setq enh-ruby-indent-level 2
		  enh-ruby-add-encoding-comment-on-save nil
		  enh-ruby-deep-indent-paren nil
		  enh-ruby-bounce-deep-indent t
		  enh-ruby-hanging-indent-level 2)
	    (setq enh-ruby-program "/Users/senny/.rbenv/versions/2.4.0/bin/ruby")))

(use-package rubocop
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook 'rubocop-mode))

;; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; autocompletion
(use-package auto-complete
  :ensure t
  :commands auto-complete-mode
  :init
  (progn
    (auto-complete-mode t))
  :bind (("C-n" . ac-next)
         ("C-p" . ac-previous))
  :config
  (progn
    (use-package auto-complete-config)

    (ac-set-trigger-key "TAB")
    (ac-config-default)

    (setq ac-delay 0.02)
    (setq ac-use-menu-map t)
    (setq ac-menu-height 50)
    (setq ac-use-quick-help nil)
    (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
    (setq ac-ignore-case nil)
    (setq ac-dwim  t)
    (setq ac-fuzzy-enable t)

    (setq ac-modes '(emacs-lisp-mode
                     lisp-mode
                     lisp-interaction-mode
                     c-mode
                     c++-mode
                     go-mode
                     java-mode
                     scala-mode
                     perl-mode
                     python-mode
                     ruby-mode
                     enh-ruby-mode
                     lua-mode
                     ecmascript-mode
                     javascript-mode
                     js-mode
                     js2-mode
		     js3-mode
                     php-mode
		     web-mode
                     css-mode
                     sh-mode
                     fortran-mode))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-leader/in-all-states t t)
 '(package-selected-packages
   (quote
    (badger-theme web-mode use-package smartparens rubocop php-mode molokai-theme markdown-mode magit ido-completing-read+ helm-descbinds ggtags fzf flycheck enh-ruby-mode drag-stuff color-theme-sanityinc-tomorrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:background "color-22" :distant-foreground "black"))))
 '(region ((t (:background "brightblack")))))
(provide '.emacs)
;;; .emacs ends here