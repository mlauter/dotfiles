;;; package --- Summary
;;; Commentary:
;; -*-Emacs-Lisp-*-

;;; Code:

;; Load packages
(require 'package)

;; bug in Emacs: https://debbugs.gnu.org/34341 should be fixed in 26.3
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  ;; gnu elpa is down :( try a mirror?
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/"))))
;;; This won't really do anything until Emacs 25.1+
(defvar package-archive-priorities)
(setq package-archive-priorities '(("MELPA Stable" . 10)
                                   ("GNU"          . 5)
                                   ("MELPA"        . 0)))
(package-initialize)

;; Make emacs more responsive
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 800 1024))) t)

;; Start the emacs server once
(require 'server)
(unless (server-running-p) (server-start))

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-2.3")
  (require 'use-package))

;; Require version control
(require 'vc)

;; try to resolve path issue for finding fzf
(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(use-package fzf
  :ensure t
  :defer t
  :bind ("C-x f" . fzf))

(defun my-fzf()
  "Start fzf from git root or from home dir if not a git dir."
  (interactive)
  (fzf/start (or (vc-root-dir) "/Users/mlauter-m1")))

(defun fzf-home()
  "Start fzf from my homedir."
  (interactive)
  (fzf/start "/Users/mlauter-m1"))

(use-package shackle
  :ensure t
  :defer t
  :config (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))
  :init (shackle-mode 1))

;; Helm

(use-package helm
  :ensure t
  :init (helm-mode 1)
    :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
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
                  helm-use-undecorated-frame-option t
                  helm-ff-skip-boring-files t)
            (setq helm-ff-lynx-style-map t)

            (helm-mode 1)
            ;; In order for this to actually work need to setup git config:
            ;; git config --global core.excludesfile ~/.gitignore_global
            (defun my-helm-grep-do-git-grep (not-all)
              (interactive "P")
              (helm-grep-git-1 default-directory (null not-all)))))

;; rebind normal M-x to C-x M-x
(global-set-key (kbd "C-x M-x") 'execute-extended-command)
(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds))

(global-set-key (kbd "C-c g") 'my-helm-grep-do-git-grep)

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(php-phpmd
                                             php-phpcs
                                             javascript-jshint
                                             json-jsonlist))
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package drag-stuff
  :ensure t
  :bind (("ESC <up>" . drag-stuff-up)
         ("ESC <down>" . drag-stuff-down)))

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
  (progn
    (setq smartparens-strict-mode nil)
    (sp-local-pair 'php-mode "/**" "*/")))

(use-package ggtags
  :ensure t
  :defer t
  :commands ggtags-mode
  :config
  (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'scala-mode 'enh-ruby-mode)
                  (ggtags-mode 1)))))

(use-package auto-highlight-symbol
  :ensure t
  :init (setq ahs-idle-interval 0.5)
  )

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

(use-package evil-vimish-fold
  :ensure t
  :defer t
  :config
  (evil-vimish-fold-mode 1))

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
                       "ff" 'my-fzf
                       "fh" 'fzf-home
                       "ghl" 'github-link
                       "tf" 'terraform-format-buffer
                       "rb" 'revert-buffer
                       "vf" 'vimish-fold
                       "vt" 'vimish-fold-toggle
                       "gd" 'godef-describe
                       "gj" 'godef-jump-other-window
                       "tw" 'whitespace-mode))

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
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-c u") 'backward-kill-line)

(defun worklog-date ()
  "Insert date at point formatted for worklog."
  (interactive)
  (insert (shell-command-to-string "date +\"%b %d, %Y\"")))
(global-set-key (kbd "C-c d") 'worklog-date)

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
;; I don't ever use the narrow commands so i don't need C-x n
(global-unset-key (kbd "C-x n"))

;; Now bind C-x p and C-x n to (previous-buffer) and (next-buffer) to free up C-x + arrow keys
(global-set-key (kbd "C-x p") 'previous-buffer)
(global-set-key (kbd "C-x n") 'next-buffer)

;; Use the arrow keys for window navigation
(global-set-key (kbd "C-x <right>") 'evil-window-right)
(global-set-key (kbd "C-x <left>") 'evil-window-left)
(global-set-key (kbd "C-x <up>") 'evil-window-up)
(global-set-key (kbd "C-x <down>") 'evil-window-down)
;; go to the window i was last on
(global-set-key (kbd "C-x w") 'evil-window-prev)

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

(use-package monokai-theme
  :ensure t
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

;; show paren style
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Mouse
(xterm-mouse-mode t)
(setq mouse-wheel-follow-mouse 't)
;; scroll support for osx
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

;; Misc
;; subword mode everywhere
(global-subword-mode 1)

;; Require final newline everywhere
(setq require-final-newline t)

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

;; any github link
(defun github-link ()
  "Get the github url of any point in a file in a git repo."
  (interactive)
  (let ((remote-url
         (shell-command-to-string "git config --get remote.origin.url"))
        (git-root-path
         (locate-dominating-file default-directory ".git")))
    (if (or (not git-root-path) (string= "" remote-url))
        (message "github-link: not a git repo!")
        (message
         (format "https://%s/blob/master/%s#L%d"
                 (progn
                   (string-match "\\(^[a-z]+?://\\)?\\(.*?\\)\\(\\.git\\)?$" remote-url)
                   (match-string 2 remote-url))
                 (substring (buffer-file-name) (length (expand-file-name git-root-path)))
                 (line-number-at-pos))))))

(provide 'github-link)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Indent switch cases correctly
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

;; subword mode by default
(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))

;; Language support
;; no tabs
(setq-default indent-tabs-mode nil)

;; php and webmode
(use-package php-mode
  :ensure t
  :init
  (add-hook 'php-mode-hook #'ggtags-mode))

(use-package geben
  :ensure t
  :config
  (if (equal "development" (getenv "ETSY_ENVIRONMENT"))
      (setq geben-dbgp-default-port 9003)))

(use-package web-mode
  :ensure t
  :mode ("\\.tpl\\'" "\\.mustache\\'" "\\.jsx\\'" "\\.erb\\'")
  :init
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'web-mode-hook (lambda () (smartparens-mode 0)))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  :config
  (defadvice web-mode-highlight-part (around tweak-jsx-activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(use-package js2-mode
  :ensure t
  :commands js2-mode
  :mode (("\\.js$" . js2-mode)
         ("\\.es6$" . js2-mode)
         ("\\.ejs$" . js2-mode)))

;; another huge headache, get nodenv to work
(use-package nodenv
  :ensure t
  :defer t
  :config (progn
            (add-to-list 'exec-path (expand-file-name "~/.nodenv/shims"))
            (add-hook 'js-mode-hook 'nodenv-mode)
            (add-hook 'ruby-mode-hook 'nodenv-mode)))

;; go
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'")
  :config (add-hook 'before-save-hook 'gofmt-before-save))

;; js
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'")
  :config (add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 4)))
  )

;; Ruby
(use-package ruby-mode
  :ensure t
  :defer t
  :mode ("\\.rb\\'"))

(use-package enh-ruby-mode
  :ensure t
  :defer t
  :mode ("\\.rb\\'")
  :config (progn
            (setq enh-ruby-indent-level 2
                  enh-ruby-add-encoding-comment-on-save nil
                  enh-ruby-deep-indent-paren nil
                  enh-ruby-bounce-deep-indent t
                  enh-ruby-hanging-indent-level 2)))

;; this was a huge headache to figure out
;; to get emacs to find the right ruby and then be able to robe start:
;; M-x rbenv-use-corresponding
;; M-x robe-start
(use-package rbenv
  :ensure t
  :defer t
  :init (setq rbenv-show-active-ruby-in-modeline t)
  :config (progn
            (global-rbenv-mode)
            (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)))

(use-package rubocop
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook 'rubocop-mode))

(use-package robe
  :defer t
  :ensure t
  :after ruby-mode
  :init
  :disabled t
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (with-eval-after-load 'auto-complete
      (add-hook 'robe-mode-hook 'ac-robe-setup))))

(use-package rspec-mode
  :ensure t
  :after ruby-mode
  :init
  (progn
    (setq rspec-use-rake-flag nil))
  :config
  (progn
    (defadvice rspec-compile (around rspec-compile-around activate)
      "Use BASH shell for running the specs because of ZSH issues."
      (let ((shell-file-name "/bin/bash"))
        ad-do-it))))

;; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; terraform
(use-package terraform-mode
  :ensure t
  :defer t
  :hook terraform-format-on-save-mode)

;; scala
(use-package scala-mode
  :ensure t
  :defer t
  :interpreter
  ("scala" . scala-mode)
  :init
  (progn
    (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
      (add-to-list 'completion-ignored-extensions ext)))
  :config
  (progn
    ;; Automatically insert asterisk in a comment when enabled
    (defun scala/newline-and-indent-with-asterisk ()
      (interactive)
      (newline-and-indent)
      (when scala-auto-insert-asterisk-in-comments
        (scala-indent:insert-asterisk-on-multiline-comment)))

    (evil-define-key 'insert scala-mode-map
      (kbd "RET") 'scala/newline-and-indent-with-asterisk)

    (evil-define-key 'normal scala-mode-map "J" 'spacemacs/scala-join-line)

    (setq scala-indent:align-forms t
            scala-indent:align-parameters t
            scala-indent:default-run-on-strategy scala-indent:eager-strategy
            scala-indent:step 2))
  )

;; tab display width in various modes
(add-hook 'conf-toml-mode-hook
      (lambda ()
        (setq tab-width 2)))

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

    (setq ac-delay 0.5)
    (setq ac-use-menu-map t)
    (setq ac-menu-height 10)
    (setq ac-use-quick-help nil)
    (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
    (setq ac-ignore-case nil)
    (setq ac-dwim  t)
    (setq ac-fuzzy-enable t)

    (setq ac-modes '(emacs-lisp-mode
                     lisp-mode
                     lisp-interaction-mode
                     terraform-mode
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
                     php-mode
                     web-mode
                     css-mode
                     sh-mode
                     fortran-mode
                     scala-mode))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(evil-leader/in-all-states t t)
 '(geben-dbgp-feature-list
   (quote
    ((:set max_data 32768)
     (:set max_depth 1)
     (:set max_children 32)
     (:set multiple_sessions geben-dbgp-breakpoint-store-types))))
 '(helm-boring-file-regexp-list
   (quote
    ("\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$" "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$" "\\.svn\\(/\\|$\\)" "\\.hg\\(/\\|$\\)" "\\.git\\(/\\|$\\)" "\\.bzr\\(/\\|$\\)" "CVS\\(/\\|$\\)" "_darcs\\(/\\|$\\)" "_MTN\\(/\\|$\\)" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.dfsl$" "\\.pfsl$" "\\.d64fsl$" "\\.p64fsl$" "\\.lx64fsl$" "\\.lx32fsl$" "\\.dx64fsl$" "\\.dx32fsl$" "\\.fx64fsl$" "\\.fx32fsl$" "\\.sx64fsl$" "\\.sx32fsl$" "\\.wx64fsl$" "\\.wx32fsl$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$" "\\~$" "\\#$")))
 '(helm-ff-lynx-style-map t)
 '(json-mode-indent-level 4)
 '(max-specpdl-size 1400)
 '(package-selected-packages
   (quote
    (nodenv auto-highlight-symbol lua-mode javascript-eslint js2-mode yaml-mode go-mode origami badger-theme web-mode use-package smartparens rubocop php-mode molokai-theme markdown-mode magit ido-completing-read+ helm-descbinds ggtags fzf flycheck enh-ruby-mode drag-stuff color-theme-sanityinc-tomorrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-doc-face ((t (:foreground "brightblack")))))
(provide '.emacs)
;;; .emacs ends here
