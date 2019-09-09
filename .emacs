;;; .emacs --- Main emacs config
;;; Commentary:
;; -*-Emacs-Lisp-*-

;;; Code:

;; Load packages and setup
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

;; Set exec-path to system path
;; this is the path used when emacs is shelling out
;; as opposed to path, which is used in eshell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (eq system-type 'darwin)
    (exec-path-from-shell-initialize)))

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; Load the rest of my config
;; separated out into .emacs.d/custom for cleanliness
(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))
                 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/custom")
(load-directory "~/.emacs.d/custom/langs")
;; --- end setup ---

;; Packages
(use-package all-the-icons
  :ensure t)

(use-package unicode-fonts
  :ensure t
  :config (unicode-fonts-setup))

(use-package linum-relative
  :ensure t
  :init (linum-relative-global-mode))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package anzu
  :ensure t
  :defer t
  :init (setq anzu-cons-mode-line-p nil)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode t)
  :diminish anzu-mode)

;; learn about emacs
(use-package discover
  :ensure t
  :config (global-discover-mode 1))

;; hide modes from mode line
(use-package diminish
  :ensure t
  :demand t)

(use-package git-link
  :ensure t
  :init (setq git-link-open-in-browser t))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
;; --- end Packages

;; Functions
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-c u") 'backward-kill-line)

(defun worklog-date ()
  "Insert date at point formatted for worklog."
  (interactive)
  (insert (shell-command-to-string "date +\"%b %d, %Y\"")))

;; --- end Functions

;; Config
;; Require final newline everywhere
(setq require-final-newline t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-eldoc-mode 1)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; subword mode by default
(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))

;; Mouse
(xterm-mouse-mode t)
(setq mouse-wheel-follow-mouse 't)
;; scroll support for osx
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

;; org mode
(require 'org)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cv" 'org-todo-list)
(global-set-key "\C-cl" 'org-store-link)
(setq org-log-done 'time)
(define-key org-mode-map (kbd "\C-c \C-q") nil)
(define-key org-mode-map (kbd "\C-c \C-q") 'counsel-org-tag)
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "IN-REVIEW(r@)" "BLOCKED(b@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(use-package org-jira
  :ensure t
  :init (setq jiralib-url "https://actblue.atlassian.net")
  :hook (org-mode . org-jira-mode))
;; -- end config

;; Highlighting, checking, and completion
(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(php-phpmd
                                             php-phpcs
                                             javascript-jshint
                                             json-jsonlist))
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)))

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

(use-package highlight-symbol
  :ensure t
  :init
  (setq highlight-symbol-idle-delay 0.5)
  :bind (("M-p" . highlight-symbol-prev)
         ("M-n" . highlight-symbol-next))
  :config
  (progn
    (add-hook 'prog-mode-hook 'highlight-symbol-mode)
    (add-hook 'emacs-lisp-mode 'highlight-symbol-mode)))

;; show paren style
(show-paren-mode t)
(setq show-paren-style 'expression)
;; --- end highlighting, checking, and completion

;; Language support (the rest is in ~/.emacs.d/custom/langs)
;; no tabs
(setq-default indent-tabs-mode nil)

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

;; tab display width in various modes
(add-hook 'conf-toml-mode-hook
      (lambda ()
        (setq tab-width 2)))
;;--- end language support

;; autocompletion
(use-package dash-at-point
  :ensure t
  :bind ("C-c d" . dash-at-point))

;; TODO company mode
(use-package auto-complete
  :ensure t
  :disabled t
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
                     ecmascrip
                     t-mode
                     javascript-mode
                     js-mode
                     js2-mode
                     php-mode
                     web-mode
                     css-mode
                     sh-mode
                     fortran-mode
                     scala-mode))))
;;--- end autocompletion

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(coffee-tab-width 2)
 '(counsel-find-file-ignore-regexp "\\(?:\\`\\|[/\\]\\)\\(?:[#.]\\)")
 '(debug-on-error t)
 '(enh-ruby-deep-indent-construct t)
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
 '(js-indent-level 2)
 '(json-mode-indent-level 4)
 '(max-specpdl-size 1400)
 '(org-agenda-custom-commands
   (quote
    (("W" "All agenda items closed during the past week" todo "DONE|CANCELED"
      ((org-agenda-start-on-weekday 0)
       (org-agenda-overriding-header "")))
     ("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil))))
 '(org-agenda-files (quote ("~/org/jira/PC.org" "~/org/work.org")))
 '(org-jira-jira-status-to-org-keyword-alist
   (quote
    (("RESOLVED" . "DONE")
     ("TO-DO" . "TODO")
     ("In Review" . "IN-REVIEW")
     ("In Progress" . "IN-PROGRESS"))))
 '(org-jira-use-status-as-todo nil)
 '(org-jira-working-dir "~/org/jira")
 '(org-list-indent-offset 2)
 '(org-startup-folded (quote showeverything))
 '(org-startup-truncated t)
 '(package-selected-packages
   (quote
    (org-jira yard-mode coffee-mode vscode-icon dired-sidebar unicode-fonts spaceline discover highlight-symbol nodenv auto-highlight-symbol lua-mode javascript-eslint js2-mode yaml-mode go-mode origami badger-theme web-mode use-package smartparens rubocop php-mode molokai-theme markdown-mode magit ido-completing-read+ helm-descbinds ggtags fzf flycheck drag-stuff color-theme-sanityinc-tomorrow)))
 '(rubocop-autocorrect-command "rubocop -a --format emacs --rails --fail-level C")
 '(rubocop-autocorrect-on-save t)
 '(rubocop-check-command "rubocop --format emacs --rails --fail-level C")
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1b1d1e" :foreground "#dddddd" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "IBM Plex Mono"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "cadet blue" :slant normal)))))
 (provide '.emacs)
;;; .emacs ends here
