;;; evil.el --- Custom movement configuration
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:
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
                       "fa" 'ffap
                       "fh" 'fzf-home
                       "ghl" 'git-link
                       "o" 'browse-url
                       "tf" 'terraform-format-buffer
                       "rb" 'revert-buffer
                       "vf" 'vimish-fold
                       "vt" 'vimish-fold-toggle
                       "gd" 'godef-describe
                       "gj" 'godef-jump-other-window
                       "tw" 'whitespace-mode
                       "gc" 'goto-last-change
                       "gr" 'goto-last-change-reverse
                       "hss" 'highlight-symbol
                       "hqr" 'highlight-symbol-query-replace
                       "hsd" 'highlight-symbol-remove-all
                       "d" 'worklog-date
                       "cr" 'set-rectangular-region-anchor
                       "cc" 'mc/edit-lines
                       "ce" 'mc/edit-ends-of-lines
                       "ca" 'mc/edit-beginnings-of-lines
                       "rt" 'anzu-replace-at-cursor-thing
                       "rq" 'anzu-query-replace-at-cursor-thing))

  (progn
    (define-key evil-normal-state-map "i" 'evil-emacs-state)
    ;; Color the evil tag - colors taken from spaceline
    ;; (setq evil-normal-state-tag   (propertize " <N> " 'face '((:background "DarkGoldenrod2" :foreground "black")))
    ;;       evil-emacs-state-tag    (propertize " <E> " 'face '((:background "SkyBlue2"       :foreground "black")))
    ;;       evil-insert-state-tag   (propertize " <I> " 'face '((:background "chartreuse3"    :foreground "black")))
    ;;       evil-replace-state-tag  (propertize " <R> " 'face '((:background "chocolate"      :foreground "black")))
    ;;       evil-motion-state-tag   (propertize " <M> " 'face '((:background "plum3"          :foreground "black")))
    ;;       evil-visual-state-tag   (propertize " <V> " 'face '((:background "gray"           :foreground "black")))
    ;;       evil-operator-state-tag (propertize " <O> " 'face '((:background "sandy brown"    :foreground "black")))))
  (evil-mode 1)))
