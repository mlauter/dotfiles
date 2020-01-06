;;; keys.el --- Custom keybinding configuration
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:
(setq mac-right-command-modifier 'meta)
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-x C-r") 'eval-region)

;; I never want the CRM buffer
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
