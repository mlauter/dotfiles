;;; golang.el --- Custom go editing configuration
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'")
  :config (add-hook 'before-save-hook 'gofmt-before-save))
