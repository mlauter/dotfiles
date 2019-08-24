;;; php.el --- Custom php editing configuration
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:

;; Indent switch cases correctly
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))
