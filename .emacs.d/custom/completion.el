;;; completion.el --- Custom completion configuration
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:
;; completion
(use-package company
  :ensure t
  :defer t
  :init
  ; steal some defaults from spacemacs
  (progn
      (setq company-idle-delay 0.2
            company-minimum-prefix-length 2
            company-require-match 'never ;cancel selections by typing non matching chars
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil))

  (global-company-mode)
  :config
  (progn     ;use tab to cycle like autocomplete mode
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    ; and S-tab for cycling backward
    (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
    (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
    ())
  :diminish company-mode)

(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))
