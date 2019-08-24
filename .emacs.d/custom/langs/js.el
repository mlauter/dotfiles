;;; js.el --- Custom js editing configuration
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:

;; another huge headache, get nodenv to work
(use-package nodenv
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'nodenv-mode)
  (add-hook 'enh-ruby-mode-hook 'nodenv-mode))


;; webmode
(use-package web-mode
  :ensure t
  :after flycheck-mode
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

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'")
  :config (add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 4)))
  )
