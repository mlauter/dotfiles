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
      ad-do-it))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode)))

(use-package js2-mode
  :ensure t
  :demand t
  :commands js2-mode
  :hook ((js2-mode . (lambda ()
                       (flycheck-mode t)
                       (company-mode t))))
  :mode (("\\.js$" . js2-mode)
         ("\\.es6$" . js2-mode)
         ("\\.ejs$" . js2-mode))
  :config
  ;; have 2 space indentation by default
  (setq js-indent-level 2
        js2-basic-offset 2
        js-chain-indent t
	js-switch-indent-offset 2)

  ;; company-backends setup
  (set (make-local-variable 'company-backends)
       '((company-files :with company-yasnippet)
         (company-dabbrev-code company-dabbrev)))

  ;; unbind so that xref-js2 can use it
  (define-key js-mode-map (kbd "M-.") nil))

(use-package add-node-modules-path
  :ensure t
  :hook ((js2-mode . add-node-modules-path)
         (rsjx-mode . add-node-modules-path)))

;; prettier-emacs: minor-mode to prettify javascript files on save
;; https://github.com/prettier/prettier-emacs
(use-package prettier-js
  :ensure t
  :hook ((js2-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

;; rjsx-mode: A JSX major mode for Emacs
;; https://github.com/felipeochoa/rjsx-mode
(use-package rjsx-mode
  :ensure t
  :after js2-mode
  :mode (("\\.jsx?$" . rjsx-mode)
         ("components/.+\\.js$" . rjsx-mode))
  :hook (rjsx-mode . (lambda ()
                          (flycheck-mode t)
                          (company-mode t)))
  :init
  (defun +javascript-jsx-file-p ()
    "Detect React or preact imports early in the file."
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import +React\\|\\( from \\|require(\\)[\"']p?react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (sp-point-in-string-or-comment)))))
  (add-to-list 'magic-mode-alist '(+javascript-jsx-file-p . rjsx-mode))
  :config (unbind-key "C-c C-l" rjsx-mode-map))

;; tern completion https://github.com/ternjs/tern
;; npm install -g tern
(use-package tern
   :ensure t
   :config
   (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
   (use-package company-tern
     :ensure t
     :init (add-hook 'js2-mode-hook (lambda ()(add-to-list 'company-backends 'company-tern))))
   ;; Disable completion keybindings, as we use xref-js2 instead
   (define-key tern-mode-keymap (kbd "M-.") nil)
   (define-key tern-mode-keymap (kbd "M-,") nil))

;; xref-js2 better jumping to references https://github.com/NicolasPetton/xref-js2
;; requires ag (the_silver_searcher)
(use-package xref-js2
  :ensure t
  :disabled t
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'")
  :config (add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 4)))
  )

(use-package coffee-mode
  :ensure t
  :mode ("\\.coffee\\'")
  :init (custom-set-variables '(coffee-tab-width 2)))
;;; js.el ends here
