;;; setup-yas.el --- Setup for yasnippets
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all))

(provide 'setup-yas)
;;; setup-yas.el ends here
