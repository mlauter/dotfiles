;;; search.el --- Custom file search config
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:
(use-package fzf
  :ensure t
  :commands (fzf/start)
  :bind ("C-x f" . fzf)
  :config
  (progn
    (defun my-fzf()
      "Start fzf from git root or from home dir if not a git dir."
      (interactive)
      (let ((homedir (getenv "HOME")))
      (fzf/start (or (vc-root-dir) homedir))))
    (defun fzf-home()
      "Start fzf from my homedir."
      (interactive)
      (let ((homedir (getenv "HOME")))
        (fzf/start homedir)))))

;; Helm
;; one day it might be nice to try and use helm as frontend to fzf
(use-package shackle
  :ensure t
  :defer t
  :config (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))
  :init (shackle-mode 1))

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
