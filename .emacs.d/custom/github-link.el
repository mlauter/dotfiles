;;; github-link.el --- Get github-link to a file in a git repo
;;
;;; Commentary:
;; -*-Emacs-Lisp-*-
;;
;; Author: Miriam Lauter <lauter.miriam@gmail.com>
;;; Code:
(defun github-link ()
  "Get the github url of any point in a file in a git repo."
  (interactive)
  (let ((remote-url
         (shell-command-to-string "git config --get remote.origin.url"))
        (git-root-path
         (locate-dominating-file default-directory ".git")))
    (if (or (not git-root-path) (string= "" remote-url))
        (message "github-link: not a git repo!")
        (message
         (format "https://%s/blob/master/%s#L%d"
                 (progn
                   (string-match "\\(^[a-z]+?://\\)?\\(.*?\\)\\(\\.git\\)?$" remote-url)
                   (match-string 2 remote-url))
                 (substring (buffer-file-name) (length (expand-file-name git-root-path)))
                 (line-number-at-pos))))))

(provide 'github-link)
;;; github-link.el ends here
