;;; init-git.el --- Version control -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package magit
  :ensure nil
  :hook (git-commit-mode . flyspell-mode)
  :custom
  (magit-diff-refine-hunk t)
  (magit-ediff-dwim-show-on-hunks t))

(provide 'init-git)

;;; init-git.el ends here
