;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(evil-mode 1)
(setq evil-want-Y-yank-to-eol t)
(evil-set-leader 'normal (kbd "SPC"))
(evil-global-set-key 'normal (kbd "<leader>f") 'find-file)

(add-hook 'org-mode-hook
  (lambda ()
    (evil-define-key 'normal org-mode-map
      (kbd "<leader>oi") 'org-roam-note-insert
      (kbd "<leader>ol") 'org-store-link
      (kbd "<leader>oa") 'org-agenda
      (kbd "<leader>oc") 'org-capture)))

(provide 'init-evil)
;;; init-evil.el ends here
