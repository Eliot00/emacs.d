;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(evil-mode 1)
(setq evil-want-Y-yank-to-eol t)
(evil-set-leader 'normal (kbd "SPC"))
(evil-global-set-key 'normal (kbd "<leader>f") 'find-file)

(evil-define-key* 'normal 'global (kbd "<leader>od") 'org-roam-dailies-goto-today)

(add-hook 'org-mode-hook
  (lambda ()
    (evil-define-key 'normal org-mode-map
      (kbd "<leader>i") 'org-roam-node-insert
      (kbd "<return>") 'org-open-at-point
      (kbd "<leader>ol") 'org-store-link
      (kbd "<leader>oa") 'org-agenda
      (kbd "<leader>oc") 'org-capture)))

(provide 'init-evil)
;;; init-evil.el ends here
