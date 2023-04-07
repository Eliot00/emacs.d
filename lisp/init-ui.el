;;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(load-theme 'solo-jazz t)
(use-package doom-modeline
  :ensure nil
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count t))

(provide 'init-ui)

;;; init-ui.el ends here
