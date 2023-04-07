;;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(load-theme 'solo-jazz t)
(add-hook 'after-init-hook #'doom-modeline-mode)

(provide 'init-ui)

;;; init-ui.el ends here
