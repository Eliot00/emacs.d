;;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package ef-themes
  :ensure nil
  :init
  ;; set two specific themes and switch between them
  (setq ef-themes-to-toggle '(ef-summer ef-winter))
  ;; set org headings and function syntax
  (setq ef-themes-headings
        '((0 . (bold 1))
          (1 . (bold 1))
          (2 . (rainbow bold 1))
          (3 . (rainbow bold 1))
          (4 . (rainbow bold 1))
          (t . (rainbow bold 1))))
  (setq ef-themes-region '(intense no-extend neutral))
  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  (ef-themes-select 'ef-spring))

(use-package doom-modeline
  :ensure nil
  :hook (after-init . doom-modeline-mode)
  :init
  (setq nerd-icons-font-family "FiraCode Nerd Font Mono")
  :custom
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count t))

(provide 'init-ui)

;;; init-ui.el ends here
