;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)

;; optional. makes unpure packages archives unavailable
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)

(global-company-mode)
(add-hook 'company-mode-hook 'company-box-mode)

(require 'init-base)
(require 'init-ui)
(require 'init-evil)

(provide 'init)
;;; init.el ends here
