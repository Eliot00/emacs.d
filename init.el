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
(package-initialize 'noactivate)
(eval-when-compile
  (require 'use-package))

(global-flycheck-mode)

(require 'init-base)
(require 'init-ui)
(require 'init-completion)
(require 'init-git)
(require 'init-roam)
(require 'init-evil)
(require 'init-rust)

(provide 'init)
;;; init.el ends here
