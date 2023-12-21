;;; init-program.el --- Programing config -*- lexical-binding: t; -*-

;;; Commentary:

;;; treesit, rust

;;; Code:

(use-package treesit
  :when (and (fboundp 'treesit-available-p)
         (treesit-available-p))
  :config (setq treesit-font-lock-level 4)
  :init
  (setq treesit-language-source-alist
    '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
      (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
      (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
      (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
      (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
      (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
      (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
      (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
      (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
      (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
      (org        . ("https://github.com/milisims/tree-sitter-org"))
      (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
      (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
      (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
      (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
      (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))))
  (add-to-list 'major-mode-remap-alist '(sh-mode         . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-mode          . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode        . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode   . c-or-c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(css-mode        . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-mode         . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode    . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode     . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(conf-toml-mode  . toml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))

(use-package eglot
  :hook ((rust-ts-mode) . eglot-ensure)
  :config (add-to-list 'eglot-server-programs
                       `(rust-ts-mode . ("/run/current-system/sw/bin/rust-analyzer" :initializationOptions
                                     ( :procMacro (:enable t)
                                       :cargo ( :buildScripts (:enable t)
                                                :features "all"))))))

(provide 'init-program)

;;; init-program.el ends here.
