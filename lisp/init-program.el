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

(use-package elisp-mode
  :ensure nil
  :after org
  :bind (:map emacs-lisp-mode-map
              ("C-c C-b" . eval-buffer)
              ("C-c C-c" . eval-to-comment)
              :map lisp-interaction-mode-map
              ("C-c C-c" . eval-to-comment)
              :map org-mode-map
              ("C-c C-;" . eval-to-comment)
              )
  :init
  ;; for emacs-lisp org babel
  (add-to-list 'org-babel-default-header-args:emacs-lisp
             '(:results . "value pp"))
  :config
  (defconst eval-as-comment-prefix " ⇒ ")
  (defun eval-to-comment (&optional arg)
    (interactive "P")
    ;; (if (not (looking-back ";\\s*"))
    ;;     (call-interactively 'comment-dwim))
    (call-interactively 'comment-dwim)
    (progn
      (search-backward ";")
      (forward-char 1))
    (delete-region (point) (line-end-position))
    (save-excursion
      (let ((current-prefix-arg '(4)))
        (call-interactively 'eval-last-sexp)))
    (insert eval-as-comment-prefix)
    (end-of-line 1))
  )

(use-package python
  :ensure nil
  :init
  (add-list-to-list 'org-babel-default-header-args:python '((:results . "output pp")
                                                            (:noweb   . "yes")
                                                            (:session . "py")
                                                            (:async   . "yes")
                                                            (:exports . "both")
                                                            ))
  :config
  (setq python-indent-offset 4)
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-interpreter "python3"
        python-shell-prompt-detect-failure-warning nil)
  ;; disable native completion
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  )

(provide 'init-program)

;;; init-program.el ends here.
