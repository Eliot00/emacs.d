;;; init-completion.el --- Completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(savehist-mode 1)
(setq enable-recursive-minibuffers t)

(use-package vertico
  :ensure nil
  :init
  (vertico-mode)
  :bind (:map minibuffer-local-map
	      ("M-<DEL>" . my/minibuffer-backward-kill)
	      :map vertico-map
	      ("M-q" . vertico-quick-insert)) ; use C-g to exit
  :config
  (defun my/minibuffer-backward-kill (arg)
    "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
    (interactive "p")
    (if minibuffer-completing-file-name
    ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
    (if (string-match-p "/." (minibuffer-contents))
      (zap-up-to-char (- arg) ?/)
    (delete-minibuffer-contents))
      (backward-kill-word arg)))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq vertico-cycle t)                ; cycle from last to first
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))
  :custom
  (vertico-count 15)                    ; number of candidates to display, default is 10
  )

(use-package orderless
  :ensure nil
  :init
  (setq completion-styles '(orderless partial-completion basic))
  (setq orderless-component-separator "[ &]") ; & is for company because space will break completion
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure nil
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package consult
  :ensure nil
  :after org
  :bind (([remap goto-line]                     . consult-goto-line)
         ([remap isearch-forward]               . consult-line-symbol-at-point) ; my-consult-ripgrep-or-line
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap apropos]                       . consult-apropos)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap multi-occur]                   . consult-multi-occur)
         ([remap recentf-open-files]            . consult-recent-file)
         ("C-x j"                               . consult-mark)
         ("C-c g"                               . consult-ripgrep)
         ("C-c f"                               . consult-find)
         ("\e\ef"                               . consult-locate) ; need to enable locate first
         ("C-c n h"                             . my/consult-find-org-headings)
         :map org-mode-map
         ("C-c C-j"                             . consult-org-heading)
         :map minibuffer-local-map
         ("C-r"                                 . consult-history)
         :map isearch-mode-map
         ("C-;"                                 . consult-line)
         :map prog-mode-map
         ("C-c C-j"                             . consult-outline)
         )
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; MacOS locate doesn't support `--ignore-case --existing' args.
  (setq consult-locate-args (pcase system-type
                              ('gnu/linux "locate --ignore-case --existing --regex")
                              ('darwin "mdfind -name")))
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  ;; search all org file headings under a directory, see:
  ;; https://emacs-china.org/t/org-files-heading-entry/20830/4
  (defun my/consult-find-org-headings (&optional match)
    "find headngs in all org files."
    (interactive)
    (consult-org-heading match (directory-files org-directory t "^[0-9]\\{8\\}.+\\.org$")))

  ;; Use `consult-ripgrep' instead of `consult-line' in large buffers
  (defun consult-line-symbol-at-point ()
    "Consult line the synbol where the point is"
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  )

(provide 'init-completion)

;;; init-completion.el ends here
