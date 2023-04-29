;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
		 (org-mode . my/org-prettify-symbols))
  :commands (org-find-exact-headline-in-buffer org-set-tags)
  :custom-face
  ;; 设置Org mode标题以及每级标题行的大小
  (org-document-title ((t (:height 1.75 :weight bold))))
  (org-level-1 ((t (:height 1.2 :weight bold))))
  (org-level-2 ((t (:height 1.15 :weight bold))))
  (org-level-3 ((t (:height 1.1 :weight bold))))
  (org-level-4 ((t (:height 1.05 :weight bold))))
  (org-level-5 ((t (:height 1.0 :weight bold))))
  (org-level-6 ((t (:height 1.0 :weight bold))))
  (org-level-7 ((t (:height 1.0 :weight bold))))
  (org-level-8 ((t (:height 1.0 :weight bold))))
  (org-level-9 ((t (:height 1.0 :weight bold))))
  ;; 设置代码块用上下边线包裹
  (org-block-begin-line ((t (:underline t :background unspecified))))
  (org-block-end-line ((t (:overline t :underline nil :background unspecified))))
  :config
  ;; ================================
  ;; 在org mode里美化字符串
  ;; ================================
  (defun my/org-prettify-symbols ()
	(setq prettify-symbols-alist
		  (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
				  '(
					;; ("[ ]"              . 9744)         ; ☐
					;; ("[X]"              . 9745)         ; ☑
					;; ("[-]"              . 8863)         ; ⊟
					("#+begin_src"      . 9998)         ; ✎
					("#+end_src"        . 9633)         ; □
					("#+begin_example"  . 129083)       ; 🠻
					("#+end_example"    . 129081)       ; 🠹
					("#+results:"       . 9776)         ; ☰
					("#+attr_latex:"    . "🄛")
					("#+attr_html:"     . "🄗")
					("#+attr_org:"      . "🄞")
					("#+name:"          . "🄝")         ; 127261
					("#+caption:"       . "🄒")         ; 127250
					("#+date:"          . "📅")         ; 128197
					("#+author:"        . "💁")         ; 128100
					("#+setupfile:"     . 128221)       ; 📝
					("#+email:"         . 128231)       ; 📧
					("#+startup:"       . 10034)        ; ✲
					("#+options:"       . 9965)         ; ⛭
					("#+title:"         . 10162)        ; ➲
					("#+subtitle:"      . 11146)        ; ⮊
					("#+downloaded:"    . 8650)         ; ⇊
					("#+language:"      . 128441)       ; 🖹
					("#+begin_quote"    . 187)          ; »
					("#+end_quote"      . 171)          ; «
                    ("#+begin_results"  . 8943)         ; ⋯
                    ("#+end_results"    . 8943)         ; ⋯
					)))
    (setq prettify-symbols-unprettify-at-point t)
	(prettify-symbols-mode 1))

  ;; 提升latex预览的图片清晰度
  (plist-put org-format-latex-options :scale 1.8)

  ;; 设置标题行之间总是有空格；列表之间根据情况自动加空格
  (setq org-blank-before-new-entry '((heading . t)
									 (plain-list-item . auto)
									 ))

  ;; ======================================
  ;; 设置打开Org links的程序
  ;; ======================================
  (defun my-func/open-and-play-gif-image (file &optional link)
	"Open and play GIF image `FILE' in Emacs buffer.

Optional for Org-mode file: `LINK'."
	(let ((gif-image (create-image file))
		  (tmp-buf (get-buffer-create "*Org-mode GIF image animation*")))
	  (switch-to-buffer tmp-buf)
	  (erase-buffer)
	  (insert-image gif-image)
	  (image-animate gif-image nil t)
	  (local-set-key (kbd "q") 'bury-buffer)
	  ))
  (setq org-file-apps '(("\\.png\\'"     . default)
                        (auto-mode       . emacs)
                        (directory       . emacs)
                        ("\\.mm\\'"      . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'"     . emacs)
                        ("\\.md\\'"      . emacs)
                        ("\\.gif\\'"     . my-func/open-and-play-gif-image)
                        ("\\.xlsx\\'"    . default)
                        ("\\.svg\\'"     . default)
                        ("\\.pptx\\'"    . default)
                        ("\\.docx\\'"    . default)))

  :custom
  ;; 启用一些子模块
  (org-modules '(ol-bibtex ol-gnus ol-info ol-eww org-habit org-protocol))
  ;; 在按M-RET时，是否根据光标所在的位置分行，这里设置为是
  ;; (org-M-RET-may-split-line '((default . nil)))
  ;; 一些Org mode自带的美化设置
  ;; 标题行美化
  (org-fontify-whole-heading-line t)
  ;; 设置标题行折叠符号
  (org-ellipsis " ▾")
  ;; 在活动区域内的所有标题栏执行某些命令
  (org-loop-over-headlines-in-active-region t)
  ;; TODO标签美化
  (org-fontify-todo-headline t)
  ;; DONE标签美化
  (org-fontify-done-headline t)
  ;; 引用块美化
  (org-fontify-quote-and-verse-blocks t)
  ;; 隐藏宏标记
  (org-hide-macro-markers t)
  ;; 隐藏强调标签
  (org-hide-emphasis-markers t)
  ;; 高亮latex语法
  (org-highlight-latex-and-related '(native script entities))
  ;; 以UTF-8显示
  (org-pretty-entities t)
  ;; 是否隐藏标题栏的前置星号，这里我们通过org-modern来隐藏
  ;; (org-hide-leading-stars t)
  ;; 当启用缩进模式时自动隐藏前置星号
  (org-indent-mode-turns-on-hiding-stars t)
  ;; 自动启用缩进
  (org-startup-indented nil)
  ;; 根据标题栏自动缩进文本
  (org-adapt-indentation nil)
  ;; 自动显示图片
  (org-startup-with-inline-images t)
  ;; 默认以Overview的模式展示标题行
  (org-startup-folded 'overview)
  ;; 允许字母列表
  (org-list-allow-alphabetical t)
  ;; 列表的下一级设置
  (org-list-demote-modify-bullet '(
								   ("-"  . "+")
                                   ("+"  . "1.")
								   ("1." . "a.")
								   ))
  ;; 编辑时检查是否在折叠的不可见区域
  (org-fold-catch-invisible-edits 'smart)
  ;; 在当前位置插入新标题行还是在当前标题行后插入，这里设置为当前位置
  (org-insert-heading-respect-content nil)
  ;; 设置图片的最大宽度，如果有imagemagick支持将会改变图片实际宽度
  ;; 四种设置方法：(1080), 1080, t, nil
  (org-image-actual-width nil)
  ;; imenu的最大深度，默认为2
  (org-imenu-depth 4)
  ;; 回车要不要触发链接，这里设置不触发
  (org-return-follows-link nil)
  ;; 上标^下标_是否需要特殊字符包裹，这里设置需要用大括号包裹
  (org-use-sub-superscripts '{})
  ;; 复制粘贴标题行的时候删除id
  (org-clone-delete-id t)
  ;; 粘贴时调整标题行的级别
  (org-yank-adjusted-subtrees t)

  ;; TOOD的关键词设置，可以设置不同的组
  (org-todo-keywords '((sequence "TODO(t)" "DOING(i!)" "|" "DONE(d!)")))
  ;; TODO关键词的样式设置
  (org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
							("DOING"        :foreground "#0098dd" :weight bold)
							("DONE"       :foreground "#50a14f" :weight bold)))
  ;; 使用专家模式选择标题栏状态
  (org-use-fast-todo-selection 'expert)
  ;; 父子标题栏状态有依赖
  (org-enforce-todo-dependencies t)
  ;; 标题栏和任务复选框有依赖
  (org-enforce-todo-checkbox-dependencies t)
  ;; 优先级样式设置
  (org-priority-faces '((?A :foreground "red")
						(?B :foreground "orange")
						(?C :foreground "yellow")))
  ;; 标题行全局属性设置
  (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
						   ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
						   ("RISK_ALL" . "Low Medium High")
						   ("STYLE_ALL" . "habit")))
  ;; Org columns的默认格式
  (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  ;; 当状态从DONE改成其他状态时，移除 CLOSED: [timestamp]
  (org-closed-keep-when-no-todo t)
  ;; DONE时加上时间戳
  (org-log-done 'time)
  ;; 重复执行时加上时间戳
  (org-log-repeat 'time)
  ;; Deadline修改时加上一条记录
  (org-log-redeadline 'note)
  ;; Schedule修改时加上一条记录
  (org-log-reschedule 'note)
  ;; 以抽屉的方式记录
  (org-log-into-drawer t)
  ;; 紧接着标题行或者计划/截止时间戳后加上记录抽屉
  (org-log-state-notes-insert-after-drawers nil)

  ;; refile使用缓存
  (org-refile-use-cache t)
  ;; refile的目的地，这里设置的是agenda文件的所有标题
  (org-refile-targets '((org-agenda-files . (:maxlevel . 9))))
  ;; 将文件名加入到路径
  (org-refile-use-outline-path 'file)
  ;; 是否按步骤refile
  (org-outline-path-complete-in-steps nil)
  ;; 允许创建新的标题行，但需要确认
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; 设置标签的默认位置，默认是第77列右对齐
  ;; (org-tags-column -77)
  ;; 自动对齐标签
  (org-auto-align-tags t)
  ;; 标签不继承
  (org-use-tag-inheritance nil)
  ;; 在日程视图的标签不继承
  (org-agenda-use-tag-inheritance nil)
  ;; 标签快速选择
  (org-use-fast-tag-selection t)
  ;; 标签选择不需要回车确认
  (org-fast-tag-selection-single-key t)
  ;; 定义了有序属性的标题行也加上 OREDERD 标签
  (org-track-ordered-property-with-tag t)
  )

(use-package org-modern
  :ensure nil
  :hook (after-init . (lambda ()
                        (setq org-modern-hide-stars 'leading)
                        (global-org-modern-mode t)))
  :config
  ;; 标题行型号字符
  (setq org-modern-star ["◉" "○" "✸" "✳" "◈" "◇" "✿" "❀" "✜"])
  ;; 额外的行间距，0.1表示10%，1表示1px
  (setq-default line-spacing 0.1)
  ;; tag边框宽度，还可以设置为 `auto' 即自动计算
  (setq org-modern-label-border 1)
  ;; 设置表格竖线宽度，默认为3
  (setq org-modern-table-vertical 2)
  ;; 设置表格横线为0，默认为0.1
  (setq org-modern-table-horizontal 0)
  ;; 复选框美化
  (setq org-modern-checkbox
        '((?X . #("▢✓" 0 2 (composition ((2)))))
          (?- . #("▢–" 0 2 (composition ((2)))))
          (?\s . #("▢" 0 1 (composition ((1)))))))
  ;; 列表符号美化
  (setq org-modern-list
        '((?- . "•")
          (?+ . "◦")
          (?* . "▹")))
  ;; 代码块左边加上一条竖边线（需要Org mode顶头，如果启用了 `visual-fill-column-mode' 会很难看）
  (setq org-modern-block-fringe t)
  ;; 代码块类型美化，我们使用了 `prettify-symbols-mode'
  (setq org-modern-block-name nil)
  ;; #+关键字美化，我们使用了 `prettify-symbols-mode'
  (setq org-modern-keyword nil)
  )

(use-package org-appear
  :ensure nil
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t)
  )

(use-package org-src
  :ensure nil
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (("s-l" . show-line-number-in-src-block)
         :map org-src-mode-map
         ("C-c C-c" . org-edit-src-exit))
  :init
  ;; 设置代码块的默认头参数
  (setq org-babel-default-header-args
        '(
          (:eval    . "never-export")     ; 导出时不执行代码块
          (:session . "none")
          (:results . "replace")          ; 执行结果替换
          (:exports . "both")             ; 导出代码和结果
          (:cache   . "no")
          (:noweb   . "no")
          (:hlines  . "no")
          (:wrap    . "results")          ; 结果通过#+begin_results包裹
          (:tangle  . "no")               ; 不写入文件
          ))
  :config
  ;; ==================================
  ;; 如果出现代码运行结果为乱码，可以参考：
  ;; https://github.com/nnicandro/emacs-jupyter/issues/366
  ;; ==================================
  (defun display-ansi-colors ()
    (ansi-color-apply-on-region (point-min) (point-max)))
  (add-hook 'org-babel-after-execute-hook #'display-ansi-colors)

  ;; ==============================================
  ;; 通过overlay在代码块里显示行号，s-l显示，任意键关闭
  ;; ==============================================
  (defvar number-line-overlays '()
    "List of overlays for line numbers.")

  (defun show-line-number-in-src-block ()
    (interactive)
    (save-excursion
      (let* ((src-block (org-element-context))
             (nlines (- (length
                         (s-split
                          "\n"
                          (org-element-property :value src-block)))
                        1)))
        (goto-char (org-element-property :begin src-block))
        (re-search-forward (regexp-quote (org-element-property :value src-block)))
        (goto-char (match-beginning 0))

        (cl-loop for i from 1 to nlines
                 do
                 (beginning-of-line)
                 (let (ov)
                   (setq ov (make-overlay (point) (point)))
                   (overlay-put ov 'before-string (format "%3s | " (number-to-string i)))
                   (add-to-list 'number-line-overlays ov))
                 (next-line))))

    ;; now read a char to clear them
    (read-key "Press a key to clear numbers.")
    (mapc 'delete-overlay number-line-overlays)
    (setq number-line-overlays '()))

  ;; =================================================
  ;; 执行结果后，如果结果所在的文件夹不存在将自动创建
  ;; =================================================
  (defun check-directory-exists-before-src-execution (orig-fun
                                                      &optional arg
                                                      info
                                                      params)
    (when (and (assq ':file (cadr (cdr (org-babel-get-src-block-info))))
               (member (car (org-babel-get-src-block-info)) '("mermaid" "ditaa" "dot" "lilypond" "plantuml" "gnuplot" "d2")))
      (let ((foldername (file-name-directory (alist-get :file (nth 2 (org-babel-get-src-block-info))))))
        (if (not (file-exists-p foldername))
            (mkdir foldername)))))
  (advice-add 'org-babel-execute-src-block :before #'check-directory-exists-before-src-execution)

  ;; =================================================
  ;; 自动给结果的图片加上相关属性
  ;; =================================================
  (setq original-image-width-before-del "400") ; 设置图片的默认宽度为400
  (setq original-caption-before-del "")        ; 设置默认的图示文本为空

  (defun insert-attr-decls ()
    "insert string before babel execution results"
    (insert (concat "\n#+CAPTION:"
                    original-caption-before-del
                    "\n#+ATTR_ORG: :width "
                    original-image-width-before-del
                    "\n#+ATTR_LATEX: :width "
                    (if (>= (/ (string-to-number original-image-width-before-del) 800.0) 1)
                        "1.0"
                      (number-to-string (/ (string-to-number original-image-width-before-del) 800.0)))
                    "\\linewidth :float nil"
                    "\n#+ATTR_HTML: :width "
                    original-image-width-before-del
                    )))

  (defun insert-attr-decls-at (s)
    "insert string right after specific string"
    (let ((case-fold-search t))
      (if (search-forward s nil t)
          (progn
            ;; (search-backward s nil t)
            (insert-attr-decls)))))

  (defun insert-attr-decls-at-results (orig-fun
                                       &optional arg
                                       info
                                       param)
    "insert extra image attributes after babel execution"
    (interactive)
    (progn
      (when (member (car (org-babel-get-src-block-info)) '("mermaid" "ditaa" "dot" "lilypond" "plantuml" "gnuplot" "d2"))
        (setq original-image-width-before-del (number-to-string (if-let* ((babel-width (alist-get :width (nth 2 (org-babel-get-src-block-info))))) babel-width (string-to-number original-image-width-before-del))))
        (save-excursion
          ;; `#+begin_results' for :wrap results, `#+RESULTS:' for non :wrap results
          (insert-attr-decls-at "#+begin_results")))
      (org-redisplay-inline-images)))
  (advice-add 'org-babel-execute-src-block :after #'insert-attr-decls-at-results)

  ;; 再次执行时需要将旧的图片相关参数行删除，并从中头参数中获得宽度参数，参考
  ;; https://emacs.stackexchange.com/questions/57710/how-to-set-image-size-in-result-of-src-block-in-org-mode
  (defun get-attributes-from-src-block-result (&rest args)
    "get information via last babel execution"
    (let ((location (org-babel-where-is-src-block-result))
          ;; 主要获取的是图示文字和宽度信息，下面这个正则就是为了捕获这两个信息
          (attr-regexp "[:blank:]*#\\+\\(ATTR_ORG: :width \\([0-9]\\{3\\}\\)\\|CAPTION:\\(.*\\)\\)"))
      (setq original-caption-before-del "") ; 重置为空
      (when location
        (save-excursion
          (goto-char location)
          (when (looking-at (concat org-babel-result-regexp ".*$"))
            (next-line 2)               ; 因为有个begin_result的抽屉，所以往下2行
            ;; 通过正则表达式来捕获需要的信息
            (while (looking-at attr-regexp)
              (when (match-string 2)
                (setq original-image-width-before-del (match-string 2)))
              (when (match-string 3)
                (setq original-caption-before-del (match-string 3)))
              (next-line)               ; 因为设置了:wrap，所以这里不需要删除这一行
              )
            )))))
  (advice-add 'org-babel-execute-src-block :before #'get-attributes-from-src-block-result)

  :custom
  ;; 代码块语法高亮
  (org-src-fontify-natively t)
  ;; 使用编程语言的TAB绑定设置
  (org-src-tab-acts-natively t)
  ;; 保留代码块前面的空格
  (org-src-preserve-indentation t)
  ;; 代码块编辑窗口的打开方式：当前窗口+代码块编辑窗口
  (org-src-window-setup 'reorganize-frame)
  ;; 执行前是否需要确认
  (org-confirm-babel-evaluate nil)
  ;; 代码块默认前置多少空格
  (org-edit-src-content-indentation 0)
  ;; 代码块的语言模式设置，设置之后才能正确语法高亮
  (org-src-lang-modes '(("C"            . c)
                        ("C++"          . c++)
                        ("bash"         . sh)
                        ("cpp"          . c++)
                        ("elisp"        . emacs-lisp)
                        ("python"       . python)
                        ("shell"        . sh)
                        ("mysql"        . sql)
                        ))
  ;; 在这个阶段，只需要加载默认支持的语言
  (org-babel-load-languages '((python          . t)
                              (awk             . t)
                              (C               . t)
                              (calc            . t)
                              (emacs-lisp      . t)
                              (eshell          . t)
                              (shell           . t)
                              (sql             . t)
                              (css             . t)
                              ))
  )

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
  :mode ("\\.py\\'" . python-mode)
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

(setq org-roam-directory
      (file-truename (or (getenv "MY_ORG_NOTE_DIR") "~/Documents/Notes")))
(org-roam-db-autosync-mode)

(setq org-agenda-files (list org-roam-directory))

;; 显示时间线
(setq org-agenda-use-time-grid t)
;; 设置面包屑分隔符
;; (setq org-agenda-breadcrumbs-separator " ❱ ")
;; 设置时间线的当前时间指示串
(setq org-agenda-current-time-string "⏰------------now")
;; 时间线范围和颗粒度设置
(setq org-agenda-time-grid (quote ((daily today)
				    (0600 0800 1000 1200
					1400 1600 1800
					2000 2200 2400)
				    "......" "----------------")))
;; 对于计划中的任务在视图里的显示
(setq org-agenda-scheduled-leaders
    '("计划 " "应在%02d天前开始 "))
;; 对于截止日期的任务在视图里的显示
(setq org-agenda-deadline-leaders
    '("截止 " "还有%02d天到期 " "已经过期%02d天 "))

(setq org-agenda-prefix-format
      '((agenda . " %i %(vulpea-agenda-category 12)%?-12t% s")
        (todo . " %i %(vulpea-agenda-category 12) ")
        (tags . " %i %(vulpea-agenda-category 12) ")
        (search . " %i %(vulpea-agenda-category 12) ")))

(defun vulpea-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-prop-get "title"))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))

(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(provide 'init-roam)
;;; init-roam.el ends here
