;;; init-base.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; 禁用一些GUI特性
(setq use-dialog-box nil)               ; 鼠标操作不使用对话框
(setq inhibit-default-init t)           ; 不加载 `default' 库
(setq inhibit-startup-screen t)         ; 不加载启动画面
(setq inhibit-startup-message t)        ; 不加载启动消息
(setq inhibit-startup-buffer-menu t)    ; 不显示缓冲区列表

;; 设置窗口大小，仅仅在图形界面需要设置
(when (display-graphic-p)
  (let ((top    0)                                     ; 顶不留空
        (left   (/ (x-display-pixel-width) 10))        ; 左边空10%
        (height (round (* 0.8                          ; 窗体高度为0.8倍的显示高度
                          (/ (x-display-pixel-height)
                             (frame-char-height))))))
    (let ((width  (round (* 2.5 height))))             ; 窗体宽度为2.5倍高度
      (setq default-frame-alist nil)
      (add-to-list 'default-frame-alist (cons 'top top))
      (add-to-list 'default-frame-alist (cons 'left left))
      (add-to-list 'default-frame-alist (cons 'height height))
      (add-to-list 'default-frame-alist (cons 'width width)))))

(electric-pair-mode t)                       ; 自动补全括号
(column-number-mode t)                       ; 在 Mode line 上显示列号
(global-auto-revert-mode t)                  ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(delete-selection-mode t)                    ; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
(setq make-backup-files nil)                 ; 关闭文件自动备份
(add-hook 'prog-mode-hook #'show-paren-mode) ; 编程模式下，光标在括号上时高亮另一个括号
(add-hook 'prog-mode-hook #'hs-minor-mode)   ; 编程模式下，可以折叠代码块
(global-display-line-numbers-mode 1)         ; 在 Window 显示行号
(tool-bar-mode -1)                           ; 关闭 Tool bar
(when (display-graphic-p) (toggle-scroll-bar -1)) ; 图形界面时关闭滚动条
(icomplete-mode 1)

(set-face-attribute 'default nil :height 160 :font "Cascadia Code")
(set-fontset-font "fontset-default" 'han "LXGW WenKai")

(setq display-line-numbers-type 'relative)   ; （可选）显示相对行号

(provide 'init-base)

;;; init-base.el ends here
