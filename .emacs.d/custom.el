
;; GNU Packages installed:
;; company
;; use-package
;; yasnippet
;;
;; MELPA packages:
;; evil
;; evil-escape
;; flycheck-rust
;; lsp-mode
;; lsp-ui
;; rustic
;; undo-fu

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(align-to-tab-stop nil)
 '(asm-comment-char ?#)
 '(column-number-mode t)
 '(css-indent-offset 4)
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(custom-safe-themes
   '("d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" default))
 '(default-frame-alist '((width . 120) (height . 50) (menu-bar-lines . 1)))
 '(dired-garbage-files-regexp
   "\\(?:\\.\\(?:aux\\|bak\\|pyc\\|dvi\\|log\\|orig\\|rej\\|toc\\)\\)\\'")
 '(evil-auto-balance-windows nil)
 '(evil-buffer-regexps '(("^ \\*load\\*") ("COMMIT_EDITMSG" . insert)))
 '(evil-escape-delay 0.2)
 '(evil-escape-key-sequence "jk")
 '(evil-escape-mode t)
 '(evil-mode t)
 '(evil-move-beyond-eol nil)
 '(evil-search-module 'evil-search)
 '(evil-undo-system 'undo-fu)
 '(evil-want-fine-undo t)
 '(fill-column 80)
 '(gdb-many-windows t)
 '(indent-tabs-mode nil)
 '(ispell-dictionary nil)
 '(make-backup-files nil)
 '(mouse-yank-at-point t)
 '(package-selected-packages
   '(evil-collection magit gruvbox-theme flycheck-rust undo-fu evil-escape evil yasnippet company lsp-ui lsp-mode rustic use-package))
 '(safe-local-variable-values '((index-tabs-mode) (c-indentation-style . elemetel)))
 '(server-mode t)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((comp))))
 ;;'(gdb-default-window-configuration-file "gdb-window-cfg.el")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1d2021" :foreground "#ebdbb2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight ultra-light :height 106 :width normal :foundry "JB" :family "JetBrains Mono"))))
 '(font-lock-comment-face ((t (:foreground "#7c6f64" :slant italic))))
 '(header-line ((t (:inherit mode-line-inactive :inverse-video nil :underline t))))
 '(mode-line ((t (:background "#3c7375" :foreground "#ebdbb2" :box (:line-width -1 :style released-button))))))

;; tty=based gruvbox faces:
 ;; '(font-lock-builtin-face ((t (:foreground "brightcyan"))))
 ;; '(font-lock-comment-face ((t (:foreground "brightblack" :slant italic))))
 ;; '(font-lock-constant-face ((t (:foreground "brightred"))))
 ;; '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 ;; '(font-lock-function-name-face ((t (:foreground "brightyellow"))))
 ;; '(font-lock-keyword-face ((t (:foreground "brightred"))))
 ;; '(font-lock-preprocessor-face ((t (:inherit nil :foreground "brightcyan"))))
 ;; '(font-lock-string-face ((t (:foreground "brightgreen"))))
 ;; '(font-lock-type-face ((t (:foreground "brightyellow"))))
 ;; '(font-lock-variable-name-face ((t (:foreground "brightblue"))))
 ;; '(header-line ((t (:inherit mode-line-inactive :inverse-video nil :underline t))))
 ;; '(mode-line ((t (:background "blue" :foreground "white" :box (:line-width -1 :style released-button))))))
