
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

(defun scp/debug-display-window (buf config)
  (message (format "buffer: %s, config: %s" buf config))
  nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(align-to-tab-stop nil)
 '(asm-comment-char ?#)
 '(c-basic-offset 4)
 '(column-number-mode t)
 '(css-indent-offset 4)
 '(cursor-type 'bar)
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(custom-safe-themes
   '("d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" default))
 '(default-frame-alist '((width . 120) (height . 50) (menu-bar-lines . 1)))
 '(dired-garbage-files-regexp
   "\\(?:\\.\\(?:aux\\|bak\\|pyc\\|dvi\\|log\\|orig\\|rej\\|toc\\)\\)\\'")
 '(dired-listing-switches "-alGD")
 '(display-buffer-base-action
   '((display-buffer-reuse-window display-buffer-in-previous-window display-buffer-reuse-mode-window display-buffer-use-some-window)
     (mode org-mode c-mode)))
 '(display-line-numbers 'relative)
 '(ediff-split-window-function 'split-window-horizontally)
 '(evil-search-module 'evil-search)
 '(fill-column 80)
 '(gdb-many-windows t)
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(mouse-yank-at-point t)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(package-selected-packages
   '(protobuf-mode lua-mode diminish evil-goggles org-roam-ui orderless org-roam yaml-mode projectile consult marginalia vertico evil-collection magit gruvbox-theme flycheck-rust undo-fu evil-escape evil yasnippet company lsp-ui lsp-mode rustic use-package))
 '(safe-local-variable-values '((index-tabs-mode) (c-indentation-style . elemetel)))
 '(select-enable-primary t)
 '(server-mode t)
 '(split-height-threshold 100)
 '(switch-to-buffer-in-dedicated-window 'pop)
 '(switch-to-buffer-obey-display-actions t)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(visible-bell t)
 '(warning-suppress-types '((comp)))
 '(window-sides-slots '(1 0 0 1)))
;;'(gdb-default-window-configuration-file "gdb-window-cfg.el")
;;'(completion-styles '(basic partial-completion emacs22 flex))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1d2021" :foreground "#ebdbb2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight ultra-light :height 106 :width normal :foundry "JB" :family "JetBrains Mono"))))
 '(flycheck-error ((t (:foreground "brightred" :inverse-video nil :box nil :underline (:color "#fb4933" :style wave :position nil)))))
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
