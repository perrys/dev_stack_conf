
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
 '(asm-comment-char 35)
 '(c-basic-offset 4)
 '(column-number-mode t)
 '(css-indent-offset 4)
 '(cursor-type 'bar)
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(custom-safe-themes
   '("8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" default))
 '(dap-lldb-debug-program '("/usr/bin/lldb-dap-18"))
 '(dapdbg-gdb-command-line '("rust-gdb" "-i" "dap"))
 '(dapdbg-lldb-command-line '("lldb-dap-18"))
 '(dapdbg-lldb-init-commands
   '("command script import ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/etc/lldb_lookup.py" "command source ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/etc/lldb_commands"))
 '(dapdbg-lldb-source-mappings '(("/foo/bar" . "/baz")))
 '(default-frame-alist '((width . 120) (height . 50) (menu-bar-lines . 1)))
 '(dired-garbage-files-regexp
   "\\(?:\\.\\(?:aux\\|bak\\|pyc\\|dvi\\|log\\|orig\\|rej\\|toc\\)\\)\\'")
 '(dired-listing-switches "-alGD")
 '(display-buffer-base-action
   '((display-buffer-reuse-window display-buffer-in-previous-window display-buffer-reuse-mode-window display-buffer-use-some-window)
     (mode org-mode c-mode)))
 '(ediff-split-window-function 'split-window-horizontally)
 '(evil-search-module 'evil-search)
 '(fill-column 80)
 '(gdb-debuginfod-enable-setting nil)
 '(gdb-many-windows nil)
 '(gdb-registers-filter-pattern-list
   '("^r[[:alpha:]]\\{2\\}$" "^r[[:digit:]]+$" "^[xyz]mm[[:digit:]]+$" ".*flags.*"))
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
   '(bazel protobuf-mode lua-mode diminish evil-goggles org-roam-ui orderless org-roam yaml projectile consult marginalia vertico evil-collection magit gruvbox-theme flycheck-rust undo-fu evil-escape evil yasnippet company lsp-ui lsp-mode rustic use-package))
 '(safe-local-variable-values
   '((scp/format-on-save . t)
     (scp/delete-trailing-whitespace . t)
     (index-tabs-mode)
     (c-indentation-style . elemetel)))
 '(select-enable-primary t)
 '(split-height-threshold 100)
 '(switch-to-buffer-in-dedicated-window 'pop)
 '(switch-to-buffer-obey-display-actions t)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(truncate-partial-width-windows 85)
 '(visible-bell t)
 '(warning-suppress-types '((comp)))
 '(xterm-mouse-mode t))
;;'(completion-styles '(basic partial-completion emacs22 flex))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "JB" :slant normal :weight normal :height 143 :width normal :inherit nil :extend nil :stipple nil :background "#1d2021" :foreground "#ebdbb2"))))
 '(flycheck-error ((t (:foreground "brightred" :inverse-video nil :box nil))))
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
