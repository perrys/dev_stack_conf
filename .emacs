;;  -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (file-name-concat user-emacs-directory "gdbx"))


;; ------------------- packages ---------------------


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package bazel
  :ensure t
  )

(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.2) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
        ("M-j". company-select-next)
        ("M-k". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

(use-package diminish
  :ensure t
  :config
  ;; most diminish config in the next section
  (diminish 'abbrev-mode ""))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :custom
  (evil-auto-balance-windows nil)
  (evil-buffer-regexps
   '(("^ \\*load\\*")
     ("COMMIT_EDITMSG" . insert)
     ("CAPUTRE.*" . insert)))
  (evil-move-beyond-eol nil)
  (evil-search-module 'evil-search)
  (evil-undo-system 'undo-fu)
  (evil-want-fine-undo t)
  :config
  (evil-mode t))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape
  :after evil
  :ensure t
  :custom
  (evil-escape-delay 0.2)
  (evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode t))

(use-package evil-goggles
  :after evil
  :ensure t
  :config
  (evil-goggles-mode))

(use-package flycheck
  :ensure t)

(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
              ("M-?" . lsp-find-references)
              ([f5] . lsp-execute-code-action)
              ([f6] . lsp-rename)
              ([f7] . lsp-find-type-definition))
  :commands lsp
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :custom
  (lsp-idle-delay 0.2)
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-binding-mode-hints t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-rust-analyzer-lens-enable nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-delay 0.2)
  ;; (lsp-ui-doc-enable nil) ; disable ui-doc entirely
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-enable nil) ; disable the sideline entirely
  (lsp-ui-sideline-delay 1.0)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil))

(use-package magit
  :ensure t
  )

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-anotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org-roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-enable))

(require 'project)

(use-package rustic
  ;; From https://robert.kra.hn/posts/rust-emacs-setup/
  :if (package-installed-p 'rustic)
  :bind (:map rustic-mode-map
              ("M-?" . lsp-find-references)
              ([f5] . lsp-execute-code-action)
              ([f6] . lsp-rename)
              ([f7] . lsp-ui-doc-glance)
              ([f8] . lsp-find-type-definition)
              ([f9] . flycheck-list-errors)
              ("M-SPC" . lsp-inlay-hints-mode)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t))

(use-package undo-fu
  :ensure t
  )

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              :map minibuffer-local-map
              ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle nil)
  :init
  (vertico-mode)
  ;; see https://github.com/minad/vertico?tab=readme-ov-file#completion-styles-and-tab-completion
  (keymap-set vertico-map "TAB" #'minibuffer-complete))

(use-package yaml
  :ensure t
  )

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(require 'gdbx)


;; ------------------- other settings ---------------------


;; it's easiest to just run these after all package loads
(diminish 'company-mode "")
(diminish 'evil-collection-unimpaired-mode "")
(diminish 'evil-escape-mode "")
(diminish 'evil-goggles-mode "")
(diminish 'yas-minor-mode "")

(load custom-file)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(unless window-system
  (set-face-background 'default "unspecified-bg"))


;; ------------------- function definitions ---------------------


(defun scp/add-region-to-search-history ()
  "Add region to the search history so I can use cgn."
  (interactive)
  (let ((region (buffer-substring-no-properties (point) (mark))))
    (push region evil-ex-search-history)
    (setq evil-ex-search-pattern (evil-ex-make-search-pattern region))
    (evil-ex-search-activate-highlight evil-ex-search-pattern)
    (deactivate-mark)))

(defun scp/buffer-file-menu (&optional ARG)
  "Bring up the buffer menu, by default WITHOUT non-file buffers)"
  (interactive "P")
  (if ARG
      (buffer-menu)
    (buffer-menu t)))

(defun scp/call-func-maybe-project (regular-func project-func)
  (let ((func (if (project-current)
                  project-func
                regular-func)))
    (call-interactively func)))

(defun scp/cycle-list (l)
  (cons (car (last l)) (butlast l)))

(defun scp/display-buffer-same-window (buffer alist)
  (let ((save-val (window-dedicated-p))
        (win (selected-window)))
    (set-window-dedicated-p win nil)
    (display-buffer-same-window buffer alist)
    (set-window-dedicated-p win save-val)))

(defun scp/display-buffer-here (&optional bufname)
  "Display the selected buffer in the current window"
  (interactive "b")
  (let ((display-buffer-overriding-action
         (cons 'scp/display-buffer-same-window
               nil)))
    (display-buffer bufname)))

(defun scp/display-left-side (&optional buffer)
  "Display the selected buffer in a side window at the left"
  (interactive "b")
  (unless buffer
    (setq buffer (buffer-name)))
  (display-buffer
   buffer
   (cons 'display-buffer-in-side-window
         '((window-width . 84)
           (side . left)
           (slot . 0)))))

(defun scp/display-bottom-side (&optional buffer)
  "Display the selected buffer in a side window at the bottom"
  (interactive "b")
  (unless buffer
    (setq buffer (buffer-name)))
  (display-buffer
   buffer
   (cons 'display-buffer-in-side-window
         '((window-height . 0.25)
           (side . bottom)
           (slot . 0)))))

(defun scp/enlarge-window-vertically (delta)
  "Make selected window DELTA columns taller."
  (interactive "p")
  (enlarge-window delta nil))

(defun scp/evil-ex-start-search-with-region ()
  "If there is a visual selection, make it the default search term"
  (unless (minibufferp)
    (error "minibuffer not active"))
  (let ((selection
         (with-current-buffer (other-buffer (current-buffer) 1)
           (when (evil-visual-state-p)
             (let ((selection (buffer-substring-no-properties (region-beginning) (+ 1 (region-end)))))
               (evil-normal-state)
               selection)))))
    (when selection
      (evil-ex-remove-default)
      (insert selection)
      (evil-ex-search-activate-highlight (list selection
                                               evil-ex-search-count
                                               evil-ex-search-direction)))))

(advice-add #'evil-ex-search-start-session :after #'scp/evil-ex-start-search-with-region)

(defun scp/evil-paste-before (count &optional register)
  "Override the region without adding it into the register"
  (interactive "*P<x>")
  (delete-region (region-beginning) (region-end))
  (evil-paste-before count register))

(defun scp/evil-scroll-down-and-center ()
  "Center screen on cursor after scrolling"
  (interactive)
  (evil-scroll-down 0)
  (evil-scroll-line-to-center nil)
  nil)

(defun scp/evil-scroll-up-and-center ()
  "Center screen on cursor after scrolling"
  (interactive)
  (evil-scroll-up 0)
  (evil-scroll-line-to-center nil))

(defun scp/evil-send-leader ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (push '(t . leader) unread-command-events))

(defun scp/find-file-maybe-project ()
  "Proxy for find-file which will use project-find-file if there is a project"
  (interactive)
  (scp/call-func-maybe-project #'find-file #'project-find-file))

(defun scp/list-buffers-maybe-project ()
  "Proxy for buffer-menu which will use project-list-buffers if there is a project"
  (interactive)
  (scp/call-func-maybe-project #'scp/buffer-file-menu #'project-list-buffers))

(defun scp/list-equal (l)
  (= (length (seq-uniq l)) 1))

(define-minor-mode scp/local-org-roam-mode
  "Local version of `org-roam-mode'.
Does nothing, can be used for local keybindings."
  :init-value nil
  :global nil
  :lighter " OR local"
  :keymap  (let ((map (make-sparse-keymap)))
             map)
  :group 'org-roam
  :require 'org-roam
  (when scp/local-org-roam-mode
    (message "Local keybindings for Org Roam enabled")))

(defvar scp/format-on-save nil
  "Controls if formatting is applied when saving a buffer.")

(defun scp/format-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (cond
   ((derived-mode-p 'emacs-lisp-mode)
    (let ((saved-point (point)))
      (indent-region (point-min) (point-max))
      (goto-char saved-point)))
   ((or (derived-mode-p 'c-mode)
        (derived-mode-p 'c++-mode))
    (let ((saved-point (point))
          (cmd "clang-format")
          (args `("--style=file" ,(concat "--assume-filename=" (file-name-nondirectory (buffer-file-name))))))
      (message (apply #'concat cmd " " args))
      (save-restriction
        (widen)
        (apply #'call-process-region (point-min) (point-max) cmd t t nil args))
      (goto-char saved-point))))
  t)

(defun scp/maybe-format-buffer ()
  (interactive)
  (when scp/format-on-save
    (scp/format-buffer)))

(defun scp/non-side-window-list ()
  (seq-filter (lambda (win) (not (window-parameter win 'window-side)))
              (window-list)))

(defun scp/org-roam-link-word-at-point ()
  (interactive)
  (when (word-at-point t)
    (re-search-backward "\\b")
    (mark-word)
    (call-interactively #'scp/org-roam-node-insert-immediate)))

(defun scp/org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun scp/org-roam-open-or-link-at-point ()
  (interactive)
  (let ((context (org-element-context)))
    (if (equal (car context) 'link)
        (org-open-at-point)
      (scp/org-roam-link-word-at-point))))

(defun scp/shrink-window-vertically (delta)
  "Make selected window DELTA columns shorter."
  (interactive "p")
  (shrink-window delta nil))

(defun scp/window-cycle ()
  "Rotate buffers of (non-side) windows"
  (interactive)
  (let ((non-side-windows (scp/non-side-window-list)))
    (if (> (length non-side-windows) 1)
        (let* ((cycled-buffer-list (scp/cycle-list (mapcar 'window-buffer non-side-windows)))
               (pairs (cl-pairlis non-side-windows cycled-buffer-list)))
          (mapc (lambda (pair)
                  (set-window-buffer (car pair) (cdr pair)))
                pairs)))))

(defun scp/window-split-toggle ()
  "Toggle between horizontal and vertical split with two (non-side) windows"
  (interactive)
  (let ((non-side-windows (scp/non-side-window-list)))
    (if (not (= (length non-side-windows) 2))
        (error "Can only toggle 2 windows!"))
    (save-selected-window
      (if (not (memq (selected-window) non-side-windows))
          (select-window (car non-side-windows)))
      (let* ((window-left-edges (mapcar 'car (mapcar 'window-edges non-side-windows)))
             (func (if (scp/list-equal window-left-edges)
                       #'split-window-horizontally
                     #'split-window-vertically))
             (non-side-buffer-list (mapcar 'window-buffer non-side-windows)))
        (delete-window (cadr non-side-windows))
        (funcall func)
        (other-window 1)
        (switch-to-buffer (cadr non-side-buffer-list))))))


;; ------------------- hooks ---------------------

(add-hook 'c++-mode-hook 'lsp)
(add-hook 'before-save-hook 'scp/maybe-format-buffer)

(add-hook 'dired-mode-hook (lambda () (evil-define-key 'normal 'local (kbd "SPC") 'scp/evil-send-leader)))
;;(add-hook 'org-roam-mode-hook (lambda () (evil-define-key 'normal 'local (kbd "SPC") 'scp/evil-send-leader)))

;; make _ part of a word:
(add-hook 'prog-mode-hook
          (lambda ()
            (hl-line-mode 1)
            (modify-syntax-entry ?_ "w")))

(load-file (file-name-concat user-emacs-directory "fast-hooks.elc"))

(add-to-list
 'window-selection-change-functions
 'scp/selected-window-changed)

(add-to-list
 'window-size-change-functions
 'scp/frame-size-changed)

(add-hook 'post-command-hook
          'scp/update-modeline)

(add-to-list
 'display-buffer-alist
 '((lambda (buffer-name config)
     (or
      (string-match-p "\\*Help\\*" buffer-name)
      (string-match-p "\\*info\\*" buffer-name)
      (string-match-p "\\*org-roam\\*" buffer-name)
      (with-current-buffer buffer-name
	(or (eq major-mode 'dired-mode)
	    (derived-mode-p 'tabulated-list-mode)
            (and (not (derived-mode-p 'magit-mode))
	         (not (derived-mode-p 'backtrace-mode))
	         (derived-mode-p 'special-mode))))))
   (display-buffer-in-side-window)
   (side . left)
   (slot . 1)
   (window-width . 84))
 t)

(add-to-list
 'display-buffer-alist
 '((lambda (buffer-name config)
     (or (string-match-p "\\*e?shell\\*" buffer-name)
	 (with-current-buffer buffer-name
	   (derived-mode-p 'compilation-mode))))
   (display-buffer-in-side-window)
   (side . bottom)
   (slot . 0)
   (window-height . 0.25)))

;; don't let ediff pop up new frames:
(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(advice-add 'ediff-window-display-p :override #'ignore)


;; ------------------- keybindings ---------------------


(global-set-key [f3] 'eshell)

;; these were 'kill-sentence 'default-indent-new-line 'mark-paragraph and 'downcase-word
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)

;; these were undefined
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x _") 'split-window-below)
(global-set-key (kbd "M-<up>") 'scp/shrink-window-vertically)
(global-set-key (kbd "M-<down>") 'scp/enlarge-window-vertically)
(global-set-key (kbd "M-<delete>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<insert>") 'enlarge-window-horizontally)

;; these were 'balance-windows and 'shrink-window-if-larger-than-buffer
(global-set-key (kbd "C-x +") 'scp/window-split-toggle)
(global-set-key (kbd "C-x -") 'scp/window-cycle)

(evil-define-key '(normal motion visual) 'global (kbd "SPC") nil)
(evil-define-key '(normal motion visual) 'global (kbd "RET") nil)

(evil-set-leader nil (kbd "C-SPC"))
(evil-set-leader '(normal visual motion) (kbd "SPC"))
(evil-global-set-key 'normal (kbd "<leader>p") 'scp/evil-paste-before)

(evil-define-key '(visual) 'global (kbd ".") 'scp/add-region-to-search-history)

(evil-define-key '(normal motion visual) 'global (kbd "C-f") 'scp/evil-scroll-down-and-center)
(evil-define-key '(normal motion visual) 'global (kbd "C-b") 'scp/evil-scroll-up-and-center)

(evil-define-key '(normal motion visual) 'global (kbd "<leader>s") 'scp/list-buffers-maybe-project)
(evil-define-key '(normal motion visual) 'global (kbd "<leader>f") 'scp/find-file-maybe-project)
(evil-define-key '(normal motion visual) 'global (kbd "<leader>d") 'dired)
(evil-define-key '(normal motion visual) 'global (kbd "<leader>h") 'evil-ex-nohighlight)

(evil-define-key '(normal motion visual) 'prog-mode-map (kbd "<leader>x") 'next-error)
(evil-define-key '(normal motion visual) 'prog-mode-map (kbd "<leader>b") 'compile)
(evil-define-key '(normal motion visual) 'rustic-mode-map (kbd "<leader>b") 'rustic-compile)

(evil-define-key '(normal motion visual) 'prog-mode-map (kbd "<leader>R") 'lsp-find-definition)
(evil-define-key '(normal motion visual) 'prog-mode-map (kbd "<leader>r") 'lsp-find-references)

(evil-define-key '(normal motion visual) 'c++-mode-map (kbd "<leader>o") 'projectile-find-other-file-other-window)

(evil-define-key '(normal motion visual) 'org-roam-mode-map (kbd "<leader>l") 'org-roam-buffer-toggle)
(evil-define-key '(normal motion visual) 'org-roam-mode-map (kbd "<leader>f") 'org-roam-node-find)
(evil-define-key '(normal motion visual) 'org-roam-mode-map (kbd "<leader>i") 'org-roam-node-insert)
(evil-define-key '(normal motion visual) 'org-roam-mode-map (kbd "<leader>I") 'scp/org-roam-node-insert-immediate)

;; was 'org-promote-subtree 'org-demote-subtree 'outline-move-subtree-down 'outline-move-subtree-up
(evil-define-key '(normal motion visual) 'scp/local-org-roam-mode-map (kbd "M-h") 'windmove-left)
(evil-define-key '(normal motion visual) 'scp/local-org-roam-mode-map (kbd "M-l") 'windmove-right)
(evil-define-key '(normal motion visual) 'scp/local-org-roam-mode-map (kbd "M-j") 'windmove-down)
(evil-define-key '(normal motion visual) 'scp/local-org-roam-mode-map (kbd "M-k") 'windmove-up)

(evil-define-key '(normal motion visual) 'prog-mode-map (kbd "{") 'beginning-of-defun-comments)
(evil-define-key '(normal motion visual) 'prog-mode-map (kbd "}") 'end-of-defun)

(define-key scp/local-org-roam-mode-map  [remap evil-ret] 'scp/org-roam-open-or-link-at-point)
