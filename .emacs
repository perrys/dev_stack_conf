
(setq user-emacs-directory (directory-file-name "~/.emacs.d"))
(add-to-list 'load-path user-emacs-directory)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape)
(use-package magit)
(use-package undo-fu)
(use-package vertico
  :config
  (vertico-mode 1))
(use-package marginalia
  :config
  (marginalia-mode 1))
(use-package yaml)

(load-library "rust-setup.el") 

(load custom-file)

(defun enlarge-window-vertically (delta)
  "Make selected window DELTA columns taller."
  (interactive "p")
  (enlarge-window delta nil))

(defun shrink-window-vertically (delta)
  "Make selected window DELTA columns shorter."
  (interactive "p")
  (shrink-window delta nil))

(global-set-key [f3] 'shell)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x _") 'split-window-below)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<up>") 'shrink-window-vertically)
(global-set-key (kbd "M-<down>") 'enlarge-window-vertically)

(evil-set-leader nil (kbd "C-SPC"))
(evil-set-leader '(normal visual motion) (kbd "SPC"))
(defun scp/evil-send-leader ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (push '(t . leader) unread-command-events))
(add-hook 'dired-mode-hook (lambda () (evil-define-key 'normal 'local (kbd "SPC") 'scp/evil-send-leader)))

(evil-define-key 'normal 'global (kbd "<leader>d") 'dired)
(evil-define-key '(normal motion visual) 'global (kbd "<leader>s") 'ispell-word)
(evil-define-key '(normal motion visual) 'global (kbd "<leader>h") 'evil-ex-nohighlight)
(evil-define-key '(normal motion visual) 'prog-mode-map (kbd "<leader>x") 'next-error)
(evil-define-key '(normal motion visual) 'prog-mode-map (kbd "<leader>b") 'compile)
(evil-define-key '(normal motion visual) 'rustic-mode-map (kbd "<leader>b") 'rustic-compile)
(evil-define-key '(normal motion visual) 'prog-mode-map (kbd "<leader>R") 'lsp-find-definition)
(evil-define-key '(normal motion visual) 'prog-mode-map (kbd "<leader>r") 'lsp-find-references)

(defun scp-evil-paste-before (count &optional register)
  (interactive "*P<x>")
  (delete-region (point) (mark))
  (evil-paste-before count register))
(evil-define-key 'normal 'global (kbd "<leader>p") 'scp-evil-paste-before)

;; make _ part of a word:
(add-hook 'prog-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq tramp-default-method "ssh")

(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode)
            (setq display-line-numbers 'relative)))

(defun scp-toggle-line-number-display ()
    "Toggle between absolute and relative line numbers"
  (interactive)
  (if (eq display-line-numbers 'relative) 
      (setq display-line-numbers t)
    (setq display-line-numbers 'relative)))

(defun evil-scroll-down-and-center ()
  "Center screen on cursor after scrolling"
  (interactive)
  (evil-scroll-down 0)
  (evil-scroll-line-to-center nil)
  nil)

(defun evil-scroll-up-and-center ()
  "Center screen on cursor after scrolling"
  (interactive)
  (evil-scroll-up 0)
  (evil-scroll-line-to-center nil))

(evil-global-set-key 'motion (kbd "C-f") 'evil-scroll-down-and-center)
(evil-global-set-key 'motion (kbd "C-b") 'evil-scroll-up-and-center)

;; Change the mode-line color and cursor color/shape by evil state Note, the
;; following cursor escape strings work on tmux running on alacritty; they may
;; be different for other terminals:
(defconst scp-default-ui (cons (cons (face-background 'mode-line)
                                            (face-foreground 'mode-line))
                                            "2"))

(add-hook 'post-command-hook
          (lambda ()
            (let* ((color-and-cursor (cond ((minibufferp) scp-default-ui)
                               ((evil-insert-state-p) '(("#fb4934" . "#ebdbb2") . "5"))
                               ((evil-emacs-state-p)  '(("#b8bb26" . "#3c3836") . "3"))
                               ((evil-visual-state-p)  '(("#b16286" . "#3c3836") . "2"))
                               ((buffer-modified-p)   '(("#d79921" . "#3c3836") . "2"))
                               (t scp-default-ui)
                               ))
                   (color (car color-and-cursor))
                   (cursor (cdr color-and-cursor)))
              (send-string-to-terminal (concat "\e[" cursor " q"))
              (send-string-to-terminal (concat "\e]12;" (car color) "\a"))
              (set-face-background 'mode-line (car color))
              (set-face-foreground 'mode-line (cdr color)))))

(unless window-system
  (set-face-background 'default "unspecified-bg"))

(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows"
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))
(global-set-key (kbd "C-x +") 'window-split-toggle)
