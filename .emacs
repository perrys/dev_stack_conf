
(setq user-emacs-directory (directory-file-name "~/.emacs.d"))
(add-to-list 'load-path user-emacs-directory)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)
;(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
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
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape)
(use-package magit)
(use-package undo-fu)

;; make _ part of a word:
(add-hook 'prog-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))

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
(global-set-key (kbd "M-o") 'delete-other-windows)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x _") 'split-window-below)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<up>") 'shrink-window-vertically)
(global-set-key (kbd "M-<down>") 'enlarge-window-vertically)

(evil-set-leader nil (kbd "C-SPC"))
(evil-set-leader '(normal motion visual) (kbd "SPC"))

(defun scp-dired-current-dir ()
  (interactive)
  (let ((dirname (file-name-parent-directory (buffer-file-name (current-buffer)))))
    (dired dirname)))
(evil-define-key 'normal 'global (kbd "<leader>d") 'scp-dired-current-dir)
(evil-define-key '(normal motion visual) 'global (kbd "<leader>wq") 'delete-window)
(evil-define-key '(normal motion visual) 'global (kbd "<leader>wo") 'delete-other-windows)
(evil-define-key '(normal motion visual) 'global (kbd "<leader>h") 'evil-ex-nohighlight)

(defun scp-evil-paste-before (count &optional register)
  (interactive "*P<x>")
  (delete-region (point) (mark))
  (evil-paste-before count register))
(evil-define-key 'normal 'global (kbd "<leader>p") 'scp-evil-paste-before)


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

;; change mode-line color by evil state
(defconst scp-modeline-default-colors (cons (face-background 'mode-line)
                                            (face-foreground 'mode-line)))

(add-hook 'post-command-hook
          (lambda ()
            (let ((color (cond ((minibufferp) scp-modeline-default-colors)
                               ((evil-insert-state-p) '("#fb4934" . "#ebdbb2"))
                               ((evil-emacs-state-p)  '("#b8bb26" . "#3c3836"))
                               ((evil-visual-state-p)  '("#b16286" . "#3c3836"))
                               ((buffer-modified-p)   '("#d79921" . "#3c3836"))
                               (t scp-modeline-default-colors))))
              (set-face-background 'mode-line (car color))
              (set-face-foreground 'mode-line (cdr color)))))

(set-face-background 'default "unspecified-bg")

(defun highlight-selected-window (f)
  "Blacken the background of any window which does not show the same buffer as the selected window"
  (let ((sel-buf (window-buffer (selected-window))))
    (buffer-face-set 'default)
    (walk-windows (lambda (win)
                    (let ((mybuf (window-buffer win)))
                      (unless (eq mybuf sel-buf)
                        (with-current-buffer (window-buffer win)
                          (buffer-face-set '(:background "gray7")))))))))

;(add-hook 'window-state-change-functions
;          'highlight-selected-window)

;; . in visual mode
(defun moon/make-region-search-history ()
  "Make region a histroy so I can use cgn."
  (interactive)
  (let* ((region-beg (region-beginning))
         (region-end (region-end))
         (region (buffer-substring-no-properties region-beg region-end)))
    (push region evil-ex-search-history)
    (setq evil-ex-search-pattern (evil-ex-make-search-pattern region))
    (evil-ex-search-activate-highlight evil-ex-search-pattern)
    (deactivate-mark)))
(define-key evil-visual-state-map (kbd ".") 'moon/make-region-search-history)

