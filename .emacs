
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

(use-package evil)
(use-package evil-escape)
(use-package undo-fu)
(evil-mode t)

(load-library "rust-setup.el") 

(load custom-file)

(global-set-key [f3] 'shell)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq tramp-default-method "ssh")

;; change mode-line color by evil state
(defconst scp-modeline-default-colors (cons (face-background 'mode-line)
                                            (face-foreground 'mode-line)))

(add-hook 'post-command-hook
          (lambda ()
            (let ((color (cond ((minibufferp) scp-modeline-default-colors)
                               ((evil-insert-state-p) '("brightred" . "white"))
                               ((evil-emacs-state-p)  '("brightgreen" . "black"))
                               ((evil-visual-state-p)  '("magenta" . "black"))
                               ((buffer-modified-p)   '("yellow" . "black"))
                               (t scp-modeline-default-colors))))
              (set-face-background 'mode-line (car color))
              (set-face-foreground 'mode-line (cdr color)))))
