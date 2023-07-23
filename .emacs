
(setq user-emacs-directory (directory-file-name "~/.emacs.d"))
(add-to-list 'load-path user-emacs-directory)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)
;(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

(load custom-file)
(load-library "rust-setup.el") 

(global-set-key [f3] 'shell)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; change mode-line color by evil state
(defconst scp-modeline-default-colors (cons (face-background 'mode-line)
                           (face-foreground 'mode-line)))
(add-hook 'post-command-hook
          (lambda ()
            (let ((color (cond ((minibufferp) scp-modeline-default-colors)
                               ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                               ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                               ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                               (t scp-modeline-default-colors))))
              (set-face-background 'mode-line (car color))
              (set-face-foreground 'mode-line (cdr color)))))
