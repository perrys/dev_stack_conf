
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
 '(column-number-mode t)
 '(css-indent-offset 4)
 '(default-frame-alist '((width . 120) (height . 50) (menu-bar-lines . 1)))
 '(dired-garbage-files-regexp
   "\\(?:\\.\\(?:aux\\|bak\\|pyc\\|dvi\\|log\\|orig\\|rej\\|toc\\)\\)\\'")
 '(display-line-numbers 'relative)
 '(evil-escape-key-sequence "jk")
 '(evil-escape-mode t)
 '(evil-mode t)
 '(evil-search-module 'evil-search)
 '(evil-undo-system 'undo-fu)
 '(indent-tabs-mode nil)
 '(ispell-dictionary nil)
 '(make-backup-files nil)
 '(mouse-yank-at-point t)
 '(package-selected-packages
   '(flycheck-rust undo-fu evil-escape evil yasnippet company lsp-ui lsp-mode rustic use-package))
 '(safe-local-variable-values '((index-tabs-mode) (c-indentation-style . elemetel)))
 '(server-mode t)
 '(tool-bar-mode nil)
 '(tramp-default-method-alist
   '((nil "%" "smb")
     ("" "\\`\\(anonymous\\|ftp\\)\\'" "ftp")
     ("\\`ftp\\." "" "ftp")
     ("\\`localhost\\'" "\\`root\\'" "su")
     ("\\`dbsquash\\." "" "ftp")))
 '(warning-suppress-types '((comp) (comp)))
 '(which-function-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
