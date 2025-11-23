;;  -*- lexical-binding: t; -*-
;; Functions which are called from common/frequent hooks, so should be
;; fast. After making changes remmber to recompile this file with:
;;
;; (byte-compile-file (buffer-file-name))

(require 'face-remap)

(defvar scp/frame-size-changed-hook nil
  "Hooks to run when the frame size changes")

(defun scp/frame-size-changed (frame)
  (if (frame-size-changed-p frame)
      (mapc (lambda (hookfn) (funcall hookfn frame))
            scp/frame-size-changed-hook)))

(defvar scp/selected-window-changed-hook nil
  "Hooks to run when the frame size changes")

(defun scp/selected-window-changed (frame)
  (mapc (lambda (hookfn) (funcall hookfn frame))
        scp/selected-window-changed-hook))

(defvar scp/dim-bg nil
  "Buffer-local holder for background face remapping")
(make-variable-buffer-local 'scp/dim-bg)

(defvar scp/display-line-numbers 'relative
  "Buffer-local holder for display-line-numbers")
(make-variable-buffer-local 'scp/display-line-numbers)
(set-default 'scp/display-line-numbers 'relative)

(defun scp/toggle-line-numbers ()
  (interactive)
  (setq-local scp/display-line-numbers
              (cond
               ((eq scp/display-line-numbers 'relative) t)
               (scp/display-line-numbers nil)
               (t 'relative)))
  (scp/update-focused-window (selected-window)))

(defun scp/update-unfocused-window (minibufp window)
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (when (not minibufp)
        (if (buffer-local-value 'display-line-numbers buffer)
            (setq-local display-line-numbers nil)))
      (unless scp/dim-bg
        (setq-local scp/dim-bg (face-remap-add-relative 'default :background "#131313"))))))

(defun scp/update-focused-window (selected-window)
  (with-current-buffer (window-buffer selected-window)
    (unless (or (minibufferp) (derived-mode-p 'special-mode))
      (setq-local display-line-numbers scp/display-line-numbers))
    (when scp/dim-bg
      (face-remap-remove-relative scp/dim-bg)
      (setq-local scp/dim-bg nil))))

(defun scp/highlight-focused-window (frame)
  "Display line numbers for the selected window only"
  (let* ((selected-window (frame-selected-window frame))
         (other-windows (seq-remove (lambda (w)
                                      (eq w selected-window))
                                    (window-list frame)))
         (selected-buffer (window-buffer selected-window))
         (minibufp (minibufferp selected-buffer))
         (updater (apply-partially #'scp/update-unfocused-window minibufp)))
    (mapc updater other-windows)
    (scp/update-focused-window selected-window)))

(add-to-list 'scp/selected-window-changed-hook
             'scp/highlight-focused-window)

(defconst scp/default-ui (cons (cons (face-background 'mode-line)
                                     (face-foreground 'mode-line))
                               "2"))

;; Change the mode-line color and cursor color/shape by evil state Note, the
;; following cursor escape strings work on tmux running on alacritty; they may
;; be different for other terminals:
(defun scp/update-modeline ()
  (let* ((color-and-cursor (cond ((minibufferp) scp/default-ui)
                                 ((evil-insert-state-p) '(("#fb4934" . "#ebdbb2") . "5"))
                                 ((evil-emacs-state-p)  '(("#b8bb26" . "#3c3836") . "3"))
                                 ((evil-visual-state-p)  '(("#b16286" . "#3c3836") . "2"))
                                 ((buffer-modified-p)   '(("#d79921" . "#3c3836") . "2"))
                                 (t scp/default-ui)
                                 ))
         (color (car color-and-cursor))
         (cursor (cdr color-and-cursor)))
    (unless window-system
      (send-string-to-terminal (concat "\e[" cursor " q"))
      (send-string-to-terminal (concat "\e]12;" (car color) "\a")))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

;; Remember to:
;; (byte-compile-file (buffer-file-name))
