;; Functions which are called from common/frequent hooks, so should be
;; fast. After making changes remmber to recompile this file with:
;;
;; (byte-compile-file (buffer-file-name))

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

(defun scp/display-line-numbers-selected (frame)
  "Display line numbers for the selected window only"
  (let* ((selected-window (frame-selected-window frame))
         (other-windows (seq-remove (lambda (w)
                                      (eq w selected-window))
                                    (window-list frame)))
         (selected-buffer (window-buffer selected-window)))
    (if (not (minibufferp selected-buffer))
        (progn
          (mapc (lambda (window)
                  (let ((buffer (window-buffer window)))
                    (if (buffer-local-value 'display-line-numbers buffer)
                        (with-current-buffer buffer
                          (setq-local display-line-numbers nil)))))
                other-windows)
          (with-current-buffer selected-buffer
            (setq-local display-line-numbers 'relative))))))

(add-to-list 'scp/selected-window-changed-hook
             'scp/display-line-numbers-selected)

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
    (send-string-to-terminal (concat "\e[" cursor " q"))
    (send-string-to-terminal (concat "\e]12;" (car color) "\a"))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

;; Remember to:
;; (byte-compile-file (buffer-file-name))
