;; Functions which are called from common/frequent hooks, so shoud be
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


;; Remember to:
;; (byte-compile-file (buffer-file-name))
