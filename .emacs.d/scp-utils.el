(defun process (cmd)
  (interactive "sShell command: ")
  (let ((buf (get-buffer-create "*Process Output*")))
    (display-buffer buf t)
    (save-excursion
      (save-restriction
	(widen)
	(shell-command-on-region (point-min) (point-max) cmd buf)
	(kill-region (point-min) (point-max)))))
  nil)


(defun 2mm (n)
  (* n 25.4))

(defun 2m (n)
  (* (2mm n) 0.001))
  
(defun 2in (n)
  (/ n 25.4))

(defun 2deg (n) (* n (/ 180.0 pi)))
