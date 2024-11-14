(require 'gdb-mi)

(defcustom gdb-address-format "0x%012x"
  "Format for addresses to be printed in various panels.

The default is 12 hex characters (zero-padded), which is enough
to display 48-bit addresses. You may need to widen this for some
architectures."
  :group 'gdb
  :type 'natnum)

(define-derived-mode gdb-tabulated-list-mode tabulated-list-mode "GDB Tabulated")

(define-derived-mode gdb-disassembly-mode gdb-tabulated-list-mode "Disassembly"
  "Major mode for GDB disassembly information."
  ;; TODO Rename overlay variable for disassembly mode
  (add-to-list 'overlay-arrow-variable-list 'gdb-disassembly-position)
  (setq fringes-outside-margins t)
  (setq-local font-lock-defaults '(gdb-disassembly-font-lock-keywords))
  (setq-local gdb-disassembly-position (make-marker))
  (setq-local gdb-addr-to-bol-table (make-hash-table :test 'eql))
  (setq-local gdb-marker-overlay (make-overlay 0 0))
  (overlay-put gdb-marker-overlay 'face '(:inverse-video t))
  (let ((addr-width (length (format gdb-address-format 0))))
    (setq tabulated-list-format
          (vector `("Address" ,(if (> addr-width 1) addr-width 16) nil)
                  '("Func Name" 10 nil)
                  '("Offset" 8 nil :right-align t :pad-right 2)
                  '("Instruction" 999 nil)))
    (tabulated-list-init-header))
  'gdb-invalidate-disassembly)

(defun scp/gdb-function-buffer-p (buffer)
  "Return t if BUFFER is a GDB function buffer."
  (with-current-buffer buffer
    (derived-mode-p 'gdb-parent-mode 'gdb-tabulated-list-mode 'gdb-inferior-io-mode)))

(advice-add #'gdb-function-buffer-p :override #'scp/gdb-function-buffer-p)

(defun scp/gdb-invalidate-disassembly (signal-symbol)
  "If the address of the current frame's $pc is already in our
   buffer we can skip the GDB interaction and re-render steps,
   and just move the marker"
  (catch 'shortcut
    (when (eq signal-symbol 'update-disassembly)
      (let* ((frame (gdb-current-buffer-frame))
             (addr (gdb-parse-address (gdb-mi--field frame 'addr)))
             (bol-point (gethash addr gdb-addr-to-bol-table)))
        (when bol-point
          (gdb-disassembly-set-marker bol-point)
          (throw 'shortcut t))))
    nil))

(advice-add #'gdb-invalidate-disassembly :before-until #'scp/gdb-invalidate-disassembly)

(defun gdb-parse-address (strval)
  "Parse the given string into a number, accounting for hex numbers with leading `0x'."
  (when strval
    (let  ((s-val (if (string= (downcase (substring strval 0 2)) "0x")
                      (substring strval 2)
                    strval)))
      (string-to-number s-val 16))))

(defun scp/safe-propertize (s &rest props)
  (if s (apply #'propertize s props) ""))

(defun scp/gdb-render-disassembly-line (instr)
  (let ((addr (gdb-parse-address (gdb-mi--field instr 'address)))
        (func-name (scp/safe-propertize (gdb-mi--field instr 'func-name) 'face 'font-lock-function-name-face))
        (offset (format "+%s" (gdb-mi--field instr 'offset)))
        (instr (gdb-mi--field instr 'inst)))
    (list addr (vector (format gdb-address-format addr) func-name offset instr))))

(defun gdb-disassembly-set-marker (marker-point)
  (save-excursion
    (goto-char marker-point)
    (let ((window (get-buffer-window (current-buffer) 0)))
      (set-window-point window marker-point)
      (set-marker gdb-disassembly-position marker-point)
      (move-overlay gdb-marker-overlay marker-point (line-end-position)))))

(defun scp/gdb-disassembly-handler-custom ()
  (clrhash gdb-addr-to-bol-table)
  (let ((instructions (gdb-mi--field (gdb-mi--partial-output) 'asm_insns))
        (address (gdb-parse-address (gdb-mi--field (gdb-current-buffer-frame) 'addr)))
        (marker-point nil))
    (let ((rows (mapcar #'scp/gdb-render-disassembly-line instructions))
          (tabulated-list-printer (lambda (id-addr columns)
                                    (puthash id-addr (point) gdb-addr-to-bol-table)
                                    (when (eql id-addr address)
                                      (setq marker-point (point)))
                                    (tabulated-list-print-entry id-addr columns))))
      (setq tabulated-list-entries rows)
      (tabulated-list-print))
    (gdb-disassembly-place-breakpoints)
    ;; Mark current position with overlay arrow and scroll window to
    ;; that point
    (when marker-point
      (gdb-disassembly-set-marker marker-point))
    (setq mode-name
          (gdb-current-context-mode-name
           (concat "Disassembly: "
                   (gdb-mi--field (gdb-current-buffer-frame) 'func))))))

(advice-add #'gdb-disassembly-handler-custom :override #'scp/gdb-disassembly-handler-custom)

(defun gdb-disassembly-place-breakpoints ()
  (gdb-remove-breakpoint-icons (point-min) (point-max))
  (dolist (breakpoint gdb-breakpoints-list)
    (let* ((breakpoint (cdr breakpoint))
           (bptno (gdb-mi--field breakpoint 'number))
           (flag (gdb-mi--field breakpoint 'enabled))
           (address (gdb-parse-address (gdb-mi--field breakpoint 'addr)))
           (bol (gethash address gdb-addr-to-bol-table)))
      (when bol
        (save-excursion
          (goto-char bol)
          (gdb-put-breakpoint-icon (string-equal flag "y") bptno))))))

(defun gdb-cfg-setup-many-windows ()
  (interactive)
  (add-to-list
   'display-buffer-alist
   '("\\*gud-*"
     (display-buffer-in-side-window)
     (side . left)
     (slot . 0)))
  (add-to-list
   'display-buffer-alist
   '((derived-mode . gdb-frames-mode)
     (display-buffer-in-side-window)
     (side . left)
     (slot . 2)))
  (add-to-list
   'display-buffer-alist
   `((derived-mode . gdb-memory-mode)
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 1)))
  (add-to-list
   'display-buffer-alist
   `((derived-mode . gdb-disassembly-mode)
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 2)))
  (add-to-list
   'display-buffer-alist
   '((derived-mode . gdb-breakpoints-mode)
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 3)))
  (add-to-list
   'display-buffer-alist
   `("\\*locals of*"
     (display-buffer-in-side-window)
     (side . right)
     (slot . 1)))
  )
(gdb-cfg-setup-many-windows)

(defun scp/gdb-ui ()
  (interactive)
  (gdb-display-breakpoints-buffer)
  (gdb-display-disassembly-buffer)
  (gdb-display-locals-buffer)
  (gdb-display-stack-buffer))


(provide 'gdb-cfg)
