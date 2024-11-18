(require 'gdb-mi)

(defcustom gdb-address-format "0x%012x"
  "Format for addresses to be printed in various panels.

The default is 12 hex characters (zero-padded), which is enough
to display 48-bit addresses. You may need to widen this for some
architectures."
  :group 'gdb
  :type 'string)

(define-derived-mode gdb-tabulated-list-mode tabulated-list-mode "GDB Tabulated")

(defvar-keymap gdb-ui-mode-map
  :doc "Keymap for basic debugger controls."
  "<f1>" #'gud-break
  "C-<f1>" #'gud-remove
  "<f2>" #'gud-next
  "C-<f2>" #'gud-nexti
  "<f3>" #'gud-step
  "C-<f3>" #'gud-stepi
  "S-<f3>" #'gud-finish
  "<f4>" #'gud-cont
  "C-<f4>" #'gdb-pause
  )

(define-minor-mode gdb-ui-mode
  "UI for GDB. This minor mode includes a keymap for basic debugger control.")

(defun gdb-ui-mode-enable (buf)
  (with-current-buffer buf
    (gdb-ui-mode 1)))

(advice-add #'gdb-display-source-buffer :after #'gdb-ui-mode-enable)



;;------------------------------ Disassembly ----------------------------------------


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



;;------------------------------ Registers ----------------------------------------


(defun scp/gdb-changed-registers-handler ()
  (gdb-emit-signal gdb-buf-publisher 'update-registers))

(advice-add #'gdb-changed-registers-handler :after #'scp/gdb-changed-registers-handler)

(defun scp/gdb-invalidate-registers (signal)
  (cond
   ((eq signal 'update-registers)
    ;; remove changed indicator:
    (seq-do (lambda (row)
              (when row
                (let ((value (gdb-reg-value row))
                      (raw (gdb-reg-raw row)))
                  (set-text-properties 0 (length raw) nil raw)
                  (set-text-properties 0 (length value) nil value))))
            gdb-registers-rows)
    (let* ((registers (string-join gdb-changed-registers " "))
           (raw-cmd (concat (gdb-current-context-command "-data-list-register-values") " r " registers))
           (val-cmd (concat (gdb-current-context-command "-data-list-register-values") " N " registers)))
      (gdb-input raw-cmd
                 (gdb-bind-function-to-buffer 'scp/gdb-registers-handler-raw (current-buffer)))
      (gdb-input val-cmd
                 (gdb-bind-function-to-buffer 'scp/gdb-registers-handler (current-buffer))
                 (cons (current-buffer) 'scp/gdb-invalidate-registers))))
   ((eq signal 'start)
    (gdb-input (concat (gdb-current-context-command "-data-list-register-values") " r")
               (gdb-bind-function-to-buffer 'scp/gdb-registers-handler-raw (current-buffer)))
    (gdb-input (concat (gdb-current-context-command "-data-list-register-values") " N")
               (gdb-bind-function-to-buffer 'scp/gdb-registers-handler (current-buffer))
               (cons (current-buffer) 'scp/gdb-invalidate-registers)))))

(def-gdb-auto-update-handler
  scp/gdb-registers-handler-raw
  scp/gdb-registers-handler-custom-raw)

(def-gdb-auto-update-handler
  scp/gdb-registers-handler
  scp/gdb-registers-handler-custom)

(gdb-set-buffer-rules
 'gdb-registers-buffer
 'gdb-registers-buffer-name
 'gdb-registers-mode
 'scp/gdb-invalidate-registers) ;; last one is "trigger" which is called from registers buffer by event subscriber mech

(define-derived-mode gdb-registers-mode gdb-tabulated-list-mode "Registers"
  "Major mode for gdb registers."
  (setq tabulated-list-format
        (vector '("Name" 16 nil :right-align nil)
                '("Fmt" 3 nil :right-align t)
                '("Raw" 18 nil :right-align t)
                '("Value" 999 nil)))
  (tabulated-list-init-header)
  (setq-local gdb-registers-rows nil)
  'scp/gdb-invalidate-registers)

(cl-defstruct (gdb-reg (:type vector))
  "Structure for register data and metadata"
  (name nil :read-only t)
  (fmt nil)
  (raw nil)
  (value nil)
  (other-shit nil))

(defun scp/gdb-registers-handler-custom-raw ()
  (scp/gdb-registers-handler-custom t))

(defun scp/gdb-registers-handler-custom (&optional raw-flag)
  (when gdb-register-names
    (unless gdb-registers-rows
      (setq-local gdb-registers-rows (make-vector (length gdb-register-names) nil))
      (seq-do-indexed (lambda (register-name idx)
                        (aset gdb-registers-rows idx
                              (make-gdb-reg
                               :name (propertize register-name 'font-lock-face font-lock-variable-name-face)
                               :fmt ""
                               :raw ""
                               :value "")))
                      gdb-register-names))
    (let ((register-values (gdb-mi--field (gdb-mi--partial-output) 'register-values)))
      (dolist (register register-values)
        (let* ((register-number (gdb-mi--field register 'number))
               (register-idx (string-to-number register-number))
               (val (gdb-mi--field register 'value))
               (row (aref gdb-registers-rows register-idx)))
          (setf (if raw-flag (gdb-reg-raw row) (gdb-reg-value row))
                (if (member register-number gdb-changed-registers)
                    (propertize val 'font-lock-face font-lock-warning-face)
                  val)))))
    (setq tabulated-list-entries (seq-map-indexed  (lambda (row idx)
                                                     (list idx row))
                                                   gdb-registers-rows))

    ;; (when gdb-registers-enable-filter
    ;;   (cl-loop for pattern
    ;;            in gdb-registers-filter-pattern-list
    ;;            if (string-match pattern register-name)
    ;;            return t
    ;;            finally return nil)))
    )
  (tabulated-list-print)
  (setq mode-name
        (gdb-current-context-mode-name "Registers")))

(defun scp/gdb-registers-handler-custom1 ()
  (error "should not be called"))

(advice-add #'gdb-registers-handler-custom :override #'scp/gdb-registers-handler-custom1)



;;------------------------------ Other stuff ----------------------------------------


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
   '((derived-mode . gdb-registers-mode)
     (display-buffer-in-side-window)
     (side . right)
     (slot . 2)))
  )
(gdb-cfg-setup-many-windows)

(defun scp/gdb-ui ()
  (interactive)
  (gdb-display-breakpoints-buffer)
  (gdb-display-disassembly-buffer)
  (gdb-display-registers-buffer)
  (gdb-display-stack-buffer))


(provide 'gdb-cfg)
