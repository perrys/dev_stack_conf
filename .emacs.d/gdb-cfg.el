(require 'gdb-mi)

(defcustom gdbx-address-format "0x%012x"
  "Format for addresses to be printed in various panels.

The default is 12 hex characters (zero-padded), which is enough
to display 48-bit addresses. You may need to widen this for some
architectures."
  :group 'gdb
  :type 'string)

(define-derived-mode gdbx-tabulated-list-mode tabulated-list-mode "GDB Tabulated")

(defun gdbx-function-buffer-p (buffer)
  "Return t if BUFFER is a GDB function buffer."
  (with-current-buffer buffer
    (derived-mode-p 'gdb-parent-mode 'gdbx-tabulated-list-mode 'gdb-inferior-io-mode)))

(advice-add #'gdb-function-buffer-p :override #'gdbx-function-buffer-p)

(defvar-keymap gdbx-ui-mode-map
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

(define-minor-mode gdbx-ui-mode
  "UI for GDB. This minor mode includes a keymap for basic debugger control.")

(defun gdbx-ui-mode-enable (buf)
  (with-current-buffer buf
    (gdbx-ui-mode 1)))

(advice-add #'gdb-display-source-buffer :after #'gdbx-ui-mode-enable)



;;------------------------------ Disassembly ----------------------------------------


(define-derived-mode gdb-disassembly-mode gdbx-tabulated-list-mode "Disassembly"
  "Major mode for GDB disassembly information."
  ;; TODO Rename overlay variable for disassembly mode
  (add-to-list 'overlay-arrow-variable-list 'gdb-disassembly-position)
  (setq fringes-outside-margins t)
  (setq-local font-lock-defaults '(gdb-disassembly-font-lock-keywords))
  (setq-local gdb-disassembly-position (make-marker))
  (setq-local gdb-addr-to-bol-table (make-hash-table :test 'eql))
  (setq-local gdb-marker-overlay (make-overlay 0 0))
  (overlay-put gdb-marker-overlay 'face '(:inverse-video t))
  (let ((addr-width (length (format gdbx-address-format 0))))
    (setq tabulated-list-format
          (vector `("Address" ,(if (> addr-width 1) addr-width 16) nil)
                  '("Func Name" 10 nil)
                  '("Offset" 8 nil :right-align t :pad-right 2)
                  '("Instruction" 999 nil)))
    (tabulated-list-init-header))
  'gdb-invalidate-disassembly)

(defun gdbx-invalidate-disassembly (signal-symbol)
  "If the address of the current frame's $pc is already in our
   buffer we can skip the GDB interaction & re-render steps,
   and just move the marker instead"
  (catch 'shortcut
    (when (eq signal-symbol 'update-disassembly)
      (let* ((frame (gdb-current-buffer-frame))
             (addr (gdbx-parse-address (gdb-mi--field frame 'addr)))
             (bol-point (gethash addr gdb-addr-to-bol-table)))
        (when bol-point
          (gdbx-disassembly-set-marker bol-point)
          (throw 'shortcut t))))
    nil))

(advice-add #'gdb-invalidate-disassembly :before-until #'gdbx-invalidate-disassembly)

(defun gdbx-parse-address (strval)
  "Parse the given string into a number, accounting for hex numbers with leading `0x'."
  (when strval
    (let  ((s-val (if (string= (downcase (substring strval 0 2)) "0x")
                      (substring strval 2)
                    strval)))
      (string-to-number s-val 16))))

(defun gdbx-safe-propertize (s &rest props)
  (if s (apply #'propertize s props) ""))

(defun gdbx-render-disassembly-line (instr)
  (let ((addr (gdbx-parse-address (gdb-mi--field instr 'address)))
        (func-name (gdbx-safe-propertize (gdb-mi--field instr 'func-name) 'face 'font-lock-function-name-face))
        (offset (format "+%s" (gdb-mi--field instr 'offset)))
        (instr (gdb-mi--field instr 'inst)))
    (list addr (vector (format gdbx-address-format addr) func-name offset instr))))

(defun gdbx-disassembly-set-marker (marker-point)
  (save-excursion
    (goto-char marker-point)
    (let ((window (get-buffer-window (current-buffer) 0)))
      (set-window-point window marker-point)
      (set-marker gdb-disassembly-position marker-point)
      (move-overlay gdb-marker-overlay marker-point (line-end-position)))))

(defun gdbx-disassembly-handler-custom ()
  (clrhash gdb-addr-to-bol-table)
  (let ((instructions (gdb-mi--field (gdb-mi--partial-output) 'asm_insns))
        (address (gdbx-parse-address (gdb-mi--field (gdb-current-buffer-frame) 'addr)))
        (marker-point nil))
    (let ((rows (mapcar #'gdbx-render-disassembly-line instructions))
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
      (gdbx-disassembly-set-marker marker-point))
    (setq mode-name
          (gdb-current-context-mode-name
           (concat "Disassembly: "
                   (gdb-mi--field (gdb-current-buffer-frame) 'func))))))

(advice-add #'gdb-disassembly-handler-custom :override #'gdbx-disassembly-handler-custom)

(defun gdbx-disassembly-place-breakpoints ()
  (gdb-remove-breakpoint-icons (point-min) (point-max))
  (dolist (breakpoint gdb-breakpoints-list)
    (let* ((breakpoint (cdr breakpoint))
           (bptno (gdb-mi--field breakpoint 'number))
           (flag (gdb-mi--field breakpoint 'enabled))
           (address (gdbx-parse-address (gdb-mi--field breakpoint 'addr)))
           (bol (gethash address gdb-addr-to-bol-table)))
      (when bol
        (save-excursion
          (goto-char bol)
          (gdb-put-breakpoint-icon (string-equal flag "y") bptno))))))

(advice-add #'gdb-disassembly-place-breakpoints :override #'gdbx-disassembly-place-breakpoints)

(defun gdbx-disassembly-breakpoints-list-handler-custom ()
  (let* ((buf-name (gdb-disassembly-buffer-name))
         (buf (get-buffer buf-name)))
    (when buf
      (with-current-buffer buf
        (gdb-disassembly-place-breakpoints)))))

(advice-add #'gdb-breakpoints-list-handler-custom :after #'gdbx-disassembly-breakpoints-list-handler-custom)


;;------------------------------ Registers ----------------------------------------


(defun gdbx-changed-registers-handler ()
  (gdb-emit-signal gdb-buf-publisher 'update-registers))

(advice-add #'gdb-changed-registers-handler :after #'gdbx-changed-registers-handler)

(def-gdb-auto-update-handler
  gdbx-registers-handler-raw
  gdbx-registers-handler-custom)

(defvar gdbx-registers-handler-plist nil
  "Map of register formats (x, d, N etc) to specialized handler function for that format")

(defmacro def-gdbx-reg-handler (fmt-key)
  "Define a register handler for the given format"
  (declare (indent defun))
  (let* ((fmt-name (substring (symbol-name fmt-key) 1))
         (handler-name (format "gdbx-registers-handler-%s" fmt-name))
         (custom-handler-name (format "gdbx-registers-handler-custom-%s" fmt-name)))
    `(progn
       (defun ,(intern custom-handler-name) ()
         (gdbx-registers-handler-custom ,fmt-name))
       (def-gdb-auto-update-handler
         ,(intern handler-name)
         ,(intern custom-handler-name))
       (setq gdbx-registers-handler-plist
             (plist-put gdbx-registers-handler-plist ,fmt-key #',(intern handler-name))))))

(def-gdbx-reg-handler :N)
(def-gdbx-reg-handler :d)
(def-gdbx-reg-handler :x)
(def-gdbx-reg-handler :t)
(def-gdbx-reg-handler :o)

(gdb-set-buffer-rules
 'gdb-registers-buffer
 'gdb-registers-buffer-name             ;
 'gdb-registers-mode
 'gdbx-invalidate-registers) ;; last one is "trigger" which is called from registers buffer by event subscriber mech

(defmacro def-gdbx-change-format (fmt)
  `(defun ,(intern (format "gdbx-change-format-%s" fmt)) ()
     (interactive)
     (gdbx-change-format ,fmt)))
(def-gdbx-change-format "x")
(def-gdbx-change-format "d")
(def-gdbx-change-format "N")
(def-gdbx-change-format "t")
(def-gdbx-change-format "o")

(setq gdb-registers-mode-map
      (let ((map (make-sparse-keymap)))
        (suppress-keymap map)
        (define-key map "\r" 'gdb-edit-register-value)
        (define-key map [mouse-2] 'gdb-edit-register-value)
        (define-key map "q" 'kill-current-buffer)
        (define-key map "f" #'gdb-registers-toggle-filter)
        (define-key map "x" #'gdbx-change-format-x)
        (define-key map "d" #'gdbx-change-format-d)
        (define-key map "n" #'gdbx-change-format-N)
        (define-key map "t" #'gdbx-change-format-t)
        (define-key map "o" #'gdbx-change-format-o)
        map))

(define-derived-mode gdb-registers-mode gdbx-tabulated-list-mode "Registers"
  "Major mode for gdb registers."
  (setq tabulated-list-format
        (vector '("Name" 16 nil :right-align nil)
                '("Fmt" 3 nil :right-align t)
                '("Raw" 18 nil :right-align t)
                '("Value" 999 nil)))
  (tabulated-list-init-header)
  (setq-local gdbx-registers-vector nil)
  'gdbx-invalidate-registers)

(cl-defstruct (gdbx-reg (:type vector))
  "Structure for register data and metadata"
  (name nil :read-only t)
  (fmt nil)
  (raw nil)
  (value nil)
  (display t)
  (idx nil :read-only t)
  (other-shit nil))

(defun gdbx-edit-register-value (&optional event)
  "Assign a value to a register displayed in the registers buffer."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (let* ((idx (tabulated-list-get-id))
         (reg (aref gdbx-registers-vector idx))
         (var (gdbx-reg-name reg))
	 (value (read-string (format "New value (%s): " var) (gdbx-reg-raw reg))))
    (gud-basic-call
     (concat  "-gdb-set variable $" var " = " value))))

(advice-add #'gdb-edit-register-value :override #'gdbx-edit-register-value)

(defun gdbx-initialize-registers-vector ()
  "Ensure that the buffer-local variable gdbx-registers-vector is
initialized"
  (when gdb-register-names
    (unless gdbx-registers-vector
      (setq-local gdbx-registers-vector (make-vector (length gdb-register-names) nil))
      (seq-do-indexed (lambda (register-name idx)
                        (aset gdbx-registers-vector idx
                              (make-gdbx-reg
                               :name (propertize register-name 'font-lock-face font-lock-variable-name-face)
                               :idx idx
                               :fmt "N"
                               :raw ""
                               :value ""
                               :display (cl-loop for pattern
                                                 in gdb-registers-filter-pattern-list
                                                 if (string-match pattern register-name)
                                                 return t
                                                 finally return nil))))
                      gdb-register-names))))

(defun gdbx-change-format (fmt &optional idx)
  (unless idx
    (setq idx (tabulated-list-get-id)))
  (setf (gdbx-reg-fmt (aref gdbx-registers-vector idx)) fmt)
  (gdbx-update-registers (list idx)))

(defun gdbx-collect-registers (reg-vector &optional indices)
  "Return a plist mapping GDB format to the list of register indices
in gdbx-registers-vector with that format, optionally filtered to
the ones in INDICES."
  (let ((result nil))
    (seq-do-indexed (lambda (row idx)
                      (when (or (not indices)
                                (memq idx indices))
                        (let* ((fmt (gdbx-reg-fmt row))
                               (fmt-key (intern (format ":%s" fmt)))
                               (group (plist-get result fmt-key)))
                          (push idx group)
                          (setq result (plist-put result fmt-key group)))))
                    reg-vector)
    result))

(defun gdbx-update-registers (&optional register-indices)
  "Issue requests for the values of registers in REGISTER-INDICES,
or all registers if not provided. This function requests raw
values and formatted values grouped by format."
  (let* ((registers-string (if register-indices
                               (string-join (mapcar #'number-to-string register-indices) " ")
                             ""))
         (groups (gdbx-collect-registers gdbx-registers-vector register-indices)))
    (gdb-input (concat (gdb-current-context-command "-data-list-register-values") " r " registers-string)
               (gdb-bind-function-to-buffer 'gdbx-registers-handler-raw (current-buffer)))
    (while groups
      (let* ((fmt-key (car groups))
             (group (cadr groups))
             (registers-string (string-join (mapcar #'number-to-string group) " "))
             (fmt (substring (symbol-name fmt-key) 1))
             (val-cmd (concat (gdb-current-context-command "-data-list-register-values") " " fmt " " registers-string))
             (handler (plist-get gdbx-registers-handler-plist fmt-key)))
        (gdb-input val-cmd
                   (gdb-bind-function-to-buffer handler (current-buffer))
                   (cons (current-buffer) 'gdbx-invalidate-registers))
        (setq groups (cddr groups))))))

(defun gdbx-invalidate-registers (signal)
  (gdbx-initialize-registers-vector)
  (cond
   ((eq signal 'update-registers)
    ;; remove changed indicator:
    (seq-do (lambda (row)
              (when row
                (let ((value (gdbx-reg-value row))
                      (raw (gdbx-reg-raw row)))
                  (set-text-properties 0 (length raw) nil raw)
                  (set-text-properties 0 (length value) nil value))))
            gdbx-registers-vector)
    (let* ((register-indices (mapcar #'string-to-number gdb-changed-registers)))
      (gdbx-update-registers register-indices)))
   ((eq signal 'start)
    (gdbx-update-registers))))


(defun gdbx-registers-handler-custom (&optional fmt)
  (when gdb-register-names
    (let ((register-values (gdb-mi--field (gdb-mi--partial-output) 'register-values)))
      (dolist (register register-values)
        (let* ((register-number (gdb-mi--field register 'number))
               (register-idx (string-to-number register-number))
               (val (gdb-mi--field register 'value))
               (row (aref gdbx-registers-vector register-idx)))
          (setf (if fmt (gdbx-reg-value row) (gdbx-reg-raw row))
                (if (member register-number gdb-changed-registers)
                    (propertize val 'font-lock-face font-lock-warning-face)
                  val))
          (when fmt
            (setf (gdbx-reg-fmt row) fmt)))))
    (let ((registers-vector
           (if gdb-registers-enable-filter
               (seq-filter (lambda (reg)
                             (gdbx-reg-display reg))
                           gdbx-registers-vector)
             gdbx-registers-vector)))
      (setq tabulated-list-entries (seq-map (lambda (row)
                                              (list (gdbx-reg-idx row) row))
                                            registers-vector))))
  (tabulated-list-print)
  (setq mode-name
        (gdb-current-context-mode-name
         (format "Reg-%s" (if gdb-registers-enable-filter "filt" "all")))))

(advice-add #'gdb-registers-handler-custom :override #'gdbx-registers-handler-custom)

(ert-deftest gdbx-can-group-registers ()
  (let* ((registers (vconcat `(,(make-gdb-reg :name "nat1" :fmt "N")
                               ,(make-gdb-reg :name "nat2" :fmt "N")
                               ,(make-gdb-reg :name "int1" :fmt "d")
                               ,(make-gdb-reg :name "int2" :fmt "d")
                               ,(make-gdb-reg :name "nat3" :fmt "N"))))
         (groups (gdbx-collect-registers registers))
         (to-reg (lambda (n) (aref registers n)))
         (nats (mapcar to-reg (plist-get groups :N)))
         (ints (mapcar to-reg (plist-get groups :d))))
    (should (equal '("nat3" "nat2" "nat1") (mapcar #'gdbx-reg-name nats)))
    (should (equal '("int2" "int1") (mapcar #'gdbx-reg-name ints)))))



;;------------------------------ Other stuff ----------------------------------------

(defun gdbx-mi-eval (cmd)
  "Evaluate the GDB command CMD and display results in the *scratch* buffer"
  (interactive "smi cmd: ")
  (gdb-input cmd
             (lambda ()
               (let ((response  (gdb-mi--partial-output)))
                 (with-current-buffer "*scratch*"
                   (save-excursion
                     (goto-char (point-max))
                     (insert (pp-to-string response))))))))

(defun gdbx-cfg-setup-many-windows ()
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
(gdbx-cfg-setup-many-windows)

(defun gdbx-ui ()
  (interactive)
  (gdb-display-breakpoints-buffer)
  (gdb-display-disassembly-buffer)
  (gdb-display-registers-buffer)
  (gdb-display-stack-buffer))


(provide 'gdb-cfg)
