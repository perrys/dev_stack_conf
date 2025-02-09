;;  -*- lexical-binding: t; -*-

(require 'gdb-mi)
(eval-when-compile (require 'cl-lib))

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


;;------------------------------ MI Variable Objects ----------------------------------------

(cl-defstruct (gdbx-varobj (:type vector))
  "Structure for gdb variable objects - associates variables with
their children in the tree structure."
  (data nil) ; contains the gdb-mi data
  (child-names nil))

(defvar gdbx-varobj-table (make-hash-table :test 'equal)
  "Hash table for mi variable objects, keyed by name (ID) of each variable object")

(defun gdbx-varobj-expandablep (varobj)
  "Return non-nil if VAROBJ has children"
  (let ((numchild (gdb-mi--field (gdbx-varobj-data varobj) 'numchild)))
    (and numchild (> (string-to-number numchild) 0))))

(defun gdbx-varobj-create-floating (expr &optional callback)
  "Create a floating variable object for EXPR, and store it in the varobj table."
  (gdb-input (concat "-var-create - @ " expr)
             (lambda ()
               (let* ((var-fields (gdb-mi--partial-output))
                      (var-name (gdb-mi--field var-fields 'name))
                      (varobj (make-gdbx-varobj :data var-fields)))
                 (puthash var-name varobj gdbx-varobj-table)
                 (when callback
                   (funcall callback varobj))))))

(defun gdbx-varobj-expand (var-name varobj &optional callback)
  "Expand the named variable object to one level of its children."
  (gdb-input (concat "-var-list-children --all-values " var-name)
             (lambda ()
               (let ((children (gdb-mi--field (gdb-mi--partial-output) 'children))
                     (child-names nil))
                 (seq-doseq (child children)
                   (let ((child-name (gdb-mi--field child 'name)))
                     ;; cdr to get rid of the 'child symbol which is first in the list
                     (puthash child-name (make-gdbx-varobj :data (cdr child)) gdbx-varobj-table)
                     (push child-name child-names)))
                 (setf (gdbx-varobj-child-names varobj) (nreverse child-names))
                 (when callback
                   (funcall callback))))))

(defun gdbx-varobj-remove-children-recursive (var-name)
  (let* ((varobj (gethash var-name gdbx-varobj-table))
         (child-names (gdbx-varobj-child-names varobj)))
    (seq-doseq (child-name child-names)
      (gdbx-varobj-remove-children-recursive child-name)))
  (remhash var-name gdbx-varobj-table))

(defun gdbx-varobj-unexpand (var-name varobj &optional callback)
  (let ((child-names (gdbx-varobj-child-names varobj)))
    (seq-doseq (child-name child-names)
      (gdbx-varobj-remove-children-recursive child-name))
    (setf (gdbx-varobj-child-names varobj) nil)
    (gdb-input (concat "-var-delete -c " var-name)
               (lambda()
                 (when callback
                   (funcall callback))))))

(defun gdbx-varobj-merge (var-name updates)
  (let* ((varobj (gethash var-name gdbx-varobj-table))
         (data (gdbx-varobj-data varobj))
         (in-scope (gdb-mi--field updates 'in_scope)))
    (cond
     ((equal "true" in-scope)
      (setcdr (assq 'value data) (gdb-mi--field updates 'value))
      (when (equal "true" (gdb-mi--field updates 'type_changed))
        (setcdr (assq 'type data) (or (gdb-mi--field updates 'type) "<unknown>"))))
     ((equal "false" in-scope)
      (setcdr (assq 'value data) "<invalid>")
      (setcdr (assq 'type data)  "<unknown>"))
     ((equal "invalid" in-scope)
      nil) ;; TODO: delete varobj
     (t (error "invalid variable object update - in-scope=\"%s\"" in-scope)))))

(defun gdbx-varobj-update (var-name &optional callback)
  (gdb-input (concat "-var-update --all-values " var-name)
             (lambda ()
               (let ((changelist (gdb-mi--field (gdb-mi--partial-output) 'changelist)))
                 (seq-doseq (child changelist)
                   (let ((var-name (gdb-mi--field child 'name)))
                     (gdbx-varobj-merge var-name child)))
                 (when callback
                   (funcall callback))))))


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
;; deliberately no handler for "+" format - variable objects handled separately

(gdb-set-buffer-rules
 'gdb-registers-buffer
 'gdb-registers-buffer-name
 'gdb-registers-mode
 'gdbx-reg-invalidate) ;; last one is "trigger" which is called from registers buffer by event subscriber mech

(defmacro def-gdbx-reg-change-format (fmt)
  `(defun ,(intern (format "gdbx-reg-change-format-%s" fmt)) ()
     (interactive)
     (gdbx-reg-change-format ,fmt)))
(def-gdbx-reg-change-format "x")
(def-gdbx-reg-change-format "d")
(def-gdbx-reg-change-format "N")
(def-gdbx-reg-change-format "t")
(def-gdbx-reg-change-format "o")
(def-gdbx-reg-change-format "+")

(setq gdb-registers-mode-map
      (let ((map (make-sparse-keymap)))
        (suppress-keymap map)
        (define-key map "\r" 'gdb-edit-register-value)
        (define-key map [mouse-2] 'gdb-edit-register-value)
        (define-key map [?\t] #'gdbx-reg-varobj-toggle-expanded)
        (define-key map [mouse-1] #'gdbx-reg-varobj-toggle-expanded)
        (define-key map "q" 'kill-current-buffer)
        (define-key map "f" #'gdb-registers-toggle-filter)
        (define-key map "x" #'gdbx-reg-change-format-x)
        (define-key map "d" #'gdbx-reg-change-format-d)
        (define-key map "n" #'gdbx-reg-change-format-N)
        (define-key map "t" #'gdbx-reg-change-format-t)
        (define-key map "o" #'gdbx-reg-change-format-o)
        (define-key map "+" #'gdbx-reg-change-format-+)
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
  'gdbx-reg-invalidate)


(cl-defstruct (gdbx-reg (:type vector))
  "Structure for register data and metadata"
  (name nil :read-only t)
  (fmt nil)
  (raw nil)
  (value nil)
  (display t)
  (idx nil :read-only t)
  (varobj-name nil)
  (child-names nil))

(defun gdbx-reg-create-varobj (reg-idx &optional callback)
  "Create a floating (i.e. for all frames) variable object for the
given register number. The name of the variable object will be
stored in the register's var-name field."
  (let* ((reg-obj (aref gdbx-registers-vector reg-idx))
         (reg-name (substring-no-properties (gdbx-reg-name reg-obj))))
    (gdbx-varobj-create-floating
     (concat "$" reg-name)
     (lambda (varobj)
       (let* ((data (gdbx-varobj-data varobj))
              (var-name (gdb-mi--field data 'name)))
         ;; newly-created varobjs don't have an exp field, so add it
         (setf (gdbx-varobj-data varobj) (cons (cons 'exp reg-name) data))
         (setf (gdbx-reg-varobj-name reg-obj) var-name))
       (when callback
         (funcall callback))))))

(defun gdbx-reg-edit-value (&optional event)
  "Assign a value to a register displayed in the registers buffer."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (let* ((idx (tabulated-list-get-id))
         (reg (aref gdbx-registers-vector idx))
         (var (gdbx-reg-name reg))
	 (value (read-string (format "New value (%s): " var) (gdbx-reg-raw reg))))
    (gud-basic-call
     (concat  "-gdb-set variable $" var " = " value))))

(advice-add #'gdb-edit-register-value :override #'gdbx-reg-edit-value)

(defun gdbx-reg-initialize-registers-vector ()
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

(defun gdbx-reg-change-format (fmt &optional idx)
  (unless idx
    (setq idx (tabulated-list-get-id)))
  (when (fixnump idx) ;; ignore varobj rows
    (setf (gdbx-reg-fmt (aref gdbx-registers-vector idx)) fmt)
    (gdbx-reg-update (list idx))))

(defun gdbx-reg-expandable-varobj-name-for-id (id)
  "Get the variable name for the register with ID in the registers tabulated list"
  (if (fixnump id) ;; this is the root register index
      (let* ((reg (aref gdbx-registers-vector id))
             (reg-name (gdbx-reg-name reg))
             (var-name (gdbx-reg-varobj-name reg)))
        (when (equal "+" (gdbx-reg-fmt reg))
          var-name))
    id)) ;; should already be the var name

(defun gdbx-reg-varobj-toggle-expanded (&optional id)
  "For the register under point, expand or collapse as required"
  (interactive)
  (unless id
    (setq id (tabulated-list-get-id)))
  (let ((var-name (gdbx-reg-expandable-varobj-name-for-id id)))
    (when var-name
      (let ((varobj (gethash var-name gdbx-varobj-table)))
        (when (gdbx-varobj-expandablep varobj)
          (if (gdbx-varobj-child-names varobj)
              (gdbx-varobj-unexpand
               var-name varobj
               (gdb-bind-function-to-buffer #'gdbx-reg-print-maybe-filtered (current-buffer)))
            (gdbx-varobj-expand
             var-name varobj
             (gdb-bind-function-to-buffer #'gdbx-reg-print-maybe-filtered (current-buffer)))))))))

(defun gdbx-reg-collect-by-fmt (reg-vector &optional indices)
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

(defun gdbx-reg-update (&optional register-indices)
  "Issue requests for the values of registers in REGISTER-INDICES,
or all registers if not provided. This function requests raw
values and formatted values grouped by format."
  (let* ((registers-string (if register-indices
                               (string-join (mapcar #'number-to-string register-indices) " ")
                             ""))
         (groups (gdbx-reg-collect-by-fmt gdbx-registers-vector register-indices)))
    (gdb-input (concat (gdb-current-context-command "-data-list-register-values") " r " registers-string)
               (gdb-bind-function-to-buffer 'gdbx-registers-handler-raw (current-buffer)))
    (while groups
      (let* ((fmt-key (car groups))
             (group (cadr groups))
             (registers-string (string-join (mapcar #'number-to-string group) " "))
             (fmt (substring (symbol-name fmt-key) 1))
             (val-cmd (concat (gdb-current-context-command "-data-list-register-values") " " fmt " " registers-string))
             (handler (plist-get gdbx-registers-handler-plist fmt-key)))
        (if handler ;; normal register update
            (gdb-input val-cmd
                       (gdb-bind-function-to-buffer handler (current-buffer))
                       (cons (current-buffer) 'gdbx-reg-invalidate))
          (seq-doseq (reg-idx group) ;; no handler so this must be a variable object
            (let* ((reg-obj (aref gdbx-registers-vector reg-idx))
                   (var-name (gdbx-reg-varobj-name reg-obj)))
              (if var-name
                  (gdbx-varobj-update var-name
                                      (gdb-bind-function-to-buffer #'gdbx-reg-print-maybe-filtered (current-buffer)))
                (gdbx-reg-create-varobj reg-idx
                                        (gdb-bind-function-to-buffer #'gdbx-reg-print-maybe-filtered (current-buffer))))))))
      (setq groups (cddr groups)))))

(defun gdbx-reg-invalidate (signal)
  (gdbx-reg-initialize-registers-vector)
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
      (gdbx-reg-update register-indices)))
   ((eq signal 'start)
    (gdbx-reg-update))))

(defun gdbx-reg-varobj-print (varobj depth reg-idx value-props)
  (let* ((data (gdbx-varobj-data varobj))
         (child-names (gdbx-varobj-child-names varobj))
         (varobj-name (gdb-mi--field data 'name))
         (switch (if (gdbx-varobj-expandablep varobj) (if child-names "- " "+ ") "  "))
         (pad-fmt (format "%%%ds%%s%%s" (* 2 depth)))
         (display-name (format pad-fmt "" switch
                               (propertize (gdb-mi--field data 'exp) 'font-lock-face font-lock-variable-name-face)))
         (type (apply #'propertize (gdb-mi--field data 'type) value-props))
         (value (apply #'propertize (gdb-mi--field data 'value) value-props)))
    (list (if reg-idx reg-idx varobj-name) (vector display-name "+" type value))))

(defun gdbx-reg-varobj-print-recursive (var-name entries depth reg-idx value-props)
  (let ((varobj (gethash var-name gdbx-varobj-table)))
    (when varobj
      (push (gdbx-reg-varobj-print varobj depth reg-idx value-props) entries)
      (dolist (child-name (gdbx-varobj-child-names varobj))
        (setq entries (gdbx-reg-varobj-print-recursive child-name entries (1+ depth) nil value-props)))
      ))
  entries)

(defun gdbx-reg-print (registers-vector)
  "Update 'tablulated-list-entries' with data from the given
registers vector, which is a possibly filtered list from
'gdbx-registers-vector'."
  (let* ((entries nil)
         (printer
          (lambda (row)
            (if (and (equal "+" (gdbx-reg-fmt row))
                     (gdbx-reg-varobj-name row))
                ;; Get properties from the register's raw value - a bit of a hack..
                (let ((value-props (text-properties-at 0 (gdbx-reg-raw row))))
                  (setq entries (gdbx-reg-varobj-print-recursive
                                 (gdbx-reg-varobj-name row) entries 0 (gdbx-reg-idx row)
                                 value-props)))
              (let ((line (vector
                           (concat "  " (gdbx-reg-name row))
                           (gdbx-reg-fmt row)
                           (gdbx-reg-raw row)
                           (gdbx-reg-value row))))
                (push (list (gdbx-reg-idx row) line) entries))))))
    (seq-do printer registers-vector)
    (setq tabulated-list-entries (nreverse entries))))

(defun gdbx-reg-print-maybe-filtered ()
  (let ((registers-vector
         (if gdb-registers-enable-filter
             (seq-filter (lambda (reg)
                           (gdbx-reg-display reg))
                         gdbx-registers-vector)
           gdbx-registers-vector)))
    (gdbx-reg-print registers-vector))
  (let ((pos (point)))
    (tabulated-list-print)
    (goto-char pos))
  (setq mode-name
        (gdb-current-context-mode-name
         (format "Reg-%s" (if gdb-registers-enable-filter "filt" "all")))))

(defun gdbx-registers-handler-custom (&optional fmt)
  "Handler for '-data-list-register-values'. The returned
'register-values' field is a list of entries with a 'value' and a
'number' field"
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
    (gdbx-reg-print-maybe-filtered)))

(advice-add #'gdb-registers-handler-custom :override #'gdbx-registers-handler-custom)

(ert-deftest gdbx-can-group-registers ()
  (let* ((registers (vconcat `(,(make-gdb-reg :name "nat1" :fmt "N")
                               ,(make-gdb-reg :name "nat2" :fmt "N")
                               ,(make-gdb-reg :name "int1" :fmt "d")
                               ,(make-gdb-reg :name "int2" :fmt "d")
                               ,(make-gdb-reg :name "nat3" :fmt "N"))))
         (groups (gdbx-reg-collect-by-fmt registers))
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

(defmacro gdbx-set-display-buffer (derived-mode side-sym slot-num)
  `(add-to-list 'display-buffer-alist
                '((lambda (buffer-name config)
                    (with-current-buffer buffer-name
                      (derived-mode-p ,derived-mode)))
                  (display-buffer-in-side-window)
                  (side . ,side-sym)
                  (slot . ,slot-num))))


(defun gdbx-cfg-setup-many-windows ()
  (interactive)
  (gdbx-set-display-buffer 'gud-mode left -1)
  (gdbx-set-display-buffer 'gdb-frames-mode left 1)
  (gdbx-set-display-buffer 'gdb-locals-mode right 0)
  (gdbx-set-display-buffer 'gdb-registers-mode right 1)
  (gdbx-set-display-buffer 'gdb-memory-mode bottom -1)
  (gdbx-set-display-buffer 'gdb-disassembly-mode bottom 0)
  (gdbx-set-display-buffer 'gdb-breakpoints-mode bottom 1)
  (gdbx-set-display-buffer 'gdb-inferior-io-mode bottom 2)
  )
(gdbx-cfg-setup-many-windows)

(defun gdbx-ui ()
  (interactive)
  (gdb-display-gdb-buffer)
  (gdb-display-breakpoints-buffer)
  (gdb-display-disassembly-buffer)
  (gdb-display-registers-buffer)
  (gdb-display-stack-buffer))


(provide 'gdb-cfg)
