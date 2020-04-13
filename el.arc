(%scheme

  (xdef arc-get arc-get)
  (xdef arc-set arc-set)

  (xdef arc-get-global arc-get-global)
  (xdef arc-set-global arc-set-global)

  )

(mac setq (var val) `(assign ,var ,val))

(mac not (x) `(no ,x))

(def functionp (x) (isa x 'fn))
(def stringp (x) (isa x 'string))
(def listp (x) (alist x))
(def consp (x) (acons x))
(def memq (x l) (mem x l))

(def member (x l (o test iso))
  (mem [test x _] l))

(def append (x l)
  (rev (cons x (rev l))))

(def local-variable-if-set-p (name (o buffer))
  (if (assoc name (buffer-local-variables buffer)) t nil))

(def boundp (name)
  (let unset (list 'unset)
    (isnt (arc-get name unset) unset)))

(def default-value (name)
  (arc-get-global name))

(def default-boundp (name)
  (let unset (list 'unset)
    (isnt (arc-get-global name unset) unset)))

(mac setq-default (var val . more)
  `(do (set-default ',var ,val)
       ,@(when more `((setq-default ,@more)))))

(def set-default (name value)
  (arc-set-global name value))

(def default-toplevel-value (name)
  (arc-get-global name))

(def set-default-toplevel-value (name value)
  (arc-set-global name value))

(def make-local-variable (name)
  (atomic
    (unless (isa name 'sym)
      (err "make-local-variable: expected symbol" name))
    (unless (local-variable-if-set-p name)
      (let b (current-buffer)
        (push (list name (errsafe:eval name)) (b 'locals))))
    name))

(mac setq-local (var val)
  `(do (make-local-variable ',var)
       (setq ,var ,val)))

(def add-hook (hook function (o append) (o local))
  (or (boundp hook) (arc-set hook nil))
  ;(or (default-boundp hook) (set-default hook nil))
  (if local (unless (local-variable-if-set-p hook)
              (arc-set (make-local-variable hook) (list t)))
    ;; Detect the case where make-local-variable was used on a hook
    ;; and do what we used to do.
    (unless (and (consp (symbol-value hook)) (memq t (symbol-value hook)))
      (setq local t)))
  (let hook-value (if local (symbol-value hook) (default-value hook))
    ;; If the hook value is a single function, turn it into a list.
    (when (or (not (listp hook-value)) (functionp hook-value))
      (setq hook-value (list hook-value)))
    ;; Do the actual addition if necessary
    (unless (member function hook-value)
      (when (stringp function)
        (setq function (purecopy function)))
      (setq hook-value
            (if append
                (append hook-value (list function))
                (cons function hook-value))))
    ;; Set the actual variable
    (if local
        (do
          ;;; If HOOK isn't a permanent local,
          ;;; but FUNCTION wants to survive a change of modes,
          ;;; mark HOOK as partially permanent.
          ;(and (symbolp function)
          ;     (get function 'permanent-local-hook)
          ;     (not (get hook 'permanent-local))
          ;     (put hook 'permanent-local 'permanent-local-hook))
          (arc-set hook hook-value))
        (set-default hook hook-value))))


