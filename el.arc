(%scheme

  (xdef arc-get arc-get)
  (xdef arc-set arc-set)

  (xdef arc-get-global arc-get-global)
  (xdef arc-set-global arc-set-global)

  )

(mac setq (var val) `(assign ,var ,val))

(mac not (x) `(no ,x))

(def null (x) (no x))

(def functionp (x) (isa x 'fn))
(def stringp (x) (isa x 'string))
(def symbolp (x) (isa x 'sym))
(def listp (x) (alist x))
(def consp (x) (acons x))
(def memq (x l) (mem x l))

(def member (x l (o test iso))
  (mem [test x _] l))

(def append ls
  (apply join ls))

(def plist-get (l k)
  (when l
    (if (is (car l) k) (cadr l) (plist-get (cddr l) k))))

(def plist-put (l k v)
  (if (no l) (list k v)
      (is (car l) k) (cons k (cons v (cddr l)))
    (cons (car l) (cons (cadr l) (plist-put (cddr l) k v)))))

(define-global symbol-plists* (obj)) ; should this be a weak hash?

(def symbol-plist (symbol)
  (symbol-plists* symbol))

(def setplist (symbol plist)
  (= (symbol-plists* symbol) plist))

(def get (symbol property)
  (plist-get (symbol-plist symbol) property))

(def put (symbol property value)
  (setplist symbol (plist-put (symbol-plist symbol) property value))
  value)

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
    (unless (local-variable-p name)
      (let b (current-buffer)
        (push (list name (errsafe:eval name)) (b 'locals))))
    name))

(mac setq-local (var val)
  `(do (make-local-variable ',var)
       (setq ,var ,val)))

(def make-variable-buffer-local (var)
  (unless (symbolp var)
    (err "expected symbol" var))
  (pushnew var make-variables-buffer-local*)
  var)

(mac defvar-local (name value (o docstring))
  `(do (defvar ,name ,value ,docstring)
       (make-variable-buffer-local ',name)))

(def local-variable-p (name (o buffer))
  (if (assoc name (buffer-local-variables buffer)) t nil))

(def local-variable-if-set-p (name (o buffer))
  (or (local-variable-p name)
      (and (mem name make-variables-buffer-local*) t)))

(def buffer-local-value (name (o buffer))
  (alref (buffer-local-variables buffer) name))

(def buffer-local-variables ((o buffer))
  (let b (or buffer (current-buffer))
    (b 'locals)))

(def kill-local-variable (name)
  (let b (current-buffer)
    (pull [caris _ name] (b 'locals)))
  name)

(def kill-local-variable? (entry)
  (and (isnt (car entry) t)
       (~get (car entry) 'permanent-local)
       (~get (car entry) 'permanent-local-hook)))

(def kill-all-local-variables ()
  (let b (current-buffer)
    (pull kill-local-variable? (b 'locals))
    nil))

(def add-hook (hook function (o append?) (o local))
  (or (boundp hook) (arc-set hook nil))
  (or (default-boundp hook) (set-default hook nil))
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
            (if append?
                (append hook-value (list function))
                (cons function hook-value))))
    ;; Set the actual variable
    (if local
        (do
          ; If HOOK isn't a permanent local,
          ; but FUNCTION wants to survive a change of modes,
          ; mark HOOK as partially permanent.
          (and (symbolp function)
               (get function 'permanent-local-hook)
               (not (get hook 'permanent-local))
               (put hook 'permanent-local 'permanent-local-hook))
          (arc-set hook hook-value))
        (set-default hook hook-value)))
  )

(def funcall-nil args
  (apply funcall args)
  nil)

(def run-hook-with-args-1 (funcall sym . args)
  (with (unset (list 'unset) ret nil)
    (let val (arc-get sym unset)
      (if (or (is val unset)
              (no val))
          ret
          (or (~consp val)
              (functionp val))
          (apply funcall val args)
        (let global-vals nil
          (while (and (consp val) (null ret))
            (%point continue
              (if (caris val t)
                  ; t indicates this hook has a local binding;
                  ;  it means to run the global binding too.
                  (do (= global-vals (default-value sym))
                      (if (null global-vals) (continue))
                      (if (or (~consp global-vals)
                              (caris global-vals 'fn))
                          (= ret (apply funcall global-vals args))
                          (while (and (consp global-vals)
                                      (null ret))
                            (let x (car global-vals)
                              ; In a global value, t should not occur.  If it does, we
                              ; must ignore it to avoid an endless loop. 
                              (unless (is x t)
                                (= ret (apply funcall x args))))
                            (= global-vals (cdr global-vals)))))
                  (let x (car val)
                    (= ret (apply funcall x args)))))
            (= val (cdr val))))))
    ret))

(def run-hook-wrapped (hook wrap-function)
  (run-hook-with-args-1 wrap-function hook))

(def run-hook (hook)
  (run-hook-with-args-1 funcall-nil hook))

(def run-hooks hookvars
  (each x hookvars
    (run-hook x)))

(def run-hook-with-args args
  (apply run-hook-with-args-1 funcall-nil args))

(def run-hook-with-args-until-success args
  (apply run-hook-with-args-1 funcall args))

(def funcall-not (f . args)
  (no (apply funcall f args)))

(def run-hook-with-args-until-failure args
  (if (null (apply run-hook-with-args-1 funcall-not args)) t nil))

(def function (f)
  (if (isa f 'fn) f (eval f)))

(def funcall (f . args)
  (apply (function f) args))




