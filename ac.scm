; Arc Compiler.
#lang racket/load

(require json)
(require syntax/stx)
(require racket/port)
(require racket/pretty)
(require racket/runtime-path)
(require racket/system)
(require racket/tcp)
(require racket/unsafe/ops)
(require racket/path)
(require racket/trace)
(require racket/async-channel)
(require racket/struct)
(require syntax/srcloc)

; configure reader
; (read-square-bracket-with-tag #t)
; (read-curly-brace-with-tag #t)
(print-hash-table #t)
(print-syntax-width 10000)

; sread = scheme read. eventually replace by writing read

(struct ar-tagged (type rep) #:prefab)

(define (sread p (eof eof))
  (parameterize ((read-accept-lang #t)
                 (read-accept-reader #t))
    (port-count-lines! p)
    (let ((expr (read-syntax (object-name p) p)))
      (if (eof-object? expr) eof expr))))

(define (sdata p (eof eof))
  (parameterize ((read-accept-lang #f)
                 (read-accept-reader #f))
    (let ((expr (ac-quoted (read p))))
      (if (eof-object? expr) eof expr))))

(define (syn x (src #f))
  (if (syntax? x)
      (syn (syntax->datum x) (or src x))
      (datum->syntax #f x (if (syntax? src) src #f))))

(define (datum x)
  (let ((s (syn x)))
    (syntax->datum s)))

(define env* (make-parameter (list)))

; compile an Arc expression into a Scheme expression,
; both represented as s-expressions.
; env is a list of lexically bound variables, which we
; need in order to decide whether set should create a global.

(define (stx-map proc stxl)
  (map proc (stx->list stxl)))

(define (ac* e s env)
  (cond ((string? s) (ac-string s env))
        ((keyword? s) s)
        ((literal? s) (list 'quote (ac-quoted s)))
        ((ssyntax? s) (ac (expand-ssyntax s) env))
        ((symbol? s) (ac-var-ref s env))
        ((eq? (xcar s) 'lexenv) (ac-lenv (cdr s) env))
        ((eq? (xcar s) 'syntax) (cadr (syntax-e e)))
        ((eq? (xcar (xcar s)) 'syntax) (stx-map ac e))
        ((ssyntax? (xcar s)) (ac (cons (expand-ssyntax (car s)) (cdr s)) env))
        ((eq? (xcar s) 'quote) (list 'quote (ac-quoted (cadr s))))
        ((eq? (xcar s) 'quasiquote) (ac-qq (cadr s) env))
        ((eq? (xcar s) 'if) (ac-if (cdr s) env))
        ((eq? (xcar s) 'fn) (ac-fn (cadr s) (cddr s) env))
        ((eq? (xcar s) 'assign) (ac-set (cdr s) env))
        ; the next three clauses could be removed without changing semantics
        ; ... except that they work for macros (so prob should do this for
        ; every elt of s, not just the car)
        ((eq? (xcar (xcar s)) 'compose) (ac (decompose (cdar s) (cdr s)) env))
        ((eq? (xcar (xcar s)) 'complement)
         (ac (list 'no (cons (cadar s) (cdr s))) env))
        ((eq? (xcar (xcar s)) 'andf) (ac-andf s env))
        ((pair? s) (ac-call (car s) (cdr s) env))
        ((syntax? s) s)
        (#t (err "Bad object in expression" s))))

(define (ac stx (env (env*)) (ns (arc-namespace)))
  (parameterize ((env* env))
    (let* ((e (syn stx))
           (s (syntax->datum e))
           (expr (ac* e s env)))
      (parameterize ((current-namespace ns))
        (namespace-syntax-introduce (syn expr stx))))))

(define ar-nil '())
(define ar-t #t)

(define (ar-nil? x)
  (eqv? x ar-nil))

(define atstrings #f)

(define (ac-string s env)
  (if atstrings
      (if (atpos s 0)
          (ac (cons 'string (map (lambda (x)
                                   (if (string? x)
                                       (unescape-ats x)
                                       x))
                                 (codestring s)))
              env)
          (list 'string-copy (unescape-ats s)))
      (list 'string-copy s)))     ; avoid immutable strings

(define (literal? x)
  (or (boolean? x)
      (char? x)
      (string? x)
      (number? x)
      (ar-nil? x)))

(define (ssyntax? x)
  (and (symbol? x)
       (not (or (eqv? x '+) (eqv? x '++) (eqv? x '_)))
       (let ((name (symbol->string x)))
         (has-ssyntax-char? name (- (string-length name) 2)))))

(define (has-ssyntax-char? string i)
  (and (>= i 0)
       (or (let ((c (string-ref string i)))
             (or (eqv? c #\:) (eqv? c #\~)
                 (eqv? c #\&)
                 ;(eqv? c #\_)
                 (eqv? c #\.)  (eqv? c #\!)))
           (has-ssyntax-char? string (- i 1)))))

(define (read-from-string str)
  (let ((port (open-input-string str)))
    (let ((val (read port)))
      (close-input-port port)
      val)))

; Though graphically the right choice, can't use _ for currying
; because then _!foo becomes a function.  Maybe use <>.  For now
; leave this off and see how often it would have been useful.

; Might want to make ~ have less precedence than &, because
; ~foo&bar prob should mean (andf (complement foo) bar), not
; (complement (andf foo bar)).

(define (expand-ssyntax sym)
  ((cond ((insym? #\& sym) expand-and)
         ((or (insym? #\: sym) (insym? #\~ sym)) expand-compose)
         ((or (insym? #\. sym) (insym? #\! sym)) expand-sexpr)
     ;   ((insym? #\_ sym) expand-curry)
         (#t (error "Unknown ssyntax" sym)))
   sym))

(define (expand-compose sym)
  (let ((elts (map (lambda (tok)
                     (if (eqv? (car tok) #\~)
                         (if (null? (cdr tok))
                             'no
                             `(complement ,(chars->value (cdr tok))))
                         (chars->value tok)))
                   (tokens (lambda (c) (eqv? c #\:))
                           (symbol->chars sym)
                           '()
                           '()
                           #f))))
    (if (null? (cdr elts))
        (car elts)
        (cons 'compose elts))))

(define (expand-and sym)
  (let ((elts (map chars->value
                   (tokens (lambda (c) (eqv? c #\&))
                           (symbol->chars sym)
                           '()
                           '()
                           #f))))
    (if (null? (cdr elts))
        (car elts)
        (cons 'andf elts))))

; How to include quoted arguments?  Can't treat all as quoted, because
; never want to quote fn given as first.  Do we want to allow quote chars
; within symbols?  Could be ugly.

; If release, fix the fact that this simply uses v0... as vars.  Should
; make these vars gensyms.

(define (expand-curry sym)
  (let ((expr (exc (map (lambda (x)
                          (if (pair? x) (chars->value x) x))
                        (tokens (lambda (c) (eqv? c #\_))
                                (symbol->chars sym)
                                '()
                                '()
                                #t))
                    0)))
    (list 'fn
          (keep (lambda (s)
                  (and (symbol? s)
                       (eqv? (string-ref (symbol->string s) 0)
                             #\v)))
                expr)
          expr)))

(define (keep f xs)
  (cond ((null? xs) '())
        ((f (car xs)) (cons (car xs) (keep f (cdr xs))))
        (#t (keep f (cdr xs)))))

(define (exc elts n)
  (cond ((null? elts)
         '())
        ((eqv? (car elts) #\_)
         (cons (string->symbol (string-append "v" (number->string n)))
               (exc (cdr elts) (+ n 1))))
        (#t
         (cons (car elts) (exc (cdr elts) n)))))

(define (expand-sexpr sym)
  (build-sexpr (reverse (tokens (lambda (c) (or (eqv? c #\.) (eqv? c #\!)))
                                (symbol->chars sym)
                                '()
                                '()
                                #t))
               sym))

(define (build-sexpr toks orig)
  (cond ((null? toks)
         'get)
        ((null? (cdr toks))
         (chars->value (car toks)))
        (#t
         (list (build-sexpr (cddr toks) orig)
               (if (eqv? (cadr toks) #\!)
                   (list 'quote (chars->value (car toks)))
                   (if (or (eqv? (car toks) #\.) (eqv? (car toks) #\!))
                       (err "Bad ssyntax" orig)
                       (chars->value (car toks))))))))

(define (insym? char sym) (member char (cdr (reverse (symbol->chars sym)))))

(define (symbol->chars x) (string->list (symbol->string x)))

(define (chars->value chars) (read-from-string (list->string chars)))

(define (tokens test source token acc keepsep?)
  (cond ((null? source)
         (reverse (if (pair? token)
                      (cons (reverse token) acc)
                      acc)))
        ((test (car source))
         (tokens test
                 (cdr source)
                 '()
                 (let ((rec (if (null? token)
                            acc
                            (cons (reverse token) acc))))
                   (if keepsep?
                       (cons (car source) rec)
                       rec))
                 keepsep?))
        (#t
         (tokens test
                 (cdr source)
                 (cons (car source) token)
                 acc
                 keepsep?))))

(define (ac-global-name s)
  (string->symbol (string-append "_" (symbol->string s))))

(define (ac-var-ref s env)
  (cond ((ac-boxed? 'get s) (ac-boxed-get s))
        ((lex? s env)       s)
        (#t                 (ac-global-name s))))

; quote

(define (ac-quoted x)
  (cond ((pair? x)
         (imap (lambda (x) (ac-quoted x)) x))
        ((eqv? x 'nil)
         ar-nil)
        ((eqv? x 't)
         ar-t)
        (#t x)))

(define (ac-unquoted x)
  (cond ((pair? x)
         (imap (lambda (x) (ac-unquoted x)) x))
        ((ar-nil? x)
         'nil)
        ((eqv? x ar-t)
         't)
        (#t x)))

; quasiquote

(define (ac-qq args env)
  (list 'quasiquote (ac-qq1 1 args env)))

; process the argument of a quasiquote. keep track of
; depth of nesting. handle unquote only at top level (level = 1).
; complete form, e.g. x or (fn x) or (unquote (fn x))

(define (ac-qq1 level x env)
  (cond ((= level 0)
         (ac x env))
        ((and (pair? x) (eqv? (car x) 'unquote))
         (list 'unquote (ac-qq1 (- level 1) (cadr x) env)))
        ((and (pair? x) (eqv? (car x) 'unquote-splicing) (= level 1))
         (list 'unquote-splicing
               (ac-qq1 (- level 1) (cadr x) env)))
        ((and (pair? x) (eqv? (car x) 'quasiquote))
         (list 'quasiquote (ac-qq1 (+ level 1) (cadr x) env)))
        ((pair? x)
         (imap (lambda (x) (ac-qq1 level x env)) x))
        ((eqv? x 'nil)
         ar-nil)
        ((eqv? x 't)
         ar-t)
        (#t x)))

; like map, but don't demand '()-terminated list

(define (imap f l)
  (cond ((pair? l)
         (cons (f (car l)) (imap f (cdr l))))
        ((null? l)
         '())
        (#t (f l))))

; (if) -> nil
; (if x) -> x
; (if t a ...) -> a
; (if nil a b) -> b
; (if nil a b c) -> (if b c)

(define (ac-if args env)
  (cond ((null? args) (list 'quote ar-nil))
        ((null? (cdr args)) (ac (car args) env))
        (#t `(if (not (ar-false? ,(ac (car args) env)))
                 ,(ac (cadr args) env)
                 ,(ac-if (cddr args) env)))))

(define (ac-dbname! name env)
  (if (symbol? name)
      (cons (list name) env)
      env))

(define (ac-dbname env)
  (cond ((null? env) #f)
        ((pair? (car env)) (caar env))
        (#t (ac-dbname (cdr env)))))

; translate fn directly into a lambda if it has ordinary
; parameters, otherwise use a rest parameter and parse it.

(define (ac-fn args body env)
  (if (ac-complex-args? args)
      (ac-complex-fn args body env)
      (ac-nameit
       (ac-dbname env)
       `(lambda ,args
          ,@(ac-body* body (append (ac-arglist args) env))))))

; does an fn arg list use optional parameters or destructuring?
; a rest parameter is not complex

(define (ac-complex-args? args)
  (cond ((null? args) #f)
        ((symbol? args) #f)
        ((and (pair? args) (symbol? (car args)))
         (ac-complex-args? (cdr args)))
        (#t #t)))

; translate a fn with optional or destructuring args
; (fn (x (o y x) (o z 21) (x1 x2) . rest) ...)
; arguments in top-level list are mandatory (unless optional),
; but it's OK for parts of a list you're destructuring to
; be missing.

(define (ac-complex-fn args body env)
  (let* ((ra (ar-gensym))
         (z (ac-complex-args args env ra #t)))
    `(lambda ,ra
       (let* ,z
         ,@(ac-body* body (append (ac-complex-getargs z) env))))))

; returns a list of two-element lists, first is variable name,
; second is (compiled) expression. to be used in a let.
; caller should extract variables and add to env.
; ra is the rest argument to the fn.
; is-params indicates that args are function arguments
;   (not destructuring), so they must be passed or be optional.

(define (ac-complex-args args env ra is-params)
  (cond ((null? args) '())
        ((symbol? args) (list (list args ra)))
        ((pair? args)
         (let* ((x (if (and (pair? (car args)) (eqv? (caar args) 'o))
                       (ac-complex-opt (cadar args)
                                       (if (pair? (cddar args))
                                           (caddar args)
                                           ar-nil)
                                       env
                                       ra)
                       (ac-complex-args
                        (car args)
                        env
                        (if is-params
                            `(car ,ra)
                            `(ar-xcar ,ra))
                        #f)))
                (xa (ac-complex-getargs x)))
           (append x (ac-complex-args (cdr args)
                                      (append xa env)
                                      `(ar-xcdr ,ra)
                                      is-params))))
        (#t (err "Can't understand fn arg list" args))))

; (car ra) is the argument
; so it's not present if ra is nil or '()

(define (ac-complex-opt var expr env ra)
  (list (list var `(if (pair? ,ra) (car ,ra) ,(ac expr env)))))

; extract list of variables from list of two-element lists.

(define (ac-complex-getargs a)
  (map (lambda (x) (car x)) a))

; (a b . c) -> (a b c)
; a -> (a)

(define (ac-arglist a)
  (cond ((null? a) '())
        ((symbol? a) (list a))
        ((symbol? (cdr a)) (list (car a) (cdr a)))
        (#t (cons (car a) (ac-arglist (cdr a))))))

(define (ac-body body env)
  (map (lambda (x) (ac x env)) body))

; like ac-body, but spits out a nil expression if empty

(define (ac-body* body env)
  (if (null? body)
      (list (list 'quote ar-nil))
      (ac-body body env)))

; (set v1 expr1 v2 expr2 ...)

(define (ac-set x env)
  `(begin ,@(ac-setn x env)))

(define (ac-setn x env)
  (if (null? x)
      '()
      (cons (ac-set1 (ac-macex (car x)) (cadr x) env)
            (ac-setn (cddr x) env))))

; trick to tell Scheme the name of something, so Scheme
; debugging and profiling make more sense.

(define (ac-nameit name v)
  (if (symbol? name)
      (let ((n (string->symbol (string-append " " (symbol->string name)))))
        (list 'let `((,n ,v)) n))
      v))

; = replaced by set, which is only for vars
; = now defined in arc (is it?)
; name is to cause fns to have their arc names for debugging

(define (ac-set1 a b1 env)
  (if (symbol? a)
      (let ((n (string->symbol (string-append " " (symbol->string a))))
            (b (ac b1 (ac-dbname! a env))))
        (list 'let `((,n ,b))
               (cond ((eqv? a 'nil) (err "Can't rebind nil"))
                     ((eqv? a 't) (err "Can't rebind t"))
                     ((eqv? a 'true) (err "Can't rebind true"))
                     ((eqv? a 'false) (err "Can't rebind false"))
                     ((ac-boxed? 'set a)  `(begin ,(ac-boxed-set a b) ,(ac-boxed-get a)))
                     ((lex? a env) `(set! ,a ,n))
                     (#t `(namespace-set-variable-value! ',(ac-global-name a)
                                                         ,n)))
               n))
      (err "First arg to set must be a symbol" a)))

; given a list of Arc expressions, return a list of Scheme expressions.
; for compiling passed arguments.

(define (ac-args names exprs env)
  (if (null? exprs)
      '()
      (cons (ac (car exprs)
                (ac-dbname! (if (pair? names) (car names) #f) env))
            (ac-args (if (pair? names) (cdr names) '())
                     (cdr exprs)
                     env))))

(define (ac-lexname env)
  (let ((name (ac-dbname env)))
    (if (eqv? name #f)
        'fn
        (apply string-append
               (map (lambda (x) (string-append (symbol->string x) "-"))
                    (apply append (keep pair? env)))))))

(define (ac-lenv args env)
  (ac-lexenv (ac-lexname env) env))

(define (ac-lexenv name env)
  `(list (list '*name ',name)
         ,@(imap (lambda (var)
                   (let ((val (ar-gensym)))
                     `(list ',var
                            (lambda ,val ,var)
                            (lambda (,val) (set! ,var ,val)))))
                 (filter (lambda (x) (not (or (ar-false? x) (pair? x)))) env))))

(define boxed* '())

(define (ac-boxed? op name)
  (let ((result
    (when (not (ar-false? name))
      (when (not (ar-false? boxed*))
        (let ((slot (assoc name boxed*)))
          (case op
            ((get) (when (and slot (>= (length slot) 2)) (cadr slot)))
            ((set) (when (and slot (>= (length slot) 3)) (caddr slot)))
            (else (err "ac-boxed?: bad op" name op))))))))
    (if (void? result) #f result)))

(define (ac-boxed-set name val)
  (let ((setter (ac-boxed? 'set name)))
     (if (procedure? setter)
       `(,setter ,val)
       (err "invalid setter" name val setter))))

(define (ac-boxed-get name)
  (let ((getter (ac-boxed? 'get name)))
    (if (procedure? getter)
      `(,getter 'nil)
      getter)))

; generate special fast code for ordinary two-operand
; calls to the following functions. this is to avoid
; calling e.g. ar-is with its &rest and apply.

(define ac-binaries
  '((is ar-is2)
    (< ar-<2)
    (> ar->2)
    (+ ar-+2)))

; (foo bar) where foo is a global variable bound to a procedure.

(define (ac-global-call fn args env)
  (cond ((and (assoc fn ac-binaries) (= (length args) 2))
         `(,(cadr (assoc fn ac-binaries)) ,@(ac-args '() args env)))
        (#t
         `(,(ac-global-name fn) ,@(ac-args '() args env)))))

; compile a function call
; special cases for speed, to avoid compiled output like
;   (ar-apply _pr (list 1 2))
; which results in 1/2 the CPU time going to GC. Instead:
;   (ar-funcall2 _pr 1 2)
; and for (foo bar), if foo is a reference to a global variable,
;   and it's bound to a function, generate (foo bar) instead of
;   (ar-funcall1 foo bar)

(define direct-calls #f)

(define (ac-call fn args env)
  (let ((macfn (ac-macro? fn)))
    (cond (macfn
           (ac-mac-call macfn args env))
          ((and (pair? fn) (eqv? (car fn) 'fn))
           `(,(ac fn env) ,@(ac-args (cadr fn) args env)))
          ((and direct-calls (symbol? fn) (not (lex? fn env)) (bound? fn)
                (procedure? (bound? fn)))
           (ac-global-call fn args env))
          ((memf keyword? args)
           `(,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 0)
           `(ar-funcall0 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 1)
           `(ar-funcall1 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 2)
           `(ar-funcall2 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 3)
           `(ar-funcall3 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 4)
           `(ar-funcall4 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          (#t
           `(ar-apply ,(ac fn env)
                      (list ,@(map (lambda (x) (ac x env)) args)))))))

(define (ac-mac-call m args env)
  (let ((x1 (apply m args)))
    (let ((x2 (ac x1 env)))
      x2)))

; returns #f or the macro function

(define (ac-macro? fn)
  (if (symbol? fn)
      (let ((v (bound? fn)))
        (if (and v
                 (ar-tagged? v)
                 (eq? (ar-type v) 'mac))
            (ar-rep v)
            #f))
      #f))

; macroexpand the outer call of a form as much as possible

(define (ac-macex e . once)
  (if (pair? e)
      (let ((m (ac-macro? (car e))))
        (if m
            (let ((expansion (apply m (cdr e))))
              (if (null? once) (ac-macex expansion) expansion))
            e))
      e))

; is v lexically bound?

(define (lex? v env)
  (memq v env))

(define (xcar x)
  (and (pair? x) (car x)))

; The next two are optimizations, except work for macros.

(define (decompose fns args)
  (cond ((null? fns) `((fn vals (car vals)) ,@args))
        ((null? (cdr fns)) (cons (car fns) args))
        (#t (list (car fns) (decompose (cdr fns) args)))))

(define (ac-andf s env)
  (ac (let ((gs (map (lambda (x) (ar-gensym)) (cdr s))))
               `((fn ,gs
                   (and ,@(map (lambda (f) `(,f ,@gs))
                               (cdar s))))
                 ,@(cdr s)))
      env))

(define err error)

(define-namespace-anchor arc-anchor)
; (define (arc-namespace) (namespace-anchor->namespace arc-anchor))

(define arc-namespace current-namespace)

; run-time primitive procedures

;(define (xdef a b)
;  (namespace-set-variable-value! (ac-global-name a) b)
;  b)

(define-syntax xdef
  (syntax-rules ()
    ((xxdef a b)
     (let ((nm (ac-global-name 'a))
           (a b))
       (namespace-set-variable-value! nm a)))))

(define fn-signatures (make-hash))

; This is a replacement for xdef that stores opeator signatures.
; Haven't started using it yet.

(define (odef a parms b)
  (namespace-set-variable-value! (ac-global-name a) b)
  (hash-set! fn-signatures a (list parms))
  b)

(xdef sig fn-signatures)

(xdef quoted ac-quoted)

(xdef unquoted ac-unquoted)

; versions of car and cdr for parsing arguments for optional
; parameters, that yield nil for nil. maybe we should use
; full Arc car and cdr, so we can destructure more things

(define (ar-xcar x)
  (if (ar-nil? x) x (car x)))

(define (ar-xcdr x)
  (if (ar-nil? x) x (cdr x)))

; convert #f from a Scheme predicate to NIL.

(define (ar-nill x)
  (if (or (ar-nil? x) (eq? x #f) (void? x)) ar-nil x))

; definition of falseness for Arc if.
; must include '() since sometimes Arc functions see
; Scheme lists (e.g. . body of a macro).

(define (ar-false? x)
  (or (ar-nil? x) (eq? x #f)))

; call a function or perform an array ref, hash ref, &c

; Non-fn constants in functional position are valuable real estate, so
; should figure out the best way to exploit it.  What could (1 foo) or
; ('a foo) mean?  Maybe it should mean currying.

; For now the way to make the default val of a hash table be other than
; nil is to supply the val when doing the lookup.  Later may also let
; defaults be supplied as an arg to table.  To implement this, need: an
; eq table within scheme mapping tables to defaults, and to adapt the
; code in arc.arc that reads and writes tables to read and write their
; default vals with them.  To make compatible with existing written tables,
; just use an atom or 3-elt list to keep the default.

(define (ar-apply fn args)
  (cond ((procedure? fn)
         (apply fn args))
        ((pair? fn)
         (list-ref fn (car args)))
        ((ar-nil? fn)
         fn)
        ((string? fn)
         (string-ref fn (car args)))
        ((hash? fn)
         (hash-ref fn
                   (car args)
                   (if (pair? (cdr args)) (cadr args) ar-nil)))
; experiment: means e.g. [1] is a constant fn
;       ((or (number? fn) (symbol? fn)) fn)
; another possibility: constant in functional pos means it gets
; passed to the first arg, i.e. ('kids item) means (item 'kids).
        (#t (err "Function call on inappropriate object" fn args))))

(xdef apply (lambda (fn . args)
               (ar-apply fn (ar-apply-args args))))

; special cases of ar-apply for speed and to avoid consing arg lists

(define (ar-funcall0 fn)
  (if (procedure? fn)
      (fn)
      (ar-apply fn (list))))

(define (ar-funcall1 fn arg1)
  (if (procedure? fn)
      (fn arg1)
      (ar-apply fn (list arg1))))

(define (ar-funcall2 fn arg1 arg2)
  (if (procedure? fn)
      (fn arg1 arg2)
      (ar-apply fn (list arg1 arg2))))

(define (ar-funcall3 fn arg1 arg2 arg3)
  (if (procedure? fn)
      (fn arg1 arg2 arg3)
      (ar-apply fn (list arg1 arg2 arg3))))

(define (ar-funcall4 fn arg1 arg2 arg3 arg4)
  (if (procedure? fn)
      (fn arg1 arg2 arg3 arg4)
      (ar-apply fn (list arg1 arg2 arg3 arg4))))

; turn the arguments to Arc apply into a list.
; if you call (apply fn 1 2 '(3 4))
; then args is '(1 2 (3 4))
; and we should return '(1 2 3 4)

(define (ar-apply-args args)
  (cond ((null? args) args)
        ((null? (cdr args)) (car args))
        (#t (cons (car args) (ar-apply-args (cdr args))))))





(xdef cons cons)

(xdef car (lambda (x)
             (cond ((pair? x)     (car x))
                   ((null? x)     x)
                   (#t            (err "Can't take car of" x)))))

(xdef cdr (lambda (x)
             (cond ((pair? x)     (cdr x))
                   ((null? x)     x)
                   (#t            (err "Can't take cdr of" x)))))

(define (tnil x) (if x ar-t ar-nil))

; (pairwise pred '(a b c d)) =>
;   (and (pred a b) (pred b c) (pred c d))
; pred returns t/nil, as does pairwise
; reduce?

(define (pairwise pred lst)
  (cond ((null? lst) ar-t)
        ((null? (cdr lst)) ar-t)
        ((not (ar-nil? (pred (car lst) (cadr lst))))
         (pairwise pred (cdr lst)))
        (#t ar-nil)))

; not quite right, because behavior of underlying eqv unspecified
; in many cases according to r5rs
; do we really want is to ret t for distinct strings?

; for (is x y)

(define (ar-is2 a b)
  (tnil (or (eqv? a b)
            (and (string? a) (string? b) (string=? a b))
            (and (ar-false? a) (ar-false? b)))))

; for all other uses of is

(xdef is (lambda args (pairwise ar-is2 args)))

(xdef raise raise)
(xdef err err)
(xdef nil ar-nil)
(xdef t   ar-t)
(xdef false #f)
(xdef true  #t)

(define (all test seq)
  (or (null? seq)
      (and (test (car seq)) (all test (cdr seq)))))

(define (arc-list? x) (or (pair? x) (ar-nil? x)))

; Generic +: strings, lists, numbers.
; Return val has same type as first argument.

(xdef + (lambda args
           (cond ((null? args) 0)
                 ((char-or-string? (car args))
                  (apply string-append
                         (map (lambda (a) (ar-coerce a 'string))
                              args)))
                 ((arc-list? (car args))
                  (apply append args))
                 ((evt? (car args))
                  (apply choice-evt args))
                 (#t (apply + args)))))

(define (char-or-string? x) (or (string? x) (char? x)))

(define (ar-+2 x y)
  (cond ((char-or-string? x)
         (string-append (ar-coerce x 'string) (ar-coerce y 'string)))
        ((and (arc-list? x) (arc-list? y))
         (append x y))
        (#t (+ x y))))

(xdef - -)
(xdef * *)
(xdef / /)
(xdef mod modulo)
(xdef expt expt)
(xdef sqrt sqrt)

; generic comparison

(define (ar->2 x y)
  (tnil (cond ((and (number? x) (number? y)) (> x y))
              ((and (string? x) (string? y)) (string>? x y))
              ((and (symbol? x) (symbol? y)) (string>? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x) (char? y)) (char>? x y))
              (#t (> x y)))))

(xdef > (lambda args (pairwise ar->2 args)))

(define (ar-<2 x y)
  (tnil (cond ((and (number? x) (number? y)) (< x y))
              ((and (string? x) (string? y)) (string<? x y))
              ((and (symbol? x) (symbol? y)) (string<? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x) (char? y)) (char<? x y))
              (#t (< x y)))))

(xdef < (lambda args (pairwise ar-<2 args)))

(xdef len (lambda (x)
             (cond ((string? x) (string-length x))
                   ((hash? x) (hash-count x))
                   (#t (length x)))))

(define (ar-tag type rep)
  (cond ((eqv? (ar-type rep) type) rep)
        (#t (ar-tagged type rep))))

(xdef annotate ar-tag)

; (type nil) -> sym

(define (exint? x) (and (integer? x) (exact? x)))

(define (ar-type x)
  (cond ((ar-tagged? x)     (ar-tagged-type x))
        ((pair? x)          'cons)
        ((symbol? x)        'sym)
        ((null? x)          'sym)
        ((procedure? x)     'fn)
        ((char? x)          'char)
        ((string? x)        'string)
        ((exint? x)         'int)
        ((number? x)        'num)     ; unsure about this
        ((hash? x)          'table)
        ((output-port? x)   'output)
        ((input-port? x)    'input)
        ((tcp-listener? x)  'socket)
        ((exn? x)           'exception)
        ((thread? x)        'thread)
        ((channel? x)       'channel)
        ((async-channel? x) 'channel)
        ((evt? x)           'event)
        ((keyword? x)       'keyword)
        ((boolean? x)       'bool)
        ((syntax? x)        'syntax)
        (#t                 (err "Type: unknown type" x))))
(xdef type ar-type)

(define (ar-rep x)
  (if (ar-tagged? x)
      (ar-tagged-rep x)
      x))

(xdef rep ar-rep)

(define (ar-gensym (x 'x))
  (if (or (and (symbol? x)
               (not (ssyntax? x)))
          (string? x))
      (gensym x)
      (gensym 'cons)))

(xdef uniq ar-gensym)

(xdef ccc call-with-current-continuation)

(xdef call/ec call-with-escape-continuation)

(xdef modtime file-or-directory-modify-seconds)

(xdef infile  open-input-file)

(xdef outfile (lambda (f . args)
                 (open-output-file f
                                   #:mode 'text
                                   #:exists (if (equal? args '(append))
                                                'append
                                                'truncate))))

(xdef instring  open-input-string)
(xdef outstring open-output-string)

; use as general fn for looking inside things

(xdef inside get-output-string)

(xdef stdout current-output-port)  ; should be a vars
(xdef stdin  current-input-port)
(xdef stderr current-error-port)

(xdef call-w/param
      (lambda (var val thunk)
        (parameterize ((var val)) (thunk))))

(xdef call-w/stdout
      (lambda (port thunk)
        (parameterize ((current-output-port port)) (thunk))))

(xdef call-w/stdin
      (lambda (port thunk)
        (parameterize ((current-input-port port)) (thunk))))

(xdef readc (lambda str
              (let ((c (read-char (if (pair? str)
                                      (car str)
                                      (current-input-port)))))
                (if (eof-object? c) ar-nil c))))


(xdef readb (lambda str
              (let ((c (read-byte (if (pair? str)
                                      (car str)
                                      (current-input-port)))))
                (if (eof-object? c) ar-nil c))))

(define (ready? check peek i fail)
  (atomic-invoke
    (lambda ()
      (if (check i) (peek i) fail))))

(xdef peekc (lambda str
              (let* ((i (if (pair? str) (car str) (current-input-port)))
                     (c (ready? char-ready? peek-char i eof)))
                (if (eof-object? c) ar-nil c))))

(xdef peekb (lambda str
              (let* ((i (if (pair? str) (car str) (current-input-port)))
                     (c (ready? byte-ready? peek-byte i eof)))
                (if (eof-object? c) ar-nil c))))

(xdef writec (lambda (c . args)
                (write-char c
                            (if (pair? args)
                                (car args)
                                (current-output-port)))
                c))

(xdef writeb (lambda (b . args)
                (write-byte b
                            (if (pair? args)
                                (car args)
                                (current-output-port)))
                b))

(define explicit-flush #f)

(define (printwith f args)
  (let ((port (if (> (length args) 1)
                  (cadr args)
                  (current-output-port))))
    (when (pair? args)
      (f (car args) port))
    (unless explicit-flush (flush-output port)))
  ar-nil)

(xdef write (lambda args (printwith write   args)))
(xdef disp  (lambda args (printwith display args)))

(xdef sdata sdata)
(xdef sread sread)

; these work in PLT but not scheme48

(define char->ascii char->integer)
(define ascii->char integer->char)

(define (keyword->symbol x) (string->symbol (keyword->string x)))
(define (symbol->keyword x) (string->keyword (symbol->string x)))

(define (iround x) (inexact->exact (round x)))

(define (ar-coerce x type . args)
  (cond
    ((ar-tagged? x) (err "Can't coerce annotated object"))
    ((eqv? type (ar-type x)) x)
    ((char? x)      (case type
                      ((int)     (char->ascii x))
                      ((string)  (string x))
                      ((sym)     (string->symbol (string x)))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((exint? x)     (case type
                      ((num)     x)
                      ((char)    (ascii->char x))
                      ((string)  (apply number->string x args))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((number? x)    (case type
                      ((int)     (iround x))
                      ((char)    (ascii->char (iround x)))
                      ((string)  (apply number->string x args))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((string? x)    (case type
                      ((sym)     (string->symbol x))
                      ((cons)    (string->list x))
                      ((keyword) (string->keyword x))
                      ((bytes)   (if (null? args)
                                     (bytes->list (string->bytes/latin-1 x))
                                     (bytes->list (string->bytes/utf-8 x))))
                      ((num)     (or (apply string->number x args)
                                     (err "Can't coerce" x type)))
                      ((int)     (let ((n (apply string->number x args)))
                                   (if n
                                       (iround n)
                                       (err "Can't coerce" x type))))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((pair? x)      (case type
                      ((string)  (if (byte? (xcar x))
                                     (if (null? args)
                                         (bytes->string/latin-1 (list->bytes x))
                                         (bytes->string/utf-8 (list->bytes x)))
                                     (apply string-append
                                            (map (lambda (y) (ar-coerce y 'string))
                                                 x))))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((ar-nil? x)    (case type
                      ((bytes)   ar-nil)
                      ((string)  "")
                      ((bool)    #f)
                      ((keyword) (string->keyword ""))
                      (else      (err "Can't coerce" x type))))
    ((keyword? x)    (case type
                      ((string)  (keyword->string x))
                      ((sym)     (keyword->symbol x))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((symbol? x)    (case type
                      ((string)  (symbol->string x))
                      ((keyword) (symbol->keyword x))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    (#t             x)))

(xdef coerce ar-coerce)

(xdef open-socket  (lambda (num) (tcp-listen num 50 #t)))

; the 2050 means http requests currently capped at 2 meg
; http://list.cs.brown.edu/pipermail/plt-scheme/2005-August/009414.html

(xdef socket-accept (lambda (s)
                      (let ((oc (current-custodian))
                            (nc (make-custodian)))
                        (current-custodian nc)
                        (call-with-values
                         (lambda () (tcp-accept s))
                         (lambda (in out)
                           (let ((in1 (make-limited-input-port in 100000 #t)))
                             (current-custodian oc)
                             (associate-custodian nc in1 out)
                             (list in1
                                   out
                                   (let-values (((us them) (tcp-addresses out)))
                                               them))))))))

; allow Arc to give up root privileges after it
; calls open-socket. thanks, Eli!
; (define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)
;                  ; dummy version for Windows: http://arclanguage.org/item?id=10625.
;                  (lambda () (lambda (x) ar-nil))))
; (xdef setuid setuid)
(xdef setuid (lambda args ar-nil))

(xdef new-thread thread)
(xdef kill-thread kill-thread)
(xdef break-thread break-thread)
(xdef current-thread current-thread)
(xdef wait (lambda (thd)
             (when (thread? thd)
               (unless (eqv? thd (current-thread))
                 (thread-wait thd)))
             ar-t))

(define (wrapnil f) (lambda args (apply f args) ar-nil))

(xdef sleep (wrapnil sleep))

; Will system "execute" a half-finished string if thread killed
; in the middle of generating it?

(xdef system (if (eqv? (system-type) 'windows) (lambda args ar-nil) (wrapnil system)))

(define (rmrf path)
  (delete-directory/files path #:must-exist? #f))
(xdef rmrf rmrf)

(xdef ensure-dir (wrapnil make-directory*))

; PLT scheme provides only eq? and equal? hash tables,
; we need the latter for strings.

(xdef table (lambda args
              (let ((h (make-hash)))
                (when (pair? args) ((car args) h))
                h)))

;(xdef table (lambda args
;               (fill-table (make-hash)
;                           (if (pair? args) (car args) ar-nil))))

(define (fill-table h pairs)
  (if (ar-nil? pairs)
      h
      (let ((pair (car pairs)))
        (begin (hash-set! h (car pair) (cadr pair))
               (fill-table h (cdr pairs))))))

(xdef maptable (lambda (fn table)               ; arg is (fn (key value) ...)
                  (hash-for-each table fn)
                  table))

(define (protect during after)
  (dynamic-wind (lambda () #t) during after))

(xdef protect protect)

; need to use a better seed

(xdef rand random)

(xdef dir (lambda (name)
            (map path->string (directory-list name))))

; Would def mkdir in terms of make-directory and call that instead
; of system in ensure-dir, but make-directory is too weak: it doesn't
; create intermediate directories like mkdir -p.

(xdef file-exists (lambda (name)
                     (if (file-exists? name) name ar-nil)))

(xdef dir-exists (lambda (name)
                     (if (directory-exists? name) name ar-nil)))

(xdef rmfile (wrapnil delete-file))

(xdef mvfile (lambda (old new)
                (rename-file-or-directory old new #t)
                ar-nil))

(define-syntax w/restore
  (syntax-rules ()
    ((_ var val body ...)
     (let ((w/restore-prev var)
           (w/restore-val  val))
       (dynamic-wind (lambda () (set! var w/restore-val))
                     (lambda () body ...)
                     (lambda () (set! var w/restore-prev)))))))

; top level read-eval-print
; tle kept as a way to get a break loop when a scheme err

(define (arc-eval expr . args)
  (if (null? args)
      (seval (ac expr))
      (apply arc-eval-boxed expr args)))

(define (arc-eval-boxed expr lexenv)
  (w/restore boxed* (if (or (ar-false? boxed*)
                            (ar-false? lexenv))
                      lexenv
                      (append lexenv boxed*))
    (arc-eval expr)))


(define (tle)
  (display "Arc> ")
  (let ((expr (read)))
    (when (not (eqv? expr ':a))
      (write (arc-eval expr))
      (newline)
      (tle))))

(define (tl)
  (display "Use (quit) to quit, (tl) to return here after an interrupt.\n")
  (tl2))

(define ac-that-expr* (make-parameter (void)))

(define (ac-read-interaction src in)
  (parameterize ((read-accept-reader #t)
                 (read-accept-lang #f))
    (let ((stx (read-syntax src in)))
      (if (eof-object? stx) stx
        (if (eq? (xcar (syntax->datum stx)) 'unquote) stx
          (begin (ac-that-expr* stx)
                 (ac stx)))))))

 (define (ac-prompt-read)
   (if (syntax? (ac-that-expr*))
       (display (format "arc:~a> " (source-location-line (ac-that-expr*))))
       (display "arc:0> "))
  (let ((in ((current-get-interaction-input-port))))
    (parameterize ((current-read-interaction ac-read-interaction))
      ((current-read-interaction) (object-name in) in))))
 
(define (ac-prompt-print val)
  (namespace-set-variable-value! (ac-global-name 'that) val)
  (namespace-set-variable-value! (ac-global-name 'thatexpr) (ac-that-expr*))
  (unless (void? val)
    (pretty-print val))
  val)

(define (tl2)
  (parameterize ((port-count-lines-enabled #t)
                 (current-namespace (arc-namespace))
                 (current-prompt-read ac-prompt-read)
                 (current-print       ac-prompt-print)
                 ; ((dynamic-require 'readline/pread 'current-prompt) #"arc> ")
                 ; (current-prompt-read (dynamic-require 'readline/pread 'read-cmdline-syntax))
                 )
    ; (dynamic-require 'xrepl #f)
    (port-count-lines! (current-input-port))
    (read-eval-print-loop)))
 
(define (cwd)
  (path->string (find-system-path 'orig-dir)))

(define-syntax-rule (get-here)
  (begin 
    (let ((ccr (current-contract-region)))
      (let-values (((here-dir here-name ignored) (split-path ccr)))
        (build-path here-dir here-name)))))

(define ac-load-path
  (list (path->string (path-only (path->complete-path (find-system-path 'run-file))))
        (cwd)))

(xdef load-path ac-load-path)

(define (aload1 p)
  (let ((x (sread p)))
    (if (eof-object? x)
        #t
        (begin
          (arc-eval x)
          (aload1 p)))))

(define (atests1 p)
  (let ((x (sread p)))
    (if (eof-object? x)
        #t
        (begin
          (write x)
          (newline)
          (let ((v (arc-eval x)))
            (when (ar-false? v)
              (display "  FAILED")
              (newline)))
          (atests1 p)))))

(define (aload filename)
  (call-with-input-file filename aload1))

(define (test filename)
  (call-with-input-file filename atests1))

(define (acompile1 ip op)
  (let ((x (sread ip)))
    (if (eof-object? x)
        #t
        (let ((scm (ac x)))
          (eval scm)
          (pretty-print scm op)
          (newline op)
          (newline op)
          (acompile1 ip op)))))

; compile xx.arc to xx.arc.scm
; useful to examine the Arc compiler output
(define (acompile inname)
  (let ((outname (string-append inname ".scm")))
    (when (file-exists? outname)
      (delete-file outname))
    (call-with-input-file inname
      (lambda (ip)
        (call-with-output-file outname
          (lambda (op)
            (acompile1 ip op)))))))

(xdef macex (lambda (e) (ac-macex e)))

(xdef macex1 (lambda (e) (ac-macex e 'once)))

(xdef eval arc-eval)

(define (seval s (ns (current-namespace)))
  (parameterize ((current-namespace ns)
                 (compile-allow-set!-undefined #t)
                 (compile-enforce-module-constants #f))
    (eval (if (syntax? s)
              (compile-syntax (namespace-syntax-introduce s))
              (compile s)))))

(xdef seval seval)

; If an err occurs in an on-err expr, no val is returned and code
; after it doesn't get executed.  Not quite what I had in mind.

(define (on-err errfn f)
  ((call-with-current-continuation
     (lambda (k)
       (lambda ()
         (with-handlers ((exn:fail? (lambda (c)
                                      (k (lambda () (errfn c))))))
                        (f)))))))
(xdef on-err on-err)

(define (disp-to-string x)
  (let ((o (open-output-string)))
    (display x o)
    (close-output-port o)
    (get-output-string o)))

(xdef details (lambda (c)
                 (disp-to-string (exn-message c))))

(xdef scar (lambda (x val)
              (if (string? x)
                  (string-set! x 0 val)
                  (x-set-car! x val))
              val))

(xdef scdr (lambda (x val)
              (if (string? x)
                  (err "Can't set cdr of a string" x)
                  (x-set-cdr! x val))
              val))

; waterhouse's code to modify Racket's immutable pairs.
; http://arclanguage.org/item?id=13616
(require racket/unsafe/ops)

(define x-set-car!
  (let ((fn (namespace-variable-value 'set-car! #t (lambda () #f))))
    (if (procedure? fn)
        fn
        (lambda (p x)
          (if (pair? p)
              (unsafe-set-immutable-car! p x)
              (raise-type-error 'set-car! "pair" p))))))

(define x-set-cdr!
  (let ((fn (namespace-variable-value 'set-cdr! #t (lambda () #f))))
    (if (procedure? fn)
        fn
        (lambda (p x)
          (if (pair? p)
              (unsafe-set-immutable-cdr! p x)
              (raise-type-error 'set-cdr! "pair" p))))))

; When and if cdr of a string returned an actual (eq) tail, could
; say (if (string? x) (string-replace! x val 1) ...) in scdr, but
; for now would be misleading to allow this, because fails for cddr.

(define (string-replace! str val index)
  (if (eqv? (string-length val) (- (string-length str) index))
      (do ((i index (+ i 1)))
          ((= i (string-length str)) str)
        (string-set! str i (string-ref val (- i index))))
      (err "Length mismatch between strings" str val index)))

; Later may want to have multiple indices.

(xdef sref
  (lambda (com val ind)
    (cond ((hash? com)  (if (ar-nil? val)
                            (hash-remove! com ind)
                            (hash-set! com ind val)))
          ((string? com) (string-set! com ind val))
          ((pair? com)   (nth-set! com ind val))
          (#t (err "Can't set reference " com ind val)))
    val))

(define (nth-set! lst n val)
  (x-set-car! (list-tail lst n) val))

; rewrite to pass a (true) gensym instead of #f in case var bound to #f

(define (bound? arcname)
  (namespace-variable-value (ac-global-name arcname)
                            #t
                            (lambda () #f)))

(xdef bound (lambda (x) (tnil (bound? x))))

(xdef newstring make-string)

(xdef trunc (lambda (x) (inexact->exact (truncate x))))

; bad name

(xdef exact (lambda (x) (tnil (exint? x))))

(xdef msec                         current-milliseconds)
(xdef now                          current-inexact-milliseconds)
(xdef current-process-milliseconds current-process-milliseconds)
(xdef current-gc-milliseconds      current-gc-milliseconds)

(xdef seconds current-seconds)

(print-hash-table #t)

(xdef client-ip (lambda (port)
                   (let-values (((x y) (tcp-addresses port)))
                     y)))

; make sure only one thread at a time executes anything
; inside an atomic-invoke. atomic-invoke is allowed to
; nest within a thread; the thread-cell keeps track of
; whether this thread already holds the lock.

(define ar-the-sema (make-semaphore 1))

(define ar-sema-cell (make-thread-cell #f))

(define atomic-invoke (lambda (f)
                       (if (thread-cell-ref ar-sema-cell)
                           (ar-apply f '())
                           (begin
                             (thread-cell-set! ar-sema-cell #t)
			     (protect
			      (lambda ()
				(call-with-semaphore
				 ar-the-sema
				 (lambda () (ar-apply f '()))))
			      (lambda ()
				(thread-cell-set! ar-sema-cell #f)))))))
(xdef atomic-invoke atomic-invoke)

(xdef dead (lambda (x) (tnil (thread-dead? x))))

(xdef chan (lambda args
             (cond
               ((null? args)           (make-channel))
               ((ar-false? (car args)) (make-async-channel #f))
               ((positive? (car args)) (make-async-channel (car args)))
               ((zero? (car args))     (make-channel))
               (#t (err "Channel limit must be > 0 or nil: " (car args))))))

(define (sync? . args)
  (apply sync/timeout 0 args))

(define (chan-fn c method)
  (cond ((channel? c)
         (cond ((eq? method 'get)     channel-get)
               ((eq? method 'try-get) channel-try-get)
               ((eq? method 'put)     channel-put)
               ((eq? method 'put-evt) channel-put-evt)
               (#t (err "chan-fn: invalid method: " method))))
        ((async-channel? c)
         (cond ((eq? method 'get)     async-channel-get)
               ((eq? method 'try-get) async-channel-try-get)
               ((eq? method 'put)     async-channel-put)
               ((eq? method 'put-evt) async-channel-put-evt)
               (#t (err "chan-fn: invalid method: " method))))
        ((and (evt? c) (or (eq? method 'get) (eq? method 'try-get)))
         sync?)
        (#t (err "chan-fn: invalid channel: " c))))

(xdef <- (lambda (c . args)
           (ar-nill
             (if (null? args)
                 ((chan-fn c 'get) c)
                 (begin ((chan-fn c 'put) c args)
                        args)))))

(xdef <-? (lambda (c . args)
            (ar-nill
              (if (null? args)
                  ((chan-fn c 'try-get) c)
                  (let* ((evt ((chan-fn c 'put-evt) c args))
                         (ret (sync/timeout 0 evt)))
                    (if (eq? ret #f)
                        ar-nil
                        args))))))

; Added because Mzscheme buffers output.  Not a permanent part of Arc.
; Only need to use when declare explicit-flush optimization.

(xdef flushout (lambda () (flush-output) ar-t))

(xdef ssyntax (lambda (x) (tnil (ssyntax? x))))

(xdef ssexpand (lambda (x)
                  (if (ssyntax? x) (expand-ssyntax x) x)))

(xdef quit exit)

; there are two ways to close a TCP output port.
; (close o) waits for output to drain, then closes UNIX descriptor.
; (force-close o) discards buffered output, then closes UNIX desc.
; web servers need the latter to get rid of connections to
; clients that are not reading data.
; mzscheme close-output-port doesn't work (just raises an error)
; if there is buffered output for a non-responsive socket.
; must use custodian-shutdown-all instead.

(define custodians (make-hash))

(define (associate-custodian c i o)
  (hash-set! custodians i c)
  (hash-set! custodians o c))

; if a port has a custodian, use it to close the port forcefully.
; also get rid of the reference to the custodian.
; sadly doing this to the input port also kills the output port.

(define (try-custodian p)
  (let ((c (hash-ref custodians p #f)))
    (if c
        (begin
          (custodian-shutdown-all c)
          (hash-remove! custodians p)
          #t)
        #f)))

(define (ar-close . args)
  (map (lambda (p)
         (cond ((input-port? p)   (close-input-port p))
               ((output-port? p)  (close-output-port p))
               ((tcp-listener? p) (tcp-close p))
               (#t (err "Can't close " p))))
       args)
  (map (lambda (p) (try-custodian p)) args) ; free any custodian
  ar-nil)

(xdef close ar-close)

(xdef force-close (lambda args
                       (map (lambda (p)
                              (unless (try-custodian p)
                                  (ar-close p)))
                            args)
                       ar-nil))

(xdef memory current-memory-use)

(xdef declare (lambda (key val)
                (let ((flag (not (ar-false? val))))
                  (case key
                    ((atstrings)      (set! atstrings      flag))
                    ((direct-calls)   (set! direct-calls   flag))
                    ((explicit-flush) (set! explicit-flush flag)))
                  val)))

(xdef get-environment-variable getenv)
(xdef set-environment-variable putenv)

(void (putenv "TZ" ":GMT"))

(define (gmt-date sec) (seconds->date sec))

(xdef timedate
  (lambda args
    (let ((d (gmt-date (if (pair? args) (car args) (current-seconds)))))
      (list (date-second d)
            (date-minute d)
            (date-hour d)
            (date-day d)
            (date-month d)
            (date-year d)))))

(xdef sin sin)
(xdef cos cos)
(xdef tan tan)
(xdef asin asin)
(xdef acos acos)
(xdef atan atan)
(xdef log log)

(define (codestring s)
  (let ((i (atpos s 0)))
    (if i
        (cons (substring s 0 i)
              (let* ((rest (substring s (+ i 1)))
                     (in (open-input-string rest))
                     (expr (read in))
                     (i2 (let-values (((x y z) (port-next-location in))) z)))
                (close-input-port in)
                (cons expr (codestring (substring rest (- i2 1))))))
        (list s))))

; First unescaped @ in s, if any.  Escape by doubling.

(define (atpos s i)
  (cond ((eqv? i (string-length s))
         #f)
        ((eqv? (string-ref s i) #\@)
         (if (and (< (+ i 1) (string-length s))
                  (not (eqv? (string-ref s (+ i 1)) #\@)))
             i
             (atpos s (+ i 2))))
        (#t
         (atpos s (+ i 1)))))

(define (unescape-ats s)
  (list->string (letrec ((unesc (lambda (cs)
                                  (cond
                                    ((null? cs)
                                     '())
                                    ((and (eqv? (car cs) #\@)
                                          (not (null? (cdr cs)))
                                          (eqv? (cadr cs) #\@))
                                     (unesc (cdr cs)))
                                    (#t
                                     (cons (car cs) (unesc (cdr cs))))))))
                  (unesc (string->list s)))))


(module bcrypt mzscheme
  (require (lib "foreign.ss"))
  (unsafe!)
  (provide bcrypt)
  (define bcrypt* (get-ffi-obj "bcrypt" (if (eqv? (system-type) 'windows) (ffi-lib "src\\bcrypt\\bcrypt") (ffi-lib "src/bcrypt/build/libbcrypt"))
                   (_fun _string _string _pointer -> _void)))

  (define bcrypt ; (passwd salt) see BSD manual crypt(3)
    (let* ((p (malloc 'atomic 256)))
      (lambda (pwd salt . failed)
        (memset p 0 256)
        (bcrypt* pwd salt p)
        (let ((x (cast p _pointer _string)))
          (if (or (<= (string-length x) 0)
                  (not (eqv? (string-ref x 0) #\$)))
              (if (pair? failed)
                  (car failed)
                  (error "bcrypt failed; use a salt like (+ \"$2a$10$\" (rand-string 22))"))
              x))))))
(require 'bcrypt)

(xdef bcrypt (lambda args
               (atomic-invoke (lambda ()
                 (apply bcrypt args)))))

(xdef system-type system-type)

(xdef make-param make-parameter)

(xdef write-json write-json)
(xdef read-json read-json)

(module uuid mzscheme
  (require (lib "foreign.ss"))
  (unsafe!)
  (provide uuid-generate)

  (define uuid-generate
    (unless (eqv? (system-type) 'windows)
      (get-ffi-obj "uuid_generate" (ffi-lib (if (eqv? (system-type 'os) 'macosx) "libSystem" "libuuid") '("1" ""))
        (_fun (out : _bytes = (make-bytes 16)) -> _void -> (uuid-unparse out)))))

  (define uuid-unparse
    (unless (eqv? (system-type) 'windows)
      (get-ffi-obj "uuid_unparse" (ffi-lib (if (eqv? (system-type 'os) 'macosx) "libSystem" "libuuid") '("1" ""))
        (_fun (uuid : _bytes) (out : _bytes = (make-bytes 32)) -> _void -> (cast out _bytes _string/utf-8)))))
  )
(require 'uuid)

(xdef uuid uuid-generate)

