(define-global delimiters (set-of "{" "}" "[" "]" "(" ")" ";" "\r" "\n"))
(define-global whitespace (set-of " " "\t" "\r" "\n"))

(define-global buffers* ())
(define-global make-variables-buffer-local* ())

(define-global buffer? (x)
  (and (obj? x)
       (has? x 'pos)
       (has? x 'string)
       (has? x 'len)))

(define-global stream (str . props)
  (listtab:join (tablist:obj pos 0 string str len (len str) locals (list (list t t))) (tuples props)))

(define-global get-buffer (name)
  (each x buffers*
    (when (is (x 'name) name)
      (return x))))

(define-global get-buffer-create (name)
  (atomic
    (or (get-buffer name)
        (let s (stream "" 'name name)
          (add buffers* s)
          s))))

(define-global the-buffer* (make-param (get-buffer-create "*scratch*")))

(%scheme
  (define (arc-get name (unset (void)))
    (atomic-invoke
      (lambda ()
    (let* ((buf ((bound? 'the-buffer*)))
           (vs (hash-ref buf 'locals))
           (l (assoc name vs)))
      (if l (cadr l) (arc-get-global name unset))))))

  (define (arc-buffer-local? name)
    (atomic-invoke
      (lambda ()
    (let* ((buf ((bound? 'the-buffer*)))
           (vs (hash-ref buf 'locals))
           (l (assoc name vs)))
      (tnil l)))))
  
  (define (arc-set name value)
    (atomic-invoke
      (lambda ()
    (let* ((buf ((bound? 'the-buffer*)))
           (vs (hash-ref buf 'locals))
           (l (assoc name vs)))
      (cond (l (x-set-car! (cdr l) value) value)
            ((member name (bound? 'make-variables-buffer-local*))
             ((bound? 'make-local-variable) name)
             (arc-set name value))
            (else (arc-set-global name value)))))))
  )

(define-global assign-buffer (v)
  (the-buffer* v)
  v)

(define-global current-buffer ()
  (the-buffer*))

(define-global buffer-name ((o buffer))
  (let b (or buffer (current-buffer))
    (b 'name)))

(define-global buffer-file-name ((o buffer))
  (let b (or buffer (current-buffer))
    (b 'file)))

(define-global set-visited-file-name (filename)
  (let b (current-buffer)
    (= (b 'file) filename)))

(define-global point ()
  (+ ((current-buffer) 'pos) 1))

(define-global point-min ()
  (+ (or ((current-buffer) 'start) 0) 1))

(define-global point-max ()
  (+ (or ((current-buffer) 'end) ((current-buffer) 'len)) 1))

(define-global buffer-end ((o flag))
  (if (> flag 1) (point-max) (point-min)))

(define-global buffer-size ((o buffer))
  (let b (or buffer (current-buffer))
    (b 'len)))

(define-global goto-char (n)
  (let n n
    (= n (max (point-min) n))
    (= n (min (point-max) n))
    (= ((current-buffer) 'pos) (- n 1)))
  n)

(define-global char-after ((o position))
  (let p (either position (point))
    (if (or (< p (point-min))
            (>= p (point-max)))
        nil
      (let b (current-buffer)
        (char (b 'string) (- p 1))))))

(define-global char-before ((o position))
  (let p (either position (point))
    (if (or (<= p (point-min))
            (> p (point-max)))
        nil
      (let b (current-buffer)
        (char (b 'string) (- p 2))))))

(define-global line-info ((o buffer) (o pos))
  (withs (b (or buffer (current-buffer))
          s (b 'string)
          p (either pos (+ (b 'pos) 1))
          row 1
          i 0)
    (each line (lines:buffer-string buffer)
      (let n (+ (len line) 1)
        (when (>= (+ i n) p)
          (return (obj line row column (- p i))))
        (inc i n)
        (inc row)))
    (obj line row column (- p i))))

(define-global buffer-line ((o buffer) (o pt))
  (if (nil? buffer) (set buffer (current-buffer)))
  ((line-info buffer pt) 'line))

(define-global buffer-column ((o buffer) (o pt))
  (if (nil? buffer) (set buffer (current-buffer)))
  ((line-info buffer pt) 'column))

(define-global narrow-to-region (start end)
  (let b (current-buffer)
    (set (b 'start) (- start 1)
         (b 'end) (- end 1)
         (b 'pos) (max (b 'pos) (b 'start))
         (b 'pos) (min (b 'pos) (b 'end))))
  nil)

(define-global buffer-narrowed? ((o buffer))
  (let b (or buffer (current-buffer))
    (or (is? (b 'start)) (is? (b 'end)))))

(define-global widen ()
  (let b (current-buffer)
    (wipe (b 'start))
    (wipe (b 'end))))

(define-global set-buffer (buffer-or-name)
  (if (string? buffer-or-name)
      (assign-buffer
         (or (get-buffer buffer-or-name)
             (err (cat "No buffer named " buffer-or-name))))
      (buffer? buffer-or-name)
      (assign-buffer buffer-or-name)
      (nil? buffer-or-name)
      (assign-buffer (current-buffer))
    (err (cat "Must be a buffer or string: " (string buffer-or-name)))))

(define-global switch-to-buffer (buffer-or-name)
  (set-buffer buffer-or-name))

(def make-string (n c)
  (seval!make-string n c))

(define-global insert-char (character (o count) (o inherit))
  (withs (n (or count 1)
          b (current-buffer)
          s1 (clip (b 'string) 0 (b 'pos))
          s2 (clip (b 'string) (b 'pos))
          c (make-string n character)
          s (seval!string-append s1 c s2))
    (= (b 'string) s)
    (inc (b 'pos) n)
    (inc (b 'len) n)
    (when (buffer-narrowed? b)
      (inc (b 'end) n)))
  nil)

(define-global insert-string (c)
  (withs (b (current-buffer)
          s1 (clip (b 'string) 0 (b 'pos))
          s2 (clip (b 'string) (b 'pos))
          n (len c)
          s (seval!string-append s1 c s2))
    (= (b 'string) s)
    (inc (b 'pos) n)
    (inc (b 'len) n)
    (when (buffer-narrowed? b)
      (inc (b 'end) n)))
  nil)

(define-global delete-char-at (pt)
  (goto-char pt)
  (when (and (>= (point) (point-min))
             (< (point) (point-max)))
    (withs (b (current-buffer)
            p (- (point) 1))
      (= (b 'string)
         (cat (clip (b 'string) 0 p)
              (clip (b 'string) (+ p 1))))
      (dec (b 'len))
      (when (buffer-narrowed? b)
        (dec (b 'end)))))
  nil)

(define-global delete-char (count killp)
  (if (< count 0)
      (for-n i (- count)
        (delete-char-at (- (point) 1)))
      (> count 0)
      (for-n i count
        (delete-char-at (point) i))))

(define-global delete-region (start end)
  (goto-char start)
  (for-n i (- end start)
    (delete-char 1)))

(define-global insert args
  (let b (current-buffer)
    (step x args
      (if (string? x)
          (insert-string x)
        (insert-char x)))))

(define-macro save-current-buffer body
  (let-unique (prev)
    `(let ,prev (current-buffer)
       (after (do ,@body)
         (set-buffer ,prev)))))

(define-macro with-current-buffer (buffer-or-name . body)
  `(save-current-buffer
     (set-buffer ,buffer-or-name)
     ,@body))

(define-macro save-excursion body
  (let-unique (b pt)
    `(withs (,b (current-buffer)
             ,pt (point))
       (after (do ,@body)
         (with-current-buffer ,b
           (goto-char ,pt))))))

(define-macro save-restriction body
  (let-unique (b start end)
    `(withs (,b (current-buffer)
             ,start (,b 'start)
             ,end (,b 'end))
       (after (do ,@body)
         (= (,b 'start) ,start
            (,b 'end) ,end)))))

(define-global buffer-string ((o buffer))
  (with-current-buffer (or buffer (current-buffer))
    (let b (current-buffer)
      (clip (b 'string) (- (point-min) 1) (- (point-max) 1)))))

(define-global peek-char ((o s (current-buffer)))
  (when (< (s 'pos) (s 'len))
    (char (s 'string) (s 'pos))))

(define-global read-char ((o s (current-buffer)))
  (let c (peek-char s)
    (if c (do (inc (s 'pos)) c))))


