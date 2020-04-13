
(scm
  (define-syntax-rule (with-raw body ...)
    (let ((saved #f))
      (define (stty x) (system (~a "stty " x)) (void))
      (dynamic-wind (lambda ()
                      (set! saved (with-output-to-string (lambda () (stty "-g"))))
                      (stty "raw -echo opost"))
                    (lambda () body ...)
                    (lambda () (stty saved)))))

  (define (flush-stdin)
    (when (char-ready?)
      (begin (read-char)
             (flush-stdin))))

  (xdef flush-stdin flush-stdin)

  (define (call-with-raw f (flush-stdin? #t))
            (when flush-stdin? (flush-stdin))
            (with-raw (f)))

  (xdef call-w/raw call-with-raw))

(mac w/raw body
  `(call-w/raw (fn () ,@body)))

(def test-cinfo ()
  (w/raw
    (prn "Press a key, or not")
    (sleep 2)
    (if (seval!char-ready?)
        (seval!printf "You pressed ~a\n" (seval!read-char))
        (seval!printf "You didn't press a key\n"))))

(define-global terminal-info ((o k))
  (let (rows cols) (map int (tokens:tostring:system "stty size"))
    (aand (obj rows rows cols cols)
          (if k (it k) it))))

(define-global terminal-cols ()
  (terminal-info 'cols))

(define-global terminal-rows ()
  (terminal-info 'rows))

(define-global vp (obj x 0 y 0))

(define-global vp-x () (or (vp 'x) 0))
(define-global vp-y () (or (vp 'y) 0))
(define-global vp-w () (or (vp 'w) (terminal-cols)))
(define-global vp-h () (or (vp 'h) (terminal-rows)))

(define-global vp-scissor (x y w h)
  (set (vp 'x) x
       (vp 'y) y
       (vp 'w) w
       (vp 'h) h))

(define-global string-append args
  (apply seval!string-append args))

(define-global rpad (s n)
  (let k (- n (len s))
    (if (<= k 0) s (string-append s (make-string k #\space)))))

(define-global mode-line-format* "%b L%l:%c")

(define-global format-mode-line ((o format) (o face) (o window) (o buffer))
  (set format (either format mode-line-format*))
  (set buffer (or buffer (current-buffer)))
  (whilet i (search format "%")
    (inc i)
    (let c (char format i)
      (let s (case c
               #\b (cat (buffer-name buffer))
               #\c (cat (buffer-column buffer))
               #\f (buffer-file-name buffer)
               #\i (cat (- (point-max) (point-min)))
               #\l (cat (buffer-line buffer))
               #\n (if (buffer-narrowed? buffer) "Narrow")
               #\% "%"
               (err (cat "Unknown format mode line spec: " c)))
        (if (nil? s) (set s ""))
        (inc i)
        (set format (cat (clip format 0 (- i 2))
                         s
                         (clip format i)))
        (dec i 2)
        (inc i (len s)))))
  format)

(load "chalk.arc")

(define-global text-style-bg* 'bg-cyan-bright)
(define-global text-style-fg* 'black)

(define-global render-header (buffer n)
  (with-current-buffer (or buffer (current-buffer))
    (w/chalk text-style-bg*
      (w/chalk text-style-fg*
        (rpad (format-mode-line) n)))))

(define-global render-line (s width (o pt))
  (withs (s (rpad (clip s 0 width) width)
          p (or pt -1))
    (when (and (>= p 0) (< p (len s)))
      (set s (string-append
               (clip s 0 p)
               (w/chalk text-style-bg*
                 (w/chalk text-style-fg*
                   (clip s p (+ p 1))))
               (clip s (+ p 1)))))
    (string-append s "\n")))

(define-global buffer-lines ((o buffer) (o height))
  (let i 0
    (join (cut (each line (lines:buffer-string buffer)
                 (out line)
                 (inc i))
               0 height)
          (when (is? height)
            (n-of (max 0 (- height i)) "")))))

(define-global render-lines ((o buffer) (o height (terminal-rows)) (o width (terminal-cols)))
  (withs (l (line-info buffer)
          i 1)
    (apply string
      (step x (buffer-lines buffer height)
        (out (render-line x width (if (is (l 'line) i) (l 'column) nil)))
        (inc i)))))

(define-global render-buffer ((o buffer) (o height (terminal-rows)) (o width (terminal-cols)))
  (cat (render-lines buffer (- height 1) width)
       (if (is buffer minibuffer) ""
       (render-header buffer width))))

(define-global minibuffer (get-buffer-create " *Minibuf-1*"))

(define-global render-to-string ((o buffer) (o rows (terminal-rows)) (o cols (terminal-cols)))
  (withs (b (or buffer (current-buffer))
          b (if (is b minibuffer) ((last recursive-buffers) 'buffer) b)
          s (render-buffer b (- rows 2) cols))
    (cat! s (render-buffer minibuffer 2 cols))
    (set s (clip s 0 (edge s)))
    s))

(define-global cursor-to (line column)
  (cat "\e[" line ";" column "H"))

(define-global render ((o buffer))
  (pr:cursor-to 0 0)
  (pr:render-to-string buffer)
  (flushout)
  nil)
