(mac for-n (var n . body)
  `(for ,var 0 (- ,n 1)
     ,@body))

(mac %function (args . body)
  `(fn ,args (%point return ,@body)))

(mac define-global (name . body)
  (case (len body)
    0 `(or= ,name nil)
    1 `(or= ,name ,(car body))
    (let (args . body) body
      `(def ,name ,args (%point return ,@body)))))

(mac define-macro (name args . body)
  `(mac ,name ,args (%point return ,@body)))

(mac let-unique (args . body)
  `(w/uniq ,args
     ,@body))

(mac step (v h . body)
  `(each ,v ,h
     ,@body))

(mac cat! (var . xs)
  `(= ,var (cat ,var ,@xs)))

(mac set-of args
  `(%object ,@(each x args
                (out x)
                (out 't))))

(mac add (l x)
  `(do (push ,x ,l) nil))

(mac drop (l)
  `(pop ,l))

(mac inc (x (o n 1))
  `(++ ,x ,n))

(mac dec (x (o n 1))
  `(-- ,x ,n))

(def %object args
  (listtab:tuples args))

(def nil? (x) (is x nil))

(def is? (x) (~nil? x))

(def has? (h k)
  (let unset (list nil)
    (~is (h k unset) unset)))

(def string? (x) (isa x 'string))
(def number? (x) (in (type x) 'int 'num))

(def obj? (x) (isa x 'table))

(def either (a b)
  (or a b))

(def char (s (o i 0))
  (errsafe:s i))

(def clip (s (o from) (o upto))
  (withs (n (len s)
          from (or from 0)
          upto (or upto n))
    (if (< from 0) (= from 0))
    (if (> from n) (= from n))
    (if (< upto 0) (= upto 0))
    (if (> upto n) (= upto n))
    (seval!substring s from upto)))

(def search (str x (o from 0))
  (posmatch x str from))

(def edge (x) (- (len x) 1))
