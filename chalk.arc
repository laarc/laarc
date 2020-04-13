
(or= chalk-styles* (obj))

(= (chalk-styles* 'bg-cyan-bright)
   (list "\u001b[106m" "\u001b[49m")
   (chalk-styles* 'black)
   (list "\u001b[30m" "\u001b[39m"))

(def chalk-fn (style)
  (fn (f)
    (let (a b) (chalk-styles* style)
      (cat a (f) b))))

(or= chalk* (obj))

(= (chalk* 'bg-cyan-bright) (chalk-fn 'bg-cyan-bright)
   (chalk* 'black) (chalk-fn 'black))

(mac w/chalk (style . body)
  `((chalk* ,style)
    (fn () ,@body)))
