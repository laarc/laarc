(seval '(require file/glob))

(seval '(xdef cwd cwd))

(def glob (pat (o root (cwd)))
  (each path (seval!glob pat #:capture-dotfiles? #t)
    (aand (seval!path->string path)
          (if (dir-exists it) (+ it "/") it)
          (if (seval!string-prefix? it root)
              (cut it (len root))
              it)
          (out it))))

