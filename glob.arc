
(seval '(require file/glob))

(def glob (pat)
  (each path (seval!glob pat #:capture-dotfiles? #t)
    (let path (seval!path->string path)
      (if (dir-exists path)
          (out (+ path "/"))
          (out path)))))
