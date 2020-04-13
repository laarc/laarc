
(or= processes* ())

(seval
  '(define (make-subprocess . args)
     (define-values (proc o i e) (apply subprocess args))
     (list proc o i e)))

(def create-subprocess args
  (apply seval!make-subprocess #f #f 'stdout args))

(def unique-name (name f (o i))
  (let s (if i (cat name "<" i ">") name)
    (if (f s) (unique-name name f (+ (or i 0) 1)) s)))

(def get-process (name)
  (catch:each p processes*
    (when (is name (p 'name))
      (throw p))))

(def process-list ()
  processes*)

(def process-command (process)
  (process 'command))

(def process-id (process)
  (seval!subprocess-pid (process 'process)))

(def process-status (process)
  (seval!subprocess-status (process 'process)))

(def process-live-p (process)
  (is (process-status process) 'running))

(def start-process (name buffer-or-name program . args)
  (let name (unique-name name get-process)
    (let (proc o i e) (apply create-subprocess program args)
      (let h (obj name name process proc stdout o stdin i stderr e
                  command (cons program args)
                  buffer (get-buffer-create buffer-or-name))
        (add processes* h)
        h))))


(edop buffers ()
  (sptab
    (each x buffers*
      (row (bufferline x))))
  (sptab
    (row "name" "command" "pid" "status" "buffer")
    (each p (process-list)
      (row p!name process-command.p process-id.p process-status.p (bufferline p!buffer))))
  (onlink 'run
          (urform user req
            (do (apply start-process (arg req "name") (arg req "buffer")
                       (map string (readvar 'sexpr (arg req "command"))))
              "/buffers")
            (tab:editvars
              `((string1 name)
                (string1 buffer)
                (sexpr command)))
            (br2)
            (submit "create"))))

(def pretext (text)
  (pr "<pre><code>" (eschtml text) "</code></pre>"))

(def buffer-status ((o buffer))
  (with-current-buffer buffer (format-mode-line)))

(def bufferline ((o buffer) (o user (get-user)))
  (onlink (pretext:buffer-status buffer)
          (pretext:buffer-string buffer)))
