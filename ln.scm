; racket -f as.scm
; (asv)
; http://localhost:8080

(require mzscheme) ; promise we won't redefine mzscheme bindings

(require "ac.scm") 
(require "brackets.scm")
(use-bracket-readtable)

(aload "arc.arc")
(aload "libs.arc") 

(aload "news.arc")

(arc-eval
  '(do (def readenv (name (o default))
         (aif (get-environment-variable name)
              (errsafe:read it)
            default))
       (prn "srv-port* " (= srv-port* (readenv "PORT" 8080)))
       (prn "srv-noisy* " (= srv-noisy* (readenv "NOISY" nil)))
       (prn "caching* " (= caching* (readenv "CACHING" 1)))
       (prn "explicit-flush " (declare 'explicit-flush (readenv "FLUSH" t)))
       (flushout)
       (nsv srv-port*)))


