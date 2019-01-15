
; is there a better way to prevent ac.scm from printing each
; statement?
(define default-print (current-print))
(current-print (lambda args #f))

(require "ac.scm") 
(require "brackets.scm")
(use-bracket-readtable)

(aload "arc.arc")
(aload "libs.arc") 

(current-print default-print)

