
; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014

(load "chez-init.ss") 

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "env.ss")
    (load "datatypes.ss")
    (load "parse.ss")
    (load "interpreter.ss")
    (load "continuations.ss")))

(load-all)

(define l load-all) ; even easier!

(l)

(l)