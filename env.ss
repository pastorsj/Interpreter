; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
; Heavily modified by Jason Lane and Sam Pastoriza

; Environments are, in general, used to store variable definitions.

(define empty-env                                   ; make an empty environment
  (lambda ()
    (empty-env-record)))

(define extend-env                                  ; extend the environment with each sym defined as the corresponding val by building a new environment on the given one
  (lambda (syms vals env)
    (extended-env-record syms (map box vals) env))) ; use boxes which contain vals instead of mapping vals directly to make them mutable

(define extend-improper-env                         ; extend the environment with an arbitrary number of arguments ( 'lambda (x . y)' , etc.)
  (lambda (syms vals env)
    (let loop ((i (length syms)) (vals vals) (ls '()))
      (if (= i 1)
	  (extend-env syms (append ls (list vals)) env)
	  (loop (- i 1) (cdr vals) (append ls (list (car vals))))))))

(define list-find-position                          ; find the first (leftmost) object in a list that is equal to the given object
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index                                  ; find the first (leftmost) object in a list that matches the given predicate
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym succeed fail)                    ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      [empty-env-record ()                          ; if the environment is empty and we're still looking, we've failed
        (fail)]
      [extended-env-record (syms vals env)          ; if it's not empty, see if we can find what we're looking for
	      (let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	          (succeed (list-ref vals pos))
	          (apply-env env sym succeed fail)))]     ; if we can't find it, check the enclosing environment
      [recursively-extended-env-record              ; special case for letrecs
        (procnames idss bodies old-env)
        (let ((pos (list-find-position sym procnames)))
	        (if (number? pos)
			      (succeed (list-ref bodies pos))
	          (apply-env old-env sym succeed fail)))])))
