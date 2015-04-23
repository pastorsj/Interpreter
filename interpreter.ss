; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
 (let ([identity-proc (lambda (x) x)])
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id) ; look up its value.
        (apply-env env
          id
	  identity-proc ; procedure to call if var is in env 
	  (lambda () ; procedure to call if var is not in env
	    (apply-env global-env  ; was init-env
		       id
		       identity-proc
		       (lambda ()
			 (error 'apply-env "variable ~s is not bound" id)))))]
      [app-exp (rands)
	       (let ([proc-value (eval-exp (car rands) env)]
		     [args (eval-rands (cdr rands) env)])
		 (apply-proc proc-value args))]
      [let-exp (vars exp bodies)
	       (let ([new-env (extend-env vars
					  (eval-rands exps env)
					  env)])
		 (eval-bodies bodies new-env))]
      [if-exp (id true false)
	      (if (eval-exp id env)
		  (eval-exp true env)
		  (eval-exp false env))]
      [if-exp-ne (id true)
		 (if (eval-exp id env)
		     (eval-exp true env))]
      [lambda-exp (id body)
		  (closure id body env)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))
 
; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) 
	   (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons =))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (+ (1st args) (2nd args))]
      [(-) (- (1st args) (2nd args))]
      [(*) (* (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))

(define global-env init-env)

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)






