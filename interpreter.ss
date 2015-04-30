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
        [quote-exp (datum) datum]
        [when-exp (test bodies) (if (eval-exp test env) (eval-bodies bodies env))]
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
        [let-exp (vars vals body)
	        (let ([new-env (extend-env vars
					  (eval-rands vals env)
					  env)])
		        (eval-bodies body new-env))]
        [if-exp (id true false)
	        (if (eval-exp id env)
		      (eval-exp true env)
		      (eval-exp false env))]
        [if-exp-ne (id true)
		      (if (eval-exp id env)
		      (eval-exp true env))]
        [lambda-exp (id body)
		    (clos-proc id body env)]
	[lambda-exp-improperls (reqs non-req body)
			       (clos-improc (append reqs non-req) body env)]
	[lambda-exp-nolimit (id body)
			    (clos-improc (list id) body env)]
	[while-exp (test body)
		   (if (eval-exp test env) 
		       (eval-exp (app-exp '((lambda-exp () 
					      (app-exp ((lambda-exp () body))) 
					      (while-exp test body)))) env))]
        [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))
 
; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) 
	   (eval-exp x env)) rands)))

; evaluate a list of procedures, returning the last one (used for lets)

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
      (eval-exp (car bodies) env)
      (begin
        (eval-exp (car bodies) env)
        (eval-bodies (cdr bodies) env)))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [clos-proc (vars body env) (eval-bodies body (extend-env vars args env))]
      [clos-improc (vars body env) (eval-bodies body (extend-improper-env vars args env))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / quotient add1 sub1 zero? not = > >= < <= car cdr list null? assq eq? equal? eqv? atom? cons length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
	vector-set! display newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr apply map))

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
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(quotient) (apply quotient args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(zero?) (= (1st args) 0)]
      [(not) (not (1st args))]
      [(=) (= (1st args) (2nd args))]
      [(>) (apply > args)]
      [(>=) (apply >= args)]
      [(<) (apply < args)]
      [(<=) (apply <= args)]
      [(quote) (quote  ((lambda (x) x) (1st args)))]
      [(cons) (apply cons args)]
      [(car) (car (1st args))]
      [(cdr) (if (null? (1st args)) (eopl:error 'apply-prim-proc "cannot take cdr of ~s" (1st args)) (cdr (1st args)))]
      [(list) args]
      [(null?) (null? (1st args))]
      [(assq) (assq (1st args) (2nd args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(eqv?) (eqv? (1st args) (2nd args))]
      [(atom?) (atom? args)]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (proc-val? (1st args))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (list->vector args)]
      [(make-vector) (cond
      					[(= 1 (length args)) (make-vector (1st args) 0)]
      					[(= 2 (length args)) (make-vector (1st args) (2nd args))]
      					[else (eopl:error 'apply-prim-proc "Incorrect number of arguments to make-vector" args)])]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (apply vector-set! args)]
      [(display) (display args)]
      [(newline) (newline)]
      [(caar) (car (car (1st args)))]
      [(cadr) (car (cdr (1st args)))]
      [(cdar) (cdr (car (1st args)))]
      [(cddr) (cdr (cdr (1st args)))]
      [(caaar) (car (car (car (1st args))))]
      [(caadr) (car (car (cdr (1st args))))]
      [(cadar) (car (cdr (car (1st args))))]
      [(caddr) (car (cdr (cdr (1st args))))]
      [(cdddr) (cdr (cdr (cdr (1st args))))]
      [(cddar) (cdr (cdr (car (1st args))))]
      [(cdaar) (cdr (car (car (1st args))))]
      [(cdadr) (cdr (car (cdr (1st args))))]
      [(apply) (apply-proc (1st args) (2nd args))]
      [(map) (map (lambda (x) (apply-proc (1st args) x)) (matrix-transpose (cdr args)))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

;syntax-expand procedure

(define syntax-expand
  (lambda (exp)
    (let exp-recur ((exp exp))
      (cases expression exp
       [let-exp (vars vals body)
		(app-exp (append (list (lambda-exp vars (map syntax-expand body))) vals))]
       [let*-exp (vars vals body)
		 (syntax-expand (let-exp (list (car vars)) (list (car vals)) 
					 (list (if (not (null? (cddr vals)))
						   (let*-exp (cdr vars) (cdr vals) body)
						   (let-exp (cdr vars) (cdr vals) body)))))]
       [begin-exp (body)
		  (app-exp (list (lambda-exp '() (map syntax-expand body))))]
       [cond-exp (conditions bodies)
        (cond
          [(null? (cdr conditions)) (if-exp-ne (car conditions) (car bodies))]
          [(equal? (var-exp 'else) (cadr conditions)) (if-exp (car conditions) (car bodies) (cadr bodies))]
          [else (if-exp (car conditions) (car bodies) (syntax-expand (cond-exp (cdr conditions) (cdr bodies))))])]
       [else exp]))))


(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))

(define global-env init-env)

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

;Extra helper procedures for our interpreter...
(define matrix-transpose
  (lambda (m)
    (if (null? (car m))
	'()
	(cons (get-firsts m) (matrix-transpose (get-new-matrix m))))))

(define get-firsts 
  (lambda (m)
    (if (null? m)
	'()
	(cons (caar m) (get-firsts (cdr m))))))

(define get-new-matrix
  (lambda (m)
    (if (null? m)
	'()
	(cons (cdr (car m)) (get-new-matrix (cdr m))))))

;(define let-exp->lambda-exp
;  (lambda (expression)
;    (app-exp (list (lambda-exp (id body




