; Abstract syntax interpreter
; By Jason Lane and Sam Pastoriza
; Class modifications by Lexi Harris, Jason Lane, and Sam Pastoriza

(define top-level-eval                                                              ; Evaluates an expression at the top level (i.e., in an empty environment)
  (lambda (form)
    (eval-exp form (empty-env))))



(define eval-exp                                                                    ; The main component of the interpreter in this version
  (lambda (exp env)
    (cases expression exp

      [define-exp (id body)                                                         ; define-exp
        (set!                                                                       ; Set the global env to an extension of itself that includes the defined var
          global-env
          (extend-env
            (list id)
            (list (eval-exp body env))
            global-env))]

      [set-exp (var val)                                                            ; set-exp
        (cases environment env                                                      ; Set the value of the variable in the most local environment
          [recursively-extended-env-record (procnames idss bodies env)              ; Special case for letrec (this shouldn't be necessary)
            (apply-env
              env
              (cadr var)
              (lambda (x) (set-box! x (eval-exp val env)))
                (lambda ()
                  (apply-env
                    global-env
                    (cadr var)
                    (lambda (x) (set-box! x (eval-exp val env)))
                      void)))]
          [else                                                                     ; General case
            (apply-env                                                              ; When you find the variable, change the value in the box
              env
              (cadr var)
              (lambda (x) (set-box! x (eval-exp val env)))
                (lambda ()
                  (apply-env
                    global-env
                    (cadr var)
                    (lambda (x) (set-box! x (eval-exp val env)))
                    void)))])]

        [cond-exp (conds bodies) (eval-exp (syntax-expand exp) env)]                ; cond-exp (re-evaluate as nexted ifs)

        [quote-exp (datum) datum]                                                   ; quote-exp (return the value)

        [when-exp (test bodies)                                                     ; when-exp
          (if (eval-exp test env)
              (eval-bodies bodies env))]

        [lit-exp (datum) datum]                                                     ; lit-exp (return the value)

        [var-exp (id)                                                               ; var-exp (look up the associated value)
          (cases environment env
            [recursively-extended-env-record (procs idss bodies env2)               ; Special case for letrec (this shouldn't be necessary)
              (apply-env
                env
                id
                (lambda (x) (eval-exp (unbox x) env))
                  (lambda ()
                    (apply-env
                      global-env
                      id
                      unbox
                      (lambda ()
                        (eopl:error 'apply-env "variable ~s is not bound" id)))))]
            [else                                                                   ; General case
              (apply-env
                env
                id
	              (lambda (x)
                  (let ((res (unbox x)))
                    (if
                      (and
                        (not (null? res))
                        (list? res)
                        (eqv? (car res) 'lambda-exp-improperls))                    ; Prevents an error in classes (this should be handled better)
                          (eval-exp res env)
                        res)))
                (lambda ()
	                (apply-env
                    global-env
		                id
		                unbox
                    (lambda ()
			                (eopl:error 'apply-env "variable ~s is not bound" id)))))])]

        [app-exp (rands)                                                            ; app-exp
	        (let ([proc-value (eval-exp (car rands) env)]                             ; Apply the evaluation of the first argument to the rest
		        [args (cdr rands)])
            (cases proc-val proc-value
              [clos-proc (vars body env2) (apply-proc proc-value args env)]         ; Clos-proc case (used to handle refs)
              [else (apply-proc proc-value (eval-rands args env) env)]))]           ; Normal case

	      [letrec-exp (procnames idss body letrec-body)                               ; letrec-exp
		      (eval-bodies letrec-body
			      (extend-env-recursively procnames idss body env))]

        [member-exp (item list)                                                     ; member-exp
          (member (eval-exp item env) (map (lambda (x) (eval-exp x env)) list))]

        [if-exp (id true false)                                                     ; if-exp
	        (if (eval-exp id env)
		        (eval-exp true env)
		        (eval-exp false env))]

        [if-exp-ne (id true)                                                        ; if-exp-ne
		      (if (eval-exp id env)
		        (eval-exp true env))]

        [lambda-exp (id body)                                                       ; lambda-exp
		      (clos-proc id body env)]

        [case-lambda-exp (idss lens bodies)                                         ; case-lambda-exp
          (case-clos-proc idss lens bodies env)]

	[lambda-exp-improperls (reqs non-req body)                                        ; lambda-exp-improperls
			       (clos-improc (append reqs non-req) body env)]

	[lambda-exp-nolimit (id body)                                                     ; lambda-exp-nolimit
			    (clos-improc (list id) body env)]

	[while-exp (test body)                                                            ; while-exp
		   (if (eval-exp test env)
		       (eval-exp (app-exp `((lambda-exp ()
					      ((app-exp ((lambda-exp ()  ,body)))
					      (while-exp ,test ,body))))) env))]

  [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))



(define apply-proc                                                                  ; Apply a procedure to its arguments
  (lambda (proc-value args env2)                                                    ; An env is needed for ref cases
    (cases proc-val proc-value

      [prim-proc (op) (apply-prim-proc op args env2)]                               ; prim-proc case

      [clos-proc (vars body env) (if (ormap list? vars)                             ; clos-proc case (lambda-exp)
                                  (eval-bodies                                      ; refs case
                                    (cadr (replace-refs vars body args))
                                    (extend-env
                                      (car (replace-refs vars body args))
                                      (remove-refs vars (if ((list-of expression?) args) (eval-rands args env2) args))
                                      (if (equal? (cadr (replace-refs vars body args)) body) env env2)))
                                  (eval-bodies                                      ; no-ref case
                                    body
                                    (extend-env vars
                                      (if ((list-of expression?) args) (eval-rands args env2) args)
                                      env)))]

      [case-clos-proc (idss lens bodies env)                                        ; case-proc case (case-lambda-exp)
        (let ((pos (list-find-position (length args) lens)))
          (eval-bodies (list-ref bodies pos) (extend-env (list-ref idss pos) args env)))]

      [clos-improc (vars body env)                                                  ; clos-improc case (lambda-exp-improperls, lambda-exp-nolimit)
        (eval-bodies body (extend-improper-env vars args env))]

      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s"
                    proc-value)])))



(define replace-refs                                                                ; Replaces (ref-exp)s with the correct arg
  (lambda (vars body args)
    (cond [(null? vars) (list vars body)]
          [(symbol? (car vars))
            (let ((res (replace-refs (cdr vars) body (cdr args))))
              (list (cons (car vars) (car res)) (cadr res)))]
          [else
            (let ((res (replace-refs (cdr vars) body (cdr args))))
              (list (car res) (replace-help (car vars) (cadr res) (car args))))])))

(define replace-help                                                                ; Helper proc for replace-refs
  (lambda (var exps arg)
    (cond [(null? exps) exps]
          [(expression? exps)
            (cases expression exps
              [ref-exp (id) (if (eqv? id (cadr var)) arg exps)]
              [lit-exp (id) exps]
              [set-exp (id body) (set-exp (replace-help var id arg) (replace-help var body arg))]
              [app-exp (rands) (app-exp (replace-help var rands arg))]
              [lambda-exp (id body) (lambda-exp id (replace-help var body arg))]
              [else exps])]
          [(list-of expression?) (cons (replace-help var (car exps) arg) (replace-help var (cdr exps) arg))])))

(define remove-refs                                                                 ; Filters out (ref-exp)s
  (lambda (vars args)
    (cond [(null? vars) args]
          [(symbol? (car vars)) (cons (car args) (remove-refs (cdr vars) (cdr args)))]
          [else (remove-refs (cdr vars) (cdr args))])))

(define eval-rands                                                                  ; Evaluate a list of bodies and return a list of results
  (lambda (rands env)
    (map (lambda (x)
	   (eval-exp x env)) rands)))

(define eval-bodies                                                                 ; Evaluate a list of bodies and return the final result
  (lambda (bodies env)
    (if (null? (cdr bodies))
      (eval-exp (car bodies) env)
      (begin
        (eval-exp (car bodies) env)
        (eval-bodies (cdr bodies) env)))))

(define *prim-proc-names*                                                           ; List of prim-procs (used to create the initial environment)
  '(+ - * / quotient add1 sub1 zero? not = > >= < <= car cdr list null? assq
    eq? equal? eqv? atom? cons length list->vector list? pair? procedure?
    vector->list vector make-vector vector-ref vector? number? symbol? set-car!
    set-cdr! vector-set! display newline caar cadr cdar cddr caaar caadr cadar
    caddr cdaar cdadr cddar cdddr apply map append list-tail))

(define init-env                                                                    ; Creates the initial environment by adding prim-procs to an empty env
  (lambda ()             
  (extend-env            
     *prim-proc-names*   
     (map prim-proc
          *prim-proc-names*)
     (empty-env))))

(define apply-prim-proc                                                             ; Definitions for each of the previously named prim-procs
  (lambda (prim-proc args env2)
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
      [(append) (append (1st args) (2nd args))]
      [(null?) (null? (1st args))]
      [(assq) (assq (1st args) (2nd args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(eqv?) (eqv? (1st args) (2nd args))]
      [(atom?) (atom? args)]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(list-tail) (list-tail (1st args) (2nd args))]
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
      [(apply) (apply-proc (1st args) (2nd args) env2)]
      [(map) (map (lambda (x) (apply-proc (1st args) x env2)) (matrix-transpose-map (cdr args)))]
      [else (error 'apply-prim-proc
            "Bad primitive procedure name: ~s"
            prim-op)])))


; The following seven procedures are used in syntax-expanding class-exp
; The first five are nearly identical and are simply filter procs

(define filter-static-fields
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [else (cases classvar (car ls)
          [public-static-var (pred name val) (cons (car ls) (filter-static-fields (cdr ls)))]
          [private-static-var (pred name val) (cons (car ls) (filter-static-fields (cdr ls)))]
          [else (filter-static-fields (cdr ls))])])))

(define filter-static-methods
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [else (cases method (car ls)
          [public-static-method (name args body) (cons (car ls) (filter-static-methods (cdr ls)))]
          [private-static-method (name args body) (cons (car ls) (filter-static-methods (cdr ls)))]
          [else (filter-static-methods (cdr ls))])])))

(define filter-normal-fields
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [else (cases classvar (car ls)
          [public-var (pred name val) (cons (car ls) (filter-normal-fields (cdr ls)))]
          [private-var (pred name val) (cons (car ls) (filter-normal-fields (cdr ls)))]
          [else (filter-normal-fields (cdr ls))])])))

(define filter-normal-methods
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [else (cases method (car ls)
          [public-method (name args body) (cons (car ls) (filter-normal-methods (cdr ls)))]
          [private-method (name args body) (cons (car ls) (filter-normal-methods (cdr ls)))]
          [else (filter-normal-methods (cdr ls))])])))

(define filter-public-fields
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [else (cases classvar (car ls)
          [public-var (pred name val) (cons (car ls) (filter-public-fields (cdr ls)))]
          [private-var (pred name val) (cons (car ls) (filter-public-fields (cdr ls)))]
          [else (filter-public-fields (cdr ls))])])))

(define make-constr                                                                 ; makes the default constructor method
  (lambda (fields methods)                                                          ; at this time, constructors do not accept arguments
    (let ((fields fields) (methods methods))                                        ; the constructor method actually returns the generated cases for
      (lambda-exp (list 'args)                                                      ; non-static methods in an environment which contains the fields
        (list (letrec-exp
                (cons 'this (map (lambda (x) (caddr x)) fields))
                '((msg . args))
                (cons (make-constr-help fields methods) (map (lambda (x) (parse-exp (cadddr x))) fields))
                  (list (se 
                    (let-exp '(fields) (map parse-exp (map caddr fields))           ; used to accept arguments; down't work because of environment depth
                      (list (se (parse-exp '(if (not (null? args))                  ; it may work if refs are used here
                        (let loop ((fields fields) (args args))
                          (if (not (or (null? (car args) (null? args))))
                            (set! (car fields) (car args))
                            (loop (cdr fields) (cdr args))))))) (var-exp 'this)))))))))))

(define make-constr-help                                                            ; helper for make-constr
  (lambda (fields methods)
    (let ((res (filter-public-fields fields)))
      (lambda-exp-improperls (list 'msg) (list 'args)
        (list (se (case-exp
          (var-exp 'msg)
          (map (lambda (x) (app-exp (list x))) (append (map quote-exp (map cadr methods)) (map (lambda (x) (quote-exp (caddr x))) res)))
          (append (map cadddr methods) (map (lambda (x) (parse-exp (caddr x))) res)))))))))



(define syntax-expand                                                               ; Expand an expression into an equivalent expression
  (lambda (exp)
    (let exp-recur ((exp exp))
      (cases expression exp
        [let-exp (vars vals body)                                                    ; let->lambda
		      (app-exp (append (list (lambda-exp vars (parse-refs (find-ref vars) (map syntax-expand body)))) vals))]

        [define-exp (var val) (define-exp var (se val))]                             ; define (expand second arg)

        [class-exp (fields methods)                                                  ; class
          (let* ((res (filter-static-fields fields)) (res2 (filter-public-fields res)))
            (let-exp                                                                 ; let used for static fields
              (append (map caddr res) (list 'make))
              (append (if (null? res) '() (map parse-exp (map cadddr res))) (list (make-constr fields methods)))
              (list (lambda-exp-improperls (list 'msg) (list 'args)                  ; lambda used for static methods
                (list (se (case-exp                                                  ; generates a case-exp for methods
                      (var-exp 'msg)
                      (map (lambda (x) (app-exp (list (quote-exp x)))) (cons 'make (append (map cadr (filter-static-methods methods)) (map caddr res))))
                      (cons (parse-exp '(make args)) (append (map cadddr (filter-static-methods methods)) (map parse-exp (map caddr res)))))))))))]

        [let*-exp (vars vals body)
		      (syntax-expand (let-exp (list (car vars)) (list (car vals))
					  (list (if (not (null? (cddr vals)))
					 	  (let*-exp (cdr vars) (cdr vals) body)
						  (let-exp (cdr vars) (cdr vals) body)))))]

        [named-let (name vars vals body)
          (letrec-exp (cons name vars) (cons vars (find-idss vals '())) (cons (lambda-exp vars body) vals) (list (app-exp (cons (parse-exp name) vals))))]

        [begin-exp (body)
		      (app-exp (list (lambda-exp '() (map syntax-expand body))))]

        [cond-exp (conditions bodies)
          (cond
            [(equal? (var-exp 'else) (car conditions)) (syntax-expand (car bodies))]
            [(null? (cdr conditions)) (if-exp-ne (syntax-expand (car conditions)) (syntax-expand (car bodies)))]
            [else (if-exp (syntax-expand (car conditions)) (syntax-expand (car bodies)) (syntax-expand (cond-exp (cdr conditions) (cdr bodies))))])]

        [and-exp (body)
          (if (null? body) (lit-exp #t)
            (if-exp (car body) (syntax-expand (and-exp (cdr body))) (car body)))]

        [or-exp (body)
          (if (null? body) (lit-exp #f)
          (if (null? (cdr body))
            (if-exp (syntax-expand (car body))
                    (syntax-expand (car body))
                    (lit-exp '#f))
            (syntax-expand
              (let-exp '(car-body)
                        (list (syntax-expand (car body)))
                        (list (if-exp '(var-exp car-body)
                                      '(var-exp car-body)
                                       (syntax-expand (or-exp (cdr body)))))))))]

        [if-exp (id true false)
	        (if-exp (syntax-expand id)
		        (syntax-expand true)
		        (syntax-expand false))]

        [if-exp-ne (id true)
	        (if-exp-ne (syntax-expand id)
		        (syntax-expand true))]

        [case-exp (id conditions bodies)
		      (cond
		        [(null? (cdr conditions)) (se (if-exp-ne (member-exp id (cadar conditions)) (car bodies)))]
		        [(equal? (var-exp 'else) (cadr conditions)) (se (if-exp (member-exp id (cadar conditions)) (car bodies) (cadr bodies)))]
		        [else (se (if-exp (member-exp id (cadar conditions)) (car bodies) (case-exp id (cdr conditions) (cdr bodies))))])]

        [else exp]))))



(define se syntax-expand)

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

(define global-env (init-env))

(define reset-global-env
  (lambda ()
    (set! global-env (init-env))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((res (clean-vars proc-names bodies '(() ()))))
      (recursively-extended-env-record
        (car res) idss (map box (cadr res)) old-env))))

(define clean-vars
  (lambda (idss bodies ls)
    (cond [(null? idss) (cons (reverse (car ls)) (list (reverse (cadr ls))))]
          [(equal? (parse-exp (car idss)) (car bodies)) (clean-vars (cdr idss) (cdr bodies) ls)]
          [else (clean-vars (cdr idss) (cdr bodies) (list (cons (car idss) (car ls)) (cons (car bodies) (cadr ls))))])))

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

;Extra helper procedures for our interpreter...
(define matrix-transpose
  (lambda (m)
    (if (null? (car m))
	'()
	(cons (get-firsts m) (list (get-new-matrix m))))))

  (define matrix-transpose-map
    (lambda (m)
      (if (null? (car m))
  	'()
  	(cons (get-firsts m) (matrix-transpose-map (get-new-matrix m))))))

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