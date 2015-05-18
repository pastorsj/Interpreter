; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form k)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
  (let ([identity-proc (lambda (x) x)])
    (lambda (exp env)
      (cases expression exp
        [define-exp (id body) (set! global-env (extend-env (list id) (list (eval-exp body env)) global-env))]
        [set-exp (var val) (apply-env env (cadr var)
                              (lambda (x) (set-box! x (eval-exp val env)))
                              (lambda ()
                                (apply-env global-env (cadr var)
                                  (lambda (x) (set-box! x (eval-exp val env)))
                              void)))]
        [cond-exp (conds bodies) (eval-exp (syntax-expand exp) env)]
        [quote-exp (datum) datum]
        [when-exp (test bodies) (if (eval-exp test env) (eval-bodies bodies env))]
        [lit-exp (datum) datum]
        [var-exp (id) ; look up its value.
          (apply-env env
            id
	          ;identity-proc ; procedure to call if var is in env
	          unbox
            (lambda () ; procedure to call if var is not in env
	            (apply-env global-env  ; was init-env
		            id
		            ;identity-proc
		            unbox
                  (lambda ()
			              (eopl:error 'apply-env "variable ~s is not bound" id)))))]
        [app-exp (rands)
	        (let ([proc-value (eval-exp (car rands) env)]
		        [args (cdr rands)])
            (cases proc-val proc-value
              [clos-proc (vars body env2) (apply-proc proc-value args env)]
              [else (apply-proc proc-value (eval-rands args env) env)]))]
        [let-exp (vars vals body)
	        (let ([new-env (extend-env vars
					  (eval-rands vals env)
					  env)])
		        (eval-bodies body new-env))]
	[letrec-exp (procnames idss body letrec-body)
		    (eval-bodies letrec-body
			      (extend-env-recursively
			       procnames idss body env (init-k)))]
        [member-exp (item list)
          (member (eval-exp item env) (map (lambda (x) (eval-exp x env)) list))]
        [if-exp (id true false)
	        (if (eval-exp id env)
		      (eval-exp true env)
		      (eval-exp false env))]
        [if-exp-ne (id true)
		      (if (eval-exp id env)
		      (eval-exp true env))]
        [lambda-exp (id body)
		    (clos-proc id body env)]
        [case-lambda-exp (idss lens bodies)
          (case-clos-proc idss lens bodies env)]
	[lambda-exp-improperls (reqs non-req body)
			       (clos-improc (append reqs non-req) body env)]
	[lambda-exp-nolimit (id body)
			    (clos-improc (list id) body env)]
	[while-exp (test body)
		   (if (eval-exp test env)
		       (eval-exp (app-exp `((lambda-exp ()
					      ((app-exp ((lambda-exp ()  ,body)))
					      (while-exp ,test ,body))))) env))]
        [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

(define replace-refs
  (lambda (vars body args)
    (let ((res (replace-refs-cps vars body args (init-k)))
          (res2
        (if (null? vars)
            (list vars body)
            (let ((res (replace-refs (cdr vars) body (cdr args))))
              (if (symbol? (car vars))
                  (list (cons (car vars) (car res)) (cadr res))
                  (list (car res) (replace-help (car vars) (cadr res) (car args) (init-k))))))))
      res)))
    ;(if (null? vars)
    ;    (list vars body)
    ;    (let ((res (replace-refs (cdr vars) body (cdr args))))
    ;      (if (symbol? (car vars))
    ;          (list (cons (car vars) (car res)) (cadr res)))]
    ;          (list (car res) (replace-help (car vars) (cadr res) (car args) (init-k))))])))

(define replace-refs-cps
  (lambda (vars body args k)
    (if (null? vars)
        (apply-k k (list vars body))
        (replace-refs-cps (cdr vars) body (cdr args) (replace-refs-k vars args k)))))

(define replace-help ;;Done in cps
  (lambda (var exps arg k)
    (cond [(null? exps) (apply-k k exps)]
          [(expression? exps)
            (cases expression exps
              [ref-exp (id) (apply-k k (if (eqv? id (cadr var)) arg exps))]
              [lit-exp (id) (apply-k k exps)]
              [set-exp (id body) (replace-help var id arg (set-replace-body-k var body arg k))]
              [app-exp (rands) (app-exp (replace-help var rands arg k))]
              [lambda-exp (id body) (lambda-exp id (replace-help var body arg k))]
              [else (apply-k k exps)])]
          [(list-of expression?) (cons (replace-help var (car exps) arg k) (replace-help var (cdr exps) arg k))])))

(define eval-rands-ref ;;Done in cps
  (lambda (vars args k)
    (cond [(null? vars) (apply-k k args)]
          [(symbol? (car vars)) (cons (car args) (eval-rands-ref (cdr vars) (cdr args) k))]
          [else (eval-rands-ref (cdr vars) (cdr args) k)])))

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
  (lambda (proc-value args env2)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args env2)]
      [clos-proc (vars body env) (eval-bodies
                                    (cadr (replace-refs vars body args))
                                    (extend-env
                                      (car (replace-refs vars body args))
                                      (eval-rands-ref vars (if ((list-of expression?) args) (eval-rands args env2) args) (init-k))
                                      (if (equal? (cadr (replace-refs vars body args)) body) env env2)))]
      [case-clos-proc (idss lens bodies env) (let ((pos (list-find-position (length args) lens)))
                                                (eval-bodies (list-ref bodies pos) (extend-env (list-ref idss pos) args env)))]
      [clos-improc (vars body env) (eval-bodies body (extend-improper-env vars args env))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s"
                    proc-value)])))

(define *prim-proc-names* '(+ - * / quotient add1 sub1 zero? not = > >= < <= car cdr list null? assq eq? equal? eqv? atom? cons length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
	vector-set! display newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr apply map append list-tail))

(define init-env
  (lambda ()              ; for now, our initial global environment only contains
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc
          *prim-proc-names*)
     (empty-env))))

; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
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
      [(map) (map (lambda (x) (apply-proc (1st args) x env2)) (matrix-transpose2 (cdr args) (init-k)))]
      [else (error 'apply-prim-proc
            "Bad primitive procedure name: ~s"
            prim-op)])))

;syntax-expand procedure

(define syntax-expand
  (lambda (exp)
    (let exp-recur ((exp exp))
      (cases expression exp
       [let-exp (vars vals body)
		(app-exp (append (list (lambda-exp vars (parse-refs (find-ref vars) (map syntax-expand body)))) vals))]
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
            (let-exp '(car-body) (list (syntax-expand (car body)))
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

(define eval-one-exp ;;Done in cps
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)) (init-k))))
(define global-env (init-env)) ;;Done in cps

(define reset-global-env ;;Done in cps
  (lambda ()
    (set! global-env (init-env))))

(define extend-env-recursively ;;Done in cps
  (lambda (proc-names idss bodies old-env k)
    (clean-vars proc-names bodies '(() ()) (recursive-extend-k idss old-env k))))


(define clean-vars ;;Done in cps
  (lambda (idss bodies ls k)
    (cond [(null? idss) (apply-k k (cons (reverse (car ls)) (list (reverse (cadr ls)))))]
          [(equal? (var-exp (car idss)) (car bodies)) (clean-vars (cdr idss) (cdr bodies) ls k)]
          [else (clean-vars (cdr idss) (cdr bodies) (list (cons (car idss) (car ls)) (cons (car bodies) (cadr ls))) k)])))

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

;Extra helper procedures for our interpreter...

(define transpose-recurse ;;Done in cps
  (lambda (m1 m2 k)
    (if (or (null? m1) (null? (car m1)))
      (apply-k k m2)
      (transpose-recurse (map cdr m1) (append m2 (list (map car m1))) k))))

;;; #5
(define matrix-transpose2 ;;Done in cps
  (lambda (m k)
    (transpose-recurse (map cdr m) (list (map car m)) k)))
