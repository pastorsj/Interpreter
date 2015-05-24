; top-level-eval evaluates a form in the global environment

(define top-level-eval ;;Done in cps
  (lambda (form k)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env) k)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (let ([identity-proc (lambda (x) x)])
    (lambda (exp env k)
      (cases expression exp
        [define-exp (id body) (eval-exp body env (extend-help-define-k id global-env k))]
        [set-exp (var val) (apply-env env (cadr var)
                              (lambda (x k) (eval-exp val env (set-k x k)))
                              (lambda (k1)
                                (apply-env global-env (cadr var)
                                  (lambda (x k) (eval-exp val env (set-k x k)))
                                (lambda (k1) (eopl:error 'apply-env "variable ~s is not bound" var)) k)) k)]
        [cond-exp (conds bodies) (eval-exp (syntax-expand exp) env k)]
        [quote-exp (datum) (apply-k k datum)]
        [when-exp (test bodies) (if (eval-exp test env k) (eval-bodies bodies env k))]
        [lit-exp (datum) (apply-k k datum)]
        [var-exp (id) ; look up its value.
          (apply-env env
            id
	          (lambda (v k)
              (apply-k k (unbox v)))
            (lambda (k1) ; procedure to call if var is not in env
	            (apply-env global-env  ; was init-env
		            id
                (lambda (v k)
                  (apply-k k (unbox v)))
                  (lambda (k1)
			              (eopl:error 'apply-env "variable ~s is not bound" id))
                    k)) k)]
        [app-exp (rands)
          (eval-exp (car rands) env (app-k (cdr rands) env k))]
	[letrec-exp (procnames idss body letrec-body)
			      (extend-env-recursively
			       procnames idss body env (letrec-k letrec-body k))]
        [member-exp (item list)
          (eval-exp item env (member-k list env k))]
        [if-exp (id true false)
	        (eval-exp id env (if-k env (list true false) k))]
        [if-exp-ne (id true)
		      (eval-exp id env (if-k env (list true) k))]
        [lambda-exp (id body)
		      (apply-k k (clos-proc id body env))]
        [case-lambda-exp (idss lens bodies)
          (case-clos-proc idss lens bodies env)]
	[lambda-exp-improperls (reqs non-req body)
			       (apply-k k (clos-improc (append reqs non-req) body env))]
	[lambda-exp-nolimit (id body)
			    (apply-k k (clos-improc (list id) body env))]
	[while-exp (test body)
      (eval-exp test env (while-if-k body test env k))]
        [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

(define replace-refs-cps ;;;Done in cps
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
              [app-exp (rands) (replace-help var rands arg (app-ref-k k))]
              [lambda-exp (id body) (replace-help var body arg (lambda-ref-k id k))]
              [else (apply-k k exps)])]
          [((list-of expression?) exps) (replace-help var (cdr exps) arg (replace-help-k var (car exps) arg k))])))

(define eval-rands-ref ;;Done in cps
  (lambda (vars args k)
    (cond [(null? vars) (apply-k k args)]
          [(symbol? (car vars)) (eval-rands-ref (cdr vars) (cdr args) (rands-ref-k (car args) k))]
          [else (eval-rands-ref (cdr vars) (cdr args) k)])))

; evaluate the list of operands, putting results into a list

(define eval-rands ;;Done in cps
  (lambda (rands env k)
    (if (null? rands) (apply-k k rands)
    (eval-exp (car rands) env (eval-rands-k '() (cdr rands) env k)))))

; evaluate a list of procedures, returning the last one (used for letrecs, lambdas, and procedures that expand to lambdas)

(define eval-bodies
  (lambda (bodies env k)
    (if (null? (cdr bodies))
      (eval-exp (car bodies) env k)
      (begin
        (eval-exp (car bodies) env (bodies-k (cdr bodies) env k))))))

;  Apply a procedure to its arguments.

(define apply-proc
  (lambda (proc-value args env2 k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args env2 k)]
      [clos-proc (vars body env)
        (if (ormap list? vars)
          (replace-refs-cps vars body args (clos-ref-k vars args env env2 body k))
          (if ((list-of expression?) args)
            (eval-rands args env2 (clos-extend-k vars env body k))
            (extend-env vars args env (bodies-env-k body k))))]
      [case-clos-proc (idss lens bodies env) (let ((pos (list-find-position (length args) lens)))
                                                (eval-bodies (list-ref bodies pos) (extend-env (list-ref idss pos) args env) k))]
      [clos-improc (vars body env) (extend-improper-env vars args env (bodies-env-k body k))]
      [continuation-proc (k)
        (apply-k k (car args))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s"
                    proc-value)])))

(define *prim-proc-names* '(+ - * / quotient add1 sub1 zero? not = > >= < <= car cdr list null? assq eq? equal? eqv? atom? cons length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
	vector-set! display newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr apply map append list-tail call/cc exit-list))

(define init-env
  (lambda ()              ; for now, our initial global environment only contains
    (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc
          *prim-proc-names*)
     (empty-env) (init-k))))

(define global-env (init-env)) ;;Done in cps

; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args env2 k)
    (case prim-proc
      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
      [(quotient) (apply-k k (apply quotient args))]
      [(add1) (apply-k k (+ (1st args) 1))]
      [(sub1) (apply-k k (- (1st args) 1))]
      [(zero?) (apply-k k (= (1st args) 0))]
      [(not) (apply-k k (not (1st args)))]
      [(=) (apply-k k (= (1st args) (2nd args)))]
      [(>) (apply-k k (apply > args))]
      [(>=) (apply-k k (apply >= args))]
      [(<) (apply-k k (apply < args))]
      [(<=) (apply-k k (apply <= args))]
      [(quote) (apply-k k (quote  ((lambda (x) x) (1st args))))]
      [(cons) (apply-k k (apply cons args))]
      [(car) (apply-k k (car (1st args)))]
      [(cdr) (apply-k k (if (null? (1st args)) (eopl:error 'apply-prim-proc "cannot take cdr of ~s" (1st args)) (cdr (1st args))))]
      [(list) (apply-k k args)]
      [(append) (apply-k k (append (1st args) (2nd args)))]
      [(null?) (apply-k k (null? (1st args)))]
      [(assq) (apply-k k (assq (1st args) (2nd args)))]
      [(eq?) (apply-k k (eq? (1st args) (2nd args)))]
      [(equal?) (apply-k k (equal? (1st args) (2nd args)))]
      [(eqv?) (apply-k k (eqv? (1st args) (2nd args)))]
      [(atom?) (apply-k k (atom? args))]
      [(length) (apply-k k (length (1st args)))]
      [(list->vector) (apply-k k (list->vector (1st args)))]
      [(list?) (apply-k k (list? (1st args)))]
      [(list-tail) (apply-k k (list-tail (1st args) (2nd args)))]
      [(pair?) (apply-k k (pair? (1st args)))]
      [(procedure?) (apply-k k (proc-val? (1st args)))]
      [(vector->list) (apply-k k (vector->list (1st args)))]
      [(vector) (apply-k k (list->vector args))]
      [(make-vector) (apply-k k (cond
      					[(= 1 (length args)) (make-vector (1st args) 0)]
      					[(= 2 (length args)) (make-vector (1st args) (2nd args))]
      					[else (eopl:error 'apply-prim-proc "Incorrect number of arguments to make-vector" args)]))]
      [(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
      [(vector?) (apply-k k (vector? (1st args)))]
      [(number?) (apply-k k (number? (1st args)))]
      [(symbol?) (apply-k k (symbol? (1st args)))]
      [(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
      [(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
      [(vector-set!) (apply-k k (apply vector-set! args))]
      [(display) (apply-k k (display args))]
      [(newline) (apply-k k (newline))]
      [(caar) (apply-k k (car (car (1st args))))]
      [(cadr) (apply-k k (car (cdr (1st args))))]
      [(cdar) (apply-k k (cdr (car (1st args))))]
      [(cddr) (apply-k k (cdr (cdr (1st args))))]
      [(caaar) (apply-k k (car (car (car (1st args)))))]
      [(caadr) (apply-k k (car (car (cdr (1st args)))))]
      [(cadar) (apply-k k (car (cdr (car (1st args)))))]
      [(caddr) (apply-k k (car (cdr (cdr (1st args)))))]
      [(cdddr) (apply-k k (cdr (cdr (cdr (1st args)))))]
      [(cddar) (apply-k k (cdr (cdr (car (1st args)))))]
      [(cdaar) (apply-k k (cdr (car (car (1st args)))))]
      [(cdadr) (apply-k k (cdr (car (cdr (1st args)))))]
      [(apply) (apply-proc (1st args) (2nd args) env2 k)]
      [(map) (matrix-transpose2 (cdr args) (map-k args env2 k))]
      [(call/cc) (apply-proc (1st args) (list (continuation-proc k)) env2 k)]
      [(exit-list) (apply-proc (continuation-proc (init-k)) (list args) env2 k)]
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
