; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014

(load "chez-init.ss") 


;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp
   (rand (list-of expression?))]
  [lambda-exp
   (id (list-of symbol?))
   (body (list-of expression?))]
  [lambda-exp-improperls
   (id (list-of improper-list?))
   (body (list-of epxression?))]
  [lambda-exp-nolimit
   (id symbol?)
   (body (list-of expression?))]
  [if-exp
   (id expression?)
   (true expression?)
   (false expression?)]
  [if-exp-ne
   (id expression?)
   (true expression?)]
  [named-let
   (name symbol?)
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (body (list-of expression?))]
  [let-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (body (list-of expression?))]
  [let*-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (body (list-of expression?))]
  [letrec-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (body (list-of expression?))]
  [set-exp
   (id symbol?)
   (body expression?)]
  [quote-exp
   (id scheme-value?)])

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [clos-proc
    (vars (list-of symbol?))
    (body (list-of expression?))
    (env environment?)])
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.

(define parse-exp         
  (lambda (datum)
    (cond
     [(and (list? datum) (eqv? (car datum) 'quote)) (quote-exp (cadr datum))]
     [(symbol? datum) (var-exp datum)]
     [(literal? datum) (lit-exp datum)]
     [(pair? datum)
      (cond
       [(eqv? (car datum) 'lambda)
        (cond
         [(= 2 (length datum)) (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
         [(list? (cadr datum))
          (if (andmap symbol? (cadr datum))
	         (lambda-exp (cadr datum)
			     (map parse-exp (cddr datum))) (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" (cadr datum)))]
         [(symbol? (cadr datum))
          (lambda-exp-nolimit (cadr datum)
			      (map parse-exp (cddr datum)))]
         [(improperlist? (cadr datum))
          (lambda-var-improperls (cadr datum)
                                 (map parse-exp (cddr datum)))])]
       [(eqv? (car datum) 'if)
        (cond
         [(or (> 2 (length (cdr datum))) (< 3 (length (cdr datum)))) (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
         [(= (length (cdr datum)) 3)
	  (if-exp (parse-exp (cadr datum))
		  (parse-exp (caddr datum))
		  (parse-exp (cadddr datum)))]
	 [(= (length (cdr datum)) 2)
	  (if-exp-ne (parse-exp (cadr datum))
		     (parse-exp (caddr datum)))])]
       [(eqv? (car datum) 'set!)
	(if (= (length (cdr datum)) 2)
	    (set-exp (cadr datum)
		     (parse-exp (caddr datum))) (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum))]
       
       [(and (eqv? (car datum) 'let) (> 3 (length datum))) (eopl:error 'parse-exp "let-expression has incorrect length ~s" datum)]
       [(and (eqv? (car datum) 'let) (symbol? (cadr datum)))
	(if (list-of-list? (caddr datum))
	    (if (ormap improper-list? (caddr datum))
		(eopl:error 'parse-exp "declaration in let-exp is not a proper list ~s" datum)
		(if (not (andmap symbol? (map car (caddr datum))))
		    (eopl:error 'parse-exp "vars in let-exp must be symbols ~s" datum)
		    (if (andmap len-2-ls (caddr datum))
			(named-let (parse-exp (cadr datum))
				   (first (caddr datum))
				   (map parse-exp (last (caddr datum)))
				   (map parse-exp (cddr datum)))
			(eopl:error 'parse-exp "declaration in let-exp must be a list of length 2 ~s" datum))))
	    (eopl:error 'parse-exp "declarations in let-expression not a list ~s" datum))]
       [(and (eqv? (car datum) 'let) (list? (cadr datum)))
	(if (list-of-list? (cadr datum))
	    (if (ormap improper-list? (cadr datum))
		(eopl:error 'parse-exp "declaration in let-exp is not a proper list ~s" datum)
		(if (not (andmap symbol? (map car (cadr datum))))
		    (eopl:error 'parse-exp "vars in let-exp must be symbols ~s" datum)
		    (if (andmap len-2-ls (cadr datum))
			(let-exp (first (cadr datum))
				 (map parse-exp (last (cadr datum)))
				 (map parse-exp (cddr datum)))
			(eopl:error 'parse-exp "declaration in let-exp must be a list of length 2 ~s" datum))))
	    (eopl:error 'parse-exp "declarations in let-expression not a list ~s" datum))]
       [(eqv? (car datum) 'let*)
	(if (list-of-list? (cadr datum))
	    (if (ormap improper-list? (cadr datum))
		(eopl:error 'parse-exp "declaration in let-exp is not a proper list ~s" datum)
		(if (not (andmap symbol? (map car (cadr datum))))
		    (eopl:error 'parse-exp "vars in let-exp must be symbols ~s" datum)
		    (if (andmap len-2-ls (cadr datum))
			(let*-exp (first (cadr datum))
				  (map parse-exp (last (cadr datum)))
				  (map parse-exp (cddr datum)))
			(eopl:error 'parse-exp "declaration in let-exp must be a list of length 2 ~s" datum))))
	    (eopl:error 'parse-exp "declarations in let-expression not a list ~s" datum))]
       [(and (eqv? (car datum) 'letrec) (> 3 (length datum))) (eopl:error 'parse-exp "letrec-expression has incorrect length ~s" datum)]
       [(eqv? (car datum) 'letrec)
	(if (list-of-list? (cadr datum))
	    (if (ormap improper-list? (cadr datum))
		(eopl:error 'parse-exp "declaration in let-exp is not a proper list ~s" datum)
		(if (not (andmap symbol? (map car (cadr datum))))
		    (eopl:error 'parse-exp "vars in let-exp must be symbols ~s" datum)
		    (if (andmap len-2-ls (cadr datum))
			(letrec-exp (first (cadr datum))
				    (map parse-exp (last (cadr datum)))
				    (map parse-exp (cddr datum)))
			(eopl:error 'parse-exp "declaration in let-exp must be a list of length 2 ~s" datum))))
	    (eopl:error 'parse-exp "declarations in let-expression not a list ~s" datum))]
       [else (if (improper-list? datum) (eopl:error 'parse-exp "expression ~s is not a proper list" datum) (app-exp (map parse-exp datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
           [var-exp (id) id]
           [lit-exp (id) id]
           [set-exp (id body)
                    (list 'set! id body)]
           [lambda-exp (id body)
                       (append (list 'lambda) (list id)
			       (map unparse-exp body))]
           [lambda-exp-nolimit (id body)
                               (append (list 'lambda id)
                                       (map unparse-exp body))]
           [lambda-exp-improperls (id body)
                                  (append (list 'lambda id)
                                          (map unparse-exp body))]
           [if-exp (test true false)
                   (list 'if (unparse-exp test)
                         (unparse-exp true)
                         (unparse-exp false))]
           [if-exp-ne (test true)
                      (list 'if (unparse-exp test)
                            (unparse-exp true))]
           [let-exp (vars vals body)
                    (append (list 'let (combine-vars-vals vars (map unparse-exp vals)))
                            (map unparse-exp body))]
           [named-let (name vars vals body)
                      (append (list 'let name (combine-vars-vals vars (map unparse-exp vals)))
                              (map unparse-exp body))] 
           [let*-exp (vars vals body)
                     (append (list 'let* (combine-vars-vals vars (map unparse-exp vals)))
                             (map unparse-exp body))]
           [letrec-exp (vars vals body)
		       (append (list 'letrec (combine-vars-vals vars (map unparse-exp vals)))
			       (map unparse-exp body))]
           [app-exp (rand)
                    (map unparse-exp rand)])))

(define first
  (lambda (ls-of-ls)
    (cond [(null? ls-of-ls) '()]
          [else (cons (caar ls-of-ls) (first (cdr ls-of-ls)))])))

(define last
  (lambda (ls-of-ls)
    (cond [(null? ls-of-ls) '()]
          [else (cons (cadar ls-of-ls) (last (cdr ls-of-ls)))])))

(define improper-list?
  (lambda (x)
    (and (pair? x) (not (list? x)))))

(define list-of-list?
  (lambda (ls)
    (and (list? ls)
         (andmap list? ls))))

(define len-2-ls
  (lambda (ls)
    (= (length ls) 2)))

(define combine-vars-vals
  (lambda (vars vals)
    (cond [(null? vars) '()]
          [else (append (list (list (car vars) (car vals)))
                        (combine-vars-vals (cdr vars) (cdr vals)))])))

(define literal?
  (lambda (exp)
    (or (number? exp)
        (boolean? exp)
        (string? exp)
	(vector? exp))))

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env env sym succeed fail)))))))

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
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = > >= < <= car cdr list null? assq eq? equal? eqv? atom? cons length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
	vector-set! display newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr))

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
      [(vector) (list->vector (1st args))]
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

