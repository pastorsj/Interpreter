
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
   (id scheme-value?)]
  [when-exp
   (test expression?)
   (body (list-of expression?))])

	
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