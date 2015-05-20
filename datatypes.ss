
;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [ref-exp
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
   (id (list-of sym-or-ref?))
   (body (list-of expression?))]
  [lambda-exp-improperls
   (reqs (list-of symbol?))
   (non-req (list-of symbol?))
   (body (list-of expression?))]
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
   (procnames (list-of symbol?))
   (idss (list-of (lambda (x) (ormap (lambda (pred) (pred x)) (list improper-list? (list-of symbol?))))))
   (bodies (list-of expression?))
   (letrec-body (list-of expression?))]
  [set-exp
   (id expression?)
   (body expression?)]
  [quote-exp
   (id scheme-value?)]
  [when-exp
   (test expression?)
   (body (list-of expression?))]
  [begin-exp
   (body (list-of expression?))]
  [while-exp
   (test expression?)
   (body (list-of expression?))]
  [cond-exp
   (conditions (list-of expression?))
   (bodies (list-of expression?))]
  [and-exp
   (body (list-of expression?))]
  [or-exp
   (body (list-of expression?))]
  [case-exp
   (id expression?)
   (conditions (list-of expression?))
   (body (list-of expression?))]
  [member-exp
   (id expression?)
   (body (list-of expression?))]
  [case-lambda-exp
   (idss (list-of (list-of symbol?)))
   (lens (list-of number?))
   (bodies (list-of (list-of expression?)))]
  [define-exp
   (id symbol?)
   (body expression?)])


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [clos-proc
    (vars (list-of sym-or-ref?))
    (body (list-of expression?))
    (env environment?)]
  [clos-improc
    (vars (list-of symbol?))
    (body (list-of expression?))
    (env environment?)]
  [case-clos-proc
   (idss (list-of (list-of symbol?)))
   (lens (list-of number?))
   (bodies (list-of (list-of expression?)))
   (env environment?)]
  [member-proc
   (item expression?)
   (ls expression?)])



;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)]
  [recursively-extended-env-record
   (proc-names (list-of symbol?))
   (idss (list-of (lambda (x)
                    (ormap (lambda (pred) (pred x))
                      (list improper-list? (list-of symbol?))))))
   (bodies (list-of expression?))
   (env environment?)])

;For cells, use box datatype

;; continuations

(define-datatype continuation continuation?
	[init-k]
  [recursive-extend-k
    (idss (list-of (lambda (x)
        (or (improper-list? x) ((list-of symbol?) x)))))
    (env environment?)
    (k continuation?)]
  [map-k
  	(args (list-of scheme-value?))
  	(env environment?)
  	(k continuation?)]
  [app-k
  	(rands (list-of expression?))
  	(env environment?)
  	(k continuation?)]
  [app-rands-k
  	(proc proc-val?)
  	(env environment?)
  	(k continuation?)]
  [member-k
  	(ls list?)
  	(env environment?)
  	(k continuation?)]
  [member-help-k
  	(item scheme-value?)
  	(k continuation?)]
  [set-replace-body-k
    (var sym-or-ref?)
    (body expression?)
    (arg expression?)
    (k continuation?)]
  [set-replace-both-k
    (res expression?)
    (k continuation?)]
  [replace-refs-k
    (vars (list-of sym-or-ref?))
    (args (list-of scheme-value?))
    (k continuation?)]
  [ref-help-k
    (res (list-of list?))
    (k continuation?)]
  [replace-help-k
  	(var sym-or-ref?)
  	(exp expression?)
  	(arg expression?)
  	(k continuation?)]
  [replace-please-stop-k
  	(res (list-of expression?))
  	(k continuation?)]
  [lambda-ref-k
  	(id (list-of symbol?))
  	(k continuation?)]
  [app-ref-k
  	(k continuation?)]
  [if-k
  	(env environment?)
  	(conds (list-of expression?))
  	(k continuation?)]
  )
