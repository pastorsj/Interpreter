; Datatypes used in interpreter
; Heavily modified by Jason Lane and Sam Pastoriza

(define-datatype expression expression?                                 ; expressions (made via parsing the input to abstract syntax, used by the interpreter)
  [var-exp                                                              ; any variable
   (id symbol?)]
  [ref-exp                                                              ; reference variable, used to pass a variable into a procedure instead of the variable's value
   (id symbol?)]
  [lit-exp                                                              ; literals (lists, numbers, etc.)
   (datum
    (lambda (x)
      (ormap
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp                                                              ; application (in Scheme, the parens around a procedure call)
   (rand (list-of expression?))]
  [lambda-exp                                                           ; lambdas with explicit arguments ('lambda (x y)', etc.)
   (id (list-of sym-or-ref?))
   (body (list-of expression?))]
  [lambda-exp-improperls                                                ; lambdas with arbitrary arguments in improper list form ('lambda (x . y)', etc.)
   (reqs (list-of symbol?))
   (non-req (list-of symbol?))
   (body (list-of expression?))]
  [lambda-exp-nolimit                                                   ; lambdas with an arbitrary argument ('lambda x', etc.)
   (id symbol?)
   (body (list-of expression?))]
  [if-exp                                                               ; if with two bodies (if (x) y z)
   (id expression?)
   (true expression?)
   (false expression?)]
  [if-exp-ne                                                            ; if with one body (if (x) y)
   (id expression?)
   (true expression?)]
  [named-let                                                            ; named lets ('let loop (())', etc.)
   (name symbol?)
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (body (list-of expression?))]
  [let-exp                                                              ; standard lets
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (body (list-of expression?))]
  [let*-exp                                                             ; let*s
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (body (list-of expression?))]
  [letrec-exp                                                           ; letrecs
   (procnames (list-of symbol?))
   (idss (list-of (lambda (x) (ormap (lambda (pred) (pred x)) (list improper-list? (list-of symbol?))))))
   (bodies (list-of expression?))
   (letrec-body (list-of expression?))]
  [set-exp                                                              ; set!
   (id expression?)
   (body expression?)]
  [quote-exp                                                            ; quote and '
   (id scheme-value?)]
  [when-exp                                                             ; when (for loop) (not standard in Petite/Chez Scheme)
   (test expression?)
   (body (list-of expression?))]
  [begin-exp                                                            ; begin
   (body (list-of expression?))]
  [while-exp                                                            ; while loop (not standard in Petite/Chez Scheme)
   (test expression?)
   (body (list-of expression?))]
  [cond-exp                                                             ; cond
   (conditions (list-of expression?))
   (bodies (list-of expression?))]
  [and-exp                                                              ; and
   (body (list-of expression?))]
  [or-exp                                                               ; or
   (body (list-of expression?))]
  [case-exp                                                             ; case
   (id expression?)
   (conditions (list-of expression?))
   (body (list-of expression?))]
  [member-exp                                                           ; member
   (id expression?)
   (body (list-of expression?))]
  [case-lambda-exp                                                      ; case-lambda (executes a body selected based on arguments)
   (idss (list-of (list-of symbol?)))
   (lens (list-of number?))
   (bodies (list-of (list-of expression?)))]
  [define-exp                                                           ; define
   (id symbol?)
   (body expression?)]
  [class-exp                                                            ; CLASSES (not standard in Petite/Chez Scheme)
   (fields (list-of classvar?))
   (methods (list-of method?))])


(define-datatype proc-val proc-val?                                     ; Procedure datatypes (used in apply-proc)
  [prim-proc                                                            ; Primitive procedures
   (name symbol?)]
  [clos-proc                                                            ; Closures (created by lambda-exp)
    (vars (list-of sym-or-ref?))
    (body (list-of expression?))
    (env environment?)]
  [clos-improc                                                          ; Closures (created by lambda-exp-improperls)
    (vars (list-of symbol?))
    (body (list-of expression?))
    (env environment?)]
  [case-clos-proc                                                       ; Closures (created by case-lambda)
   (idss (list-of (list-of symbol?)))
   (lens (list-of number?))
   (bodies (list-of (list-of expression?)))
   (env environment?)]
  [member-proc                                                          ; Procedure made by member-exp
   (item expression?)
   (ls expression?)])


(define scheme-value?                                                   ; Because the fields of a datatyp need a procedure, this is used for a field where anything goes
  (lambda (x) #t))

(define-datatype environment environment?                               ; Environment datatypes. Used to store variables and their associated values
  [empty-env-record]                                                    ; The empty environment
  [extended-env-record                                                  ; An extended environment, which build upon the environment it contains
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)]
  [recursively-extended-env-record                                      ; A recursively extended environment, in which the variables can use each other. Used by letrec and named-let
   (proc-names (list-of symbol?))
   (idss (list-of (lambda (x)
                    (ormap (lambda (pred) (pred x))
                      (list improper-list? (list-of symbol?))))))
   (bodies (list-of scheme-value?))
   (env environment?)])