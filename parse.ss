; Parser for Scheme
; Heavily modified by Jason Lane and Sam Pastoriza
; Class modifications by Lexie Harris, Jason Lane, and Sam Pastoriza

(define parse-exp                                                                                                 ; Used to parse Scheme syntax into abstract syntax
  (lambda (datum)
    (cond
      [(and (list? datum) (eqv? (car datum) 'quote))                                                              ; quote-exp
        (quote-exp
          (cadr datum))]
      [(and (list? datum) (eqv? (car datum) 'member))                                                             ; member-exp
        (member-exp
          (parse-exp (cadr datum))
          (cadar (map parse-exp (cddr datum))))]
      [(and (list? datum) (eqv? (car datum) 'and))                                                                ; and-exp
        (and-exp
          (map parse-exp (cdr datum)))]
      [(and (list? datum) (eqv? (car datum) 'or))                                                                 ; or-exp
        (se (or-exp (map parse-exp (cdr datum))))]
      [(and (list? datum) (eqv? (car datum) 'when))                                                               ; when-exp
        (when-exp
          (parse-exp (cadr datum))
          (map parse-exp (cddr datum)))]
      [(and (list? datum) (eqv? (car datum) 'cond))                                                               ; cond-exp
        (let ((transp (matrix-transpose-map (cdr datum))))
	        (cond-exp
            (map parse-exp (car transp))
            (map parse-exp (cadr transp))))]
      [(and (list? datum) (eqv? (car datum) 'case))                                                               ; case-exp
        (let ((transp (matrix-transpose-map (cddr datum))))
	        (case-exp
            (parse-exp (cadr datum))
            (map parse-exp (car transp))
            (map parse-exp (cadr transp))))]
      [(symbol? datum) (var-exp datum)]                                                                           ; var-exp
      [(literal? datum) (lit-exp datum)]                                                                          ; lit-exp
      [(pair? datum)
        (cond
          [(eqv? (car datum) 'class)                                                                              ; class-exp
            (class-parse datum)]
          [(eqv? (car datum) 'case-lambda)                                                                        ; case-lambda-exp
            (let ((transp (matrix-transpose (cdr datum))))
              (case-lambda-exp
                (car transp)
                (map length (car transp))
                (map (lambda (x) (map parse-exp x)) (cadr transp))))]
          [(eqv? (car datum) 'lambda)                                                                       
            (cond
              [(= 2 (length datum))
                (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
              [(list? (cadr datum))                                                                               ; lambda-exp
                (if (andmap sym-or-ref? (cadr datum))
	                (lambda-exp
                    (cadr datum)
                    (parse-refs (find-ref (cadr datum))
                      (map parse-exp (cddr datum))))
                  (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" (cadr datum)))]
              [(symbol? (cadr datum))                                                                             ; lambda-exp-nolimit
                (lambda-exp-nolimit
                  (cadr datum)
                  (map parse-exp (cddr datum)))]
              [(improper-list? (cadr datum))                                                                      ; lambda-exp-improperls
	              (let ((res (split-list (cadr datum))))
	                (lambda-exp-improperls
	                  (car res)
	                  (cadr res)
	                  (map parse-exp (cddr datum))))])]
          [(eqv? (car datum) 'if)
            (cond
              [(or (> 2 (length (cdr datum))) (< 3 (length (cdr datum))))
                (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
              [(= (length (cdr datum)) 3)                                                                         ; if-exp
	              (if-exp (parse-exp (cadr datum))                                                             
		            (parse-exp (caddr datum))
		            (parse-exp (cadddr datum)))]
	            [(= (length (cdr datum)) 2)                                                                         ; if-exp-ne
	              (if-exp-ne                                                                                   
                  (parse-exp (cadr datum))
		              (parse-exp (caddr datum)))])]
          [(eqv? (car datum) 'set!)                                                                               ; set-exp
	          (if (= (length (cdr datum)) 2)
	            (set-exp
                (parse-exp (cadr datum))
                (parse-exp (caddr datum))) ;;SET_EXP EXPRESSION
              (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum))]
          [(eqv? (car datum) 'define)                                                                             ; define-exp
            (define-exp
              (cadr datum)
              (parse-exp (caddr datum)))]
          [(and (eqv? (car datum) 'let) (> 3 (length datum)))
            (eopl:error 'parse-exp "let-expression has incorrect length ~s" datum)]
          [(and (eqv? (car datum) 'let) (symbol? (cadr datum)))                                                   ; named-let
	          (if (list-of-list? (caddr datum))
	            (if (ormap improper-list? (caddr datum))
		            (eopl:error 'parse-exp "declaration in let-exp is not a proper list ~s" datum)
		            (if (not (andmap symbol? (map car (caddr datum))))
		              (eopl:error 'parse-exp "vars in let-exp must be symbols ~s" datum)
		              (if (andmap len-2-ls (caddr datum))
			              (se (named-let (cadr datum)
                      (first (caddr datum))
                      (map parse-exp (last (caddr datum)))
                      (map parse-exp (cdddr datum))))
			              (eopl:error 'parse-exp "declaration in let-exp must be a list of length 2 ~s" datum))))
	            (eopl:error 'parse-exp "declarations in let-expression not a list ~s" datum))]
          [(and (eqv? (car datum) 'let) (list? (cadr datum)))                                                     ; let-exp
	          (if (list-of-list? (cadr datum))
	            (if (ormap improper-list? (cadr datum))
		            (eopl:error 'parse-exp "declaration in let-exp is not a proper list ~s" datum)
		            (if (not (andmap symbol? (map car (cadr datum))))
		              (eopl:error 'parse-exp "vars in let-exp must be symbols ~s" datum)
		              (if (andmap len-2-ls (cadr datum))
			              (se (let-exp
                      (first (cadr datum))
                      (map parse-exp (last (cadr datum)))
                      (map parse-exp (cddr datum)))) ;;LET EXPRESSION
			              (eopl:error 'parse-exp "declaration in let-exp must be a list of length 2 ~s" datum))))
	            (eopl:error 'parse-exp "declarations in let-expression not a list ~s" datum))]
          [(eqv? (car datum) 'let*)                                                                               ; let*-exp
	          (if (list-of-list? (cadr datum))
	            (if (ormap improper-list? (cadr datum))
		            (eopl:error 'parse-exp "declaration in let-exp is not a proper list ~s" datum)
		            (if (not (andmap symbol? (map car (cadr datum))))
		              (eopl:error 'parse-exp "vars in let-exp must be symbols ~s" datum)
		              (if (andmap len-2-ls (cadr datum))
			              (se (let*-exp
                      (first (cadr datum))
                      (map parse-exp (last (cadr datum)))
                      (map parse-exp (cddr datum))))
			              (eopl:error 'parse-exp "declaration in let-exp must be a list of length 2 ~s" datum))))
	          (eopl:error 'parse-exp "declarations in let-expression not a list ~s" datum))]
          [(and (eqv? (car datum) 'letrec) (> 3 (length datum))) 
            (eopl:error 'parse-exp "letrec-expression has incorrect length ~s" datum)]
          [(eqv? (car datum) 'letrec)                                                                             ; letrec-exp
	          (if (list-of-list? (cadr datum))
	            (if (ormap improper-list? (cadr datum))
		            (eopl:error 'parse-exp "declaration in let-exp is not a proper list ~s" datum)
		            (if (not (andmap symbol? (map car (cadr datum))))
		              (eopl:error 'parse-exp "vars in let-exp must be symbols ~s" datum)
		              (if (andmap len-2-ls (cadr datum))
                    (letrec-exp
                      (first (cadr datum))
                      (find-idss (cadr datum) '())
                      (map parse-exp (last (cadr datum)))
                      (map parse-exp (cddr datum)))
			              (eopl:error 'parse-exp "declaration in let-exp must be a list of length 2 ~s" datum))))
	          (eopl:error 'parse-exp "declarations in let-expression not a list ~s" datum))]
          [(eqv? (car datum) 'begin)                                                                              ; begin-exp
	          (syntax-expand (begin-exp (map parse-exp (cdr datum))))]
          [(eqv? (car datum) 'while)                                                                              ; while-exp
	          (while-exp
              (parse-exp (cadr datum))
              (map parse-exp (cddr datum)))]
          [else (if (improper-list? datum)
            (eopl:error 'parse-exp "expression ~s is not a proper list" datum) (app-exp (map parse-exp datum)))])]
        [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define parse-refs                                                                                                ; Used to place ref-exps where appropriate
  (lambda (ls exp)
    (cond [(null? exp) exp]
          [(expression? exp)
            (cases expression exp
                [var-exp (id) (if (member id ls)
                                  (ref-exp id)
                                  exp)]
                [lit-exp (id) exp]
                [app-exp (id) (app-exp (parse-refs ls id))]
                [set-exp (id body) (set-exp (parse-refs ls id) (parse-refs ls body))]
                [lambda-exp (id body) (lambda-exp id (parse-refs ls body))]
                [else exp])]
          [(list-of expression?) (cons (parse-refs ls (car exp)) (parse-refs ls (cdr exp)))])))

(define find-ref                                                                                                  ; Gets a list of the elements of ls which are refs
  (lambda (ls)
    (cond [(null? ls) ls]
          [(list? (car ls))
                  (cons (cadar ls) (find-ref (cdr ls)))]
          [else (find-ref (cdr ls))])))

(define find-noref                                                                                                ; Gets a list of the elements of ls which are not refs
  (lambda (ls)
    (cond [(null? ls) ls]
          [(symbol? (car ls)) (cons (car ls) (find-noref (cdr ls)))]
          [else (find-noref (cdr ls))])))

(define unparse-exp                                                                                               ; Converts abstract syntax back to Scheme syntax
  (lambda (exp)                                                                                                   ; incomplete and not very useful
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
           [lambda-exp-improperls (reqs non-req body)
                                  (append (list 'lambda (append reqs non-req))
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
	   [while-exp (test body)
		      (append (list 'while (unparse-exp test)) (map unparse-exp body))]
           [app-exp (rand)
                    (map unparse-exp rand)]
	   [else exp])))

(define first                                                                                                     ; Gets a list of the first elements of the internal lists
  (lambda (ls-of-ls)                                                                                              ; should probably be replaced by a map
    (cond [(null? ls-of-ls) '()]
          [else (cons (caar ls-of-ls) (first (cdr ls-of-ls)))])))

(define last                                                                                                      ; Gets a list of the second elements of the internal lists
  (lambda (ls-of-ls)                                                                                              ; should be replaced by a map and given a better name
    (cond [(null? ls-of-ls) '()]
          [else (cons (cadar ls-of-ls) (last (cdr ls-of-ls)))])))

(define improper-list?                                                                                            ; Verifies that y is an improper list
  (lambda (y)
    (and (pair? y) (not (list? y)))))

(define list-of-list?                                                                                             ; Verifies that ls is a list of lists
  (lambda (ls)
    (and (list? ls)
         (andmap list? ls))))

(define sym-or-ref?                                                                                               ; Verifies that x is either a symbol or a ref-exp (used extensively in ref)
  (lambda (x) (or (symbol? x)
              (and (list? x) (eqv? (car x) 'ref)))))

(define len-2-ls                                                                                                  ; Used to ensure tha a list is a two-list
  (lambda (ls)                                                                                                    ; not complex in any way, but faster to type
    (= (length ls) 2)))

(define combine-vars-vals                                                                                         ; Used by unparse to recombine a let's vars and their assignments
  (lambda (vars vals)                                                                                             ; Could probably be replaced by a simple map
    (cond [(null? vars) '()]
          [else (append (list (list (car vars) (car vals)))
                        (combine-vars-vals (cdr vars) (cdr vals)))])))

(define literal?                                                                                                  ; Determines whether exp is a literal
  (lambda (exp)
    (or (number? exp)
        (boolean? exp)
        (string? exp)
	(vector? exp))))

(define split-list                                                                                                ; Splits an improper list into two lists (x y . z) -> ((x y) (z))
  (lambda (ls)
    (let loop ((ls ls) (ls1 '()))
	    (cond [(improper-list? ls)
	            (loop (cdr ls) (append ls1 (list (car ls))))]
	          [else (list ls1 (list ls))]))))

(define find-idss                                                                                                 ; Finds the arguments to any procedures defined as a letrec's variables
  (lambda (list-of-list ls)
    (cond [(null? list-of-list) (reverse ls)]
	  [(and (and (not (null? (cadar list-of-list))) (list? (cadar list-of-list))) (equal? (caadar list-of-list) 'lambda))
	   (find-idss (cdr list-of-list) (cons (car (cdadar list-of-list)) ls))]
	  [else
	   (find-idss (cdr list-of-list) (cons '() ls))])))
