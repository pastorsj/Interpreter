; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.

(define parse-exp         
  (lambda (datum)
    (cond
     [(and (list? datum) (eqv? (car datum) 'quote)) 
      (quote-exp (cadr datum))]
      [(and (list? datum) (eqv? (car datum) 'member)) 
      (member-exp (parse-exp (cadr datum)) (cadar (map parse-exp (cddr datum))))]
     [(and (list? datum) (eqv? (car datum) 'and)) 
      (and-exp (map parse-exp (cdr datum)))]
     [(and (list? datum) (eqv? (car datum) 'or)) 
      (or-exp (map parse-exp (cdr datum)))]
     [(and (list? datum) (eqv? (car datum) 'when)) 
      (when-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))]
     [(and (list? datum) (eqv? (car datum) 'cond)) 
      (let ((transp (matrix-transpose (cdr datum))))
	(cond-exp (map parse-exp (car transp)) (map parse-exp (cadr transp))))]
     [(and (list? datum) (eqv? (car datum) 'case))
      (let ((transp (matrix-transpose (cddr datum))))
	(case-exp (parse-exp (cadr datum)) (map parse-exp (car transp)) (map parse-exp (cadr transp))))]
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
         [(improper-list? (cadr datum))
	  (let ((res (split-list (cadr datum))))
	    (lambda-exp-improperls 
	     (car res)
	     (cadr res)
	     (map parse-exp (cddr datum))))])]
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
				    (find-idss (cadr datum) '())
				    (map parse-exp (last (cadr datum)))
				    (map parse-exp (cddr datum)))
			(eopl:error 'parse-exp "declaration in let-exp must be a list of length 2 ~s" datum))))
	    (eopl:error 'parse-exp "declarations in let-expression not a list ~s" datum))]
       [(eqv? (car datum) 'begin)
	(begin-exp (map parse-exp (cdr datum)))]
       [(eqv? (car datum) 'while)
	(while-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))]
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

(define first
  (lambda (ls-of-ls)
    (cond [(null? ls-of-ls) '()]
          [else (cons (caar ls-of-ls) (first (cdr ls-of-ls)))])))

(define last
  (lambda (ls-of-ls)
    (cond [(null? ls-of-ls) '()]
          [else (cons (cadar ls-of-ls) (last (cdr ls-of-ls)))])))

(define improper-list?
  (lambda (y)
    (and (pair? y) (not (list? y)))))

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

(define split-list
    (lambda (ls)
      (let loop ((ls ls) (ls1 '()))
	(cond [(improper-list? ls)
	       (loop (cdr ls) (append ls1 (list (car ls))))]
	      [else (list ls1 (list ls))]))))

(define find-idss
  (lambda (list-of-list ls)
    (cond [(null? list-of-list) (reverse ls)]
	  [(and (list? (cadar list-of-list)) (equal? (caadar list-of-list) 'lambda))
	   (find-idss (cdr list-of-list) (cons (car (cdadar list-of-list)) ls))]
	  [else 
	   (find-idss (cdr list-of-list) ls)])))










