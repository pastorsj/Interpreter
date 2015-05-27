(define class-parse
	(lambda (datum)
		(define-exp (cadr datum) (class-exp (map typify (caddr datum)) (map (lambda (x) (method-parse x (car datum))) (cadddr datum))))))

(define-datatype classvar classvar?
	[public-var
		(pred symbol?)
		(name symbol?)
		(val scheme-value?)]
	[private-var
		(pred symbol?)
		(name symbol?)
		(val scheme-value?)]
	[public-static-var
		(pred symbol?)
		(name symbol?)
		(val scheme-value?)]
	[private-static-var
		(pred symbol?)
		(name symbol?)
		(val scheme-value?)])

(define-datatype method method?
	[public-method
		(name symbol?)
		(args (list-of (lambda (x) (or (null? x) (and (symbol? (car x)) (symbol? (cadr x)) (scheme-value? (caddr x)))))))
		(body expression?)]
	[private-method
		(name symbol?)
		(args (list-of (lambda (x) (or (null? x) (and (symbol? (car x)) (symbol? (cadr x)) (scheme-value? (caddr x)))))))
		(body expression?)]
	[public-static-method
		(name symbol?)
		(args (list-of (lambda (x) (or (null? x) (and (symbol? (car x)) (symbol? (cadr x)) (scheme-value? (caddr x)))))))
		(body expression?)]
	[private-static-method
		(name symbol?)
		(args (list-of (lambda (x) (or (null? x) (and (symbol? (car x)) (symbol? (cadr x)) (scheme-value? (caddr x)))))))
		(body expression?)])

(define typify
	(lambda (ls)
			(case (car ls)
				[(public)
					(if (equal? (cadr ls) 'static)
						(public-static-var (caddr ls) (cadddr ls) (cond [(null? (cddddr ls)) (void)] [else (car (cddddr ls))]))
																		;[((caddr ls) (car (cddddr ls))) (car (cddddr ls))] [else (eopl:error 'fields "bad type")]))
						(public-var (cadr ls) (caddr ls) (cond [(null? (cdddr ls)) (void)] [else (cadddr ls)])))]
						;[((cadr ls) (car (cdddr ls))) (car (cdddr ls))] [else (eopl:error 'fields "bad type")])))]
				[(private)
					(if (equal? (cadr ls) 'static)
						(private-static-var (caddr ls) (cadddr ls) (cond [(null? (cddddr ls)) (void)] [else (car (cddddr ls))]))
						;[((caddr ls) (car (cddddr ls))) (car (cddddr ls))] [else (eopl:error 'fields "bad type")]))
						(private-var (cadr ls) (caddr ls) (cond [(null? (cdddr ls)) (void)] [else (cadddr ls)])))]
						;[((cadr ls) (car (cdddr ls))) (car (cdddr ls))] [else (eopl:error 'fields "bad type")])))]
				[(static)
					(public-static-var (cadr ls) (caddr ls) (cond [(null? (cdddr ls)) (void)] [else (cadddr ls)]))]
					;[((cadr ls) (car (cdddr ls))) (car (cdddr ls))] [else (eopl:error 'fields "bad type")]))]))))
				[else (public-var (car ls) (cadr ls) (cond [(null? (cddr ls)) (void)] [else (caddr ls)]))])))

(define method-parse
	(lambda (method classname)
		(case (car method)
			[(public)
				(if (equal? (cadr method) 'static)
					(let ((args (cadddr method)) (name (caddr method)))
						(public-static-method name (add-defaults args) (se (parse-exp (list 'begin (car (cddddr method)))))))
					(let ((args (caddr method)) (name (cadr method)))
							(public-method name (add-defaults args) (se (parse-exp (list 'begin (cadddr method)))))))]
			[(private)
				(if (equal? (cadr method) 'static)
					(let ((args (cadddr method)) (name (caddr method)))
						(private-static-method name (add-defaults args) (se (parse-exp (list 'begin (car (cddddr method)))))))
					(let ((args (caddr method)) (name (cadr method)))
							(private-method name (add-defaults args) (se (parse-exp (list 'begin (cadddr method)))))))]
			[(static)
					(let ((args (caddr method)))
						(public-static-method (add-defaults args) (se (parse-exp (list 'begin (cadddr method))))))]
			[else (let ((args (cadr method)))
							(public-method name (add-defaults args) (se (parse-exp (list 'begin (caddr method))))))])))

(define replace-this
	(lambda (code)
		(let recur ((code code) (iscar #t))
			(cond 	[(null? code) code]
					[(list? (car code)) (cons (recur (car code) #t)	(recur (cdr code) #f))]
					[(iscar) (if (eqv? (car code) 'this)
								(cons 'temp (recur (cdr code) #f))
								(cons (car code) (recur (cdr code) #f)))]))))

(define add-defaults
	(lambda (args)
		(if (null? (car args)) args
			(map (lambda (x) (if (null? (caddr x)) (list (car x) (cadr x) (void)) (if ((car x) (caddr x)) x (eopl:error 'methods "bad default value")))) args))))