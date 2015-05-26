(define class-parse
	(lambda (datum)
		(define-exp (cadr datum) (class-exp (map typify (caddr datum)) (map (lambda (x) (method-parse x (car datum))) (cadddr datum))))))

(define-datatype classvar classvar?
	[public-var
		(pred procedure?)
		(name symbol?)
		(val scheme-value?)]
	[private-var
		(pred procedure?)
		(name symbol?)
		(val scheme-value?)]
	[public-static-var
		(pred procedure?)
		(name symbol?)
		(val scheme-value?)]
	[private-static-var
		(pred procedure?)
		(name symbol?)
		(val scheme-value?)])

(define-datatype method method?
	[public-method
		(name symbol?)
		(args (list-of (lambda (x) (or (null? x) (and (procedure? (car x)) (symbol? (cadr x)) (scheme-value? (caddr x)))))))
		(body (list-of expression?))]
	[private-method
		(name symbol?)
		(args (list-of (lambda (x) (or (null? x) (and (procedure? (car x)) (symbol? (cadr x)) (scheme-value? (caddr x)))))))
		(body (list-of expression?))]
	[public-static-method
		(name symbol?)
		(args (list-of (lambda (x) (or (null? x) (and (procedure? (car x)) (symbol? (cadr x)) (scheme-value? (caddr x)))))))
		(body (list-of expression?))]
	[private-static-method
		(name symbol?)
		(args (list-of (lambda (x) (or (null? x) (and (procedure? (car x)) (symbol? (cadr x)) (scheme-value? (caddr x)))))))
		(body (list-of expression?))]
	[public-constr-method
		(args (list-of (lambda (x) (or (null? x) (and (procedure? (car x)) (symbol? (cadr x)) (scheme-value? (caddr x)))))))
		(body (list-of expression?))]
	[private-constr-method
		(args (list-of (lambda (x) (or (null? x) (and (procedure? (car x)) (symbol? (cadr x)) (scheme-value? (caddr x)))))))
		(body (list-of expression?))])

(define typify
	(lambda (ls)
		(if (procedure? (car ls))
			(public-var (car ls) (cadr ls) (if (null? (cddr ls)) (void) (caddr ls)))
			(case (car ls)
				[(public)
					(if (equal? (cadr ls) 'static)
						(public-static-var (caddr ls) (cadddr ls) (cond [(null? (cddddr ls)) (void)] [((caddr ls) (car (cddddr ls))) (car (cddddr ls))] [else (eopl:error 'fields "bad type")]))
						(public-var (cadr ls) (caddr ls) (cond [(null? (cdddr ls)) (void)] [((cadr ls) (car (cdddr ls))) (car (cdddr ls))] [else (eopl:error 'fields "bad type")])))]
				[(private)
					(if (equal? (cadr ls) 'static)
						(private-static-var (caddr ls) (cadddr ls) (cond [(null? (cddddr ls)) (void)] [((caddr ls) (car (cddddr ls))) (car (cddddr ls))] [else (eopl:error 'fields "bad type")]))
						(private-var (cadr ls) (caddr ls) (cond [(null? (cdddr ls)) (void)] [((cadr ls) (car (cdddr ls))) (car (cdddr ls))] [else (eopl:error 'fields "bad type")])))]
				[(static)
					(public-static-var (cadr ls) (caddr ls) (cond [(null? (cdddr ls)) (void)] [((cadr ls) (car (cdddr ls))) (car (cdddr ls))] [else (eopl:error 'fields "bad type")]))]))))

(define method-parse
	(lambda (method classname)
		(case (car method)
			[(public)
				(if (equal? (cadr ls) 'static)
					(let ((args (cadddr method)) (name (caddr method)))
						(public-static-method name (add-defaults args) (se (parse-exp (list 'begin (car (cddddr method)))))))
					(let ((args (caddr method)) (name (cadr method)))
						(if (eqv? classname name)
							(public-constr-method (add-defaults args) (se (parse-exp `(let ((temp (init classname))) ,(replace-this (cadddr method)) temp))))
							(public-method name (add-defaults args) (se (parse-exp (list 'begin (cadddr method))))))))]
			[(private)
				(if (equal? (cadr ls) 'static)
					(let ((args (cadddr method)) (name (caddr method)))
						(private-static-method name (add-defaults args) (se (parse-exp (list 'begin (car (cddddr method)))))))
					(let ((args (caddr method)) (name (cadr method)))
						(if (eqv? classname name)
							(private-constr-method (add-defaults args) (se (parse-exp `(let ((temp (init classname))) ,(replace-this (cadddr method)) temp))))
							(private-method name (add-defaults args) (se (parse-exp (list 'begin (cadddr method))))))))]
			[(static)
					(let ((args (caddr method)))
						(public-static-method (add-defaults args) (se (parse-exp (list 'begin (cadddr method))))))]
			[else (let ((args (cadr method)))
						(if (eqv? classname name)
							(public-constr-method (add-defaults args) (se (parse-exp `(let ((temp (init classname))) ,(replace-this (caddr method)) temp))))
							(public-method name (add-defaults args) (se (parse-exp (list 'begin (caddr method)))))))])))

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
		(map (lambda (x) (if (null? (caddr x)) (list (car x) (cadr x) (void)) (if ((car x) (caddr x)) x (eopl:error 'methods "bad default value")))) args)))