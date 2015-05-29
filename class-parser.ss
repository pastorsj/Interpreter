; Procedures used in parsing classes.
; Created by Lexie Harris, Jason Lane, and Sam Pastoriza from 25 May 2015 to 28 May 2015

(define class-parse 															; Used to define a class as its name and parse its fields. Calls the other procs in this file.
	(lambda (datum)
		(define-exp (cadr datum) (class-exp (map typify (caddr datum)) (map (lambda (x) (method-parse x (car datum))) (cadddr datum))))))

(define-datatype classvar classvar? 											; Field/static variable datatype
	[public-var 																; public var
		(pred symbol?) 															; predicates are not used in this version
		(name symbol?)
		(val scheme-value?)]
	[private-var 																; private var
		(pred symbol?)
		(name symbol?)
		(val scheme-value?)]
	[public-static-var 															; public static var
		(pred symbol?)
		(name symbol?)
		(val scheme-value?)]
	[private-static-var 														; private static var
		(pred symbol?)
		(name symbol?)
		(val scheme-value?)])

(define-datatype method method? 												; Method datatype
	[public-method 																; public method
		(name symbol?)
		(args (list-of arg?))
		(body expression?)]
	[private-method 															; private method													
		(name symbol?)
		(args (list-of arg?))
		(body expression?)]
	[public-static-method 														; public static method
		(name symbol?)
		(args (list-of arg?))
		(body expression?)]
	[private-static-method 														; private static method
		(name symbol?)
		(args (list-of arg?))
		(body expression?)])

(define arg? 																	; definition of an argument
	(lambda (x)
		(or (null? x)
			(and (symbol? (car x)) (symbol? (cadr x)) (scheme-value? (caddr x))))))

(define typify 																	; Determines the types (public, etc.) of a field
	(lambda (ls) 																; Adds #<void> as a default value if one is not given (causes error in this version)
			(case (car ls)
				[(public)
					(if (equal? (cadr ls) 'static)
						(public-static-var (caddr ls) (cadddr ls) (cond [(null? (cddddr ls)) (void)] [else (car (cddddr ls))]))
						(public-var (cadr ls) (caddr ls) (cond [(null? (cdddr ls)) (void)] [else (cadddr ls)])))]
				[(private)
					(if (equal? (cadr ls) 'static)
						(private-static-var (caddr ls) (cadddr ls) (cond [(null? (cddddr ls)) (void)] [else (car (cddddr ls))]))
						(private-var (cadr ls) (caddr ls) (cond [(null? (cdddr ls)) (void)] [else (cadddr ls)])))]
				[(static)
					(public-static-var (cadr ls) (caddr ls) (cond [(null? (cdddr ls)) (void)] [else (cadddr ls)]))]
				[else (public-var (car ls) (cadr ls) (cond [(null? (cddr ls)) (void)] [else (caddr ls)]))])))

(define method-parse 															; Parses a method, adds default values of #<void> to args if necessary, and
	(lambda (method classname) 													; transforms the method's code into abstract that will use the arguments properly
		(case (car method)
			[(public)
				(if (equal? (cadr method) 'static)
					(let ((args (cadddr method)) (name (caddr method)))
						(public-static-method name (add-defaults args) (se (parse-exp (list 'begin (list 'apply (list 'lambda (if (null? (car args)) '() (map cadr args)) (car (cddddr method))) 'args))))))
					(let ((args (caddr method)) (name (cadr method)))
							(public-method name (add-defaults args) (se (parse-exp (list 'begin (list 'apply (list 'lambda (if (null? (car args)) '() (map cadr args)) (cadddr method)) 'args)))))))]
			[(private)
				(if (equal? (cadr method) 'static)
					(let ((args (cadddr method)) (name (caddr method)))
						(private-static-method name (add-defaults args) (se (parse-exp (list 'begin (list 'apply (list 'lambda (if (null? (car args)) '() (map cadr args)) (car (cddddr method))) 'args))))))
					(let ((args (caddr method)) (name (cadr method)))
							(private-method name (add-defaults args) (se (parse-exp (list 'begin (list 'apply (list 'lambda (if (null? (car args)) '() (map cadr args)) (cadddr method)) 'args)))))))]
			[(static)
					(let ((args (caddr method)))
						(public-static-method (add-defaults args) (se (parse-exp (list 'begin (list 'apply (list 'lambda (if (null? (car args)) '() (map cadr args)) (cadddr method)) 'args))))))]
			[else (let ((args (cadr method)))
							(public-method name (add-defaults args) (se (parse-exp (list 'begin (list 'apply (list 'lambda (if (null? (car args)) '() (map cadr args)) (caddr method)) 'args))))))])))

(define add-defaults 															; Adds default values of #<void> to a list of arguments (causes error in this version)
	(lambda (args)
		(if (null? (car args)) args
			(map (lambda (x) (list (car x) (cadr x) (if (null? (cddr x)) (void) (caddr x)))) args))))	