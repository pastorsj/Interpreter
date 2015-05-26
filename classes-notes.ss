(define vector-list
	(lambda (v)
 		(letrec ((v2 (vec-copy v (make-vector (vector-length v)))) (size (vector-length v)) (capacity (vector-length v))
 			(this (lambda (msg . args)
 				(case msg ; Scheme's case is a similar to switch in some other languages.
 					[(empty?) (eqv? (vector) v2)]
 					[(get) (vector-ref v2 (car args))]
 					[(set) (vector-set! v2 (car args) (cadr args))]
 					[(add) (if (<= (this 'capacity) size)
 								(begin 	(set! v2 (vec-copy v2 (make-vector (* 2 (vector-length v2)))))
 										(vector-set! v2 size (car args))
 										(set! size (+ size 1))
 										(set! capacity (vector-length v2)))
 								(begin
 										(vector-set! v2 size (car args))
 										(set! size (+ size 1))))]
 					[(remove) (begin (set! size (- size 1)) (vector-ref v2 size))]
 					[(capacity) capacity]
 					[(size) size]
 					[else (errorf 'vector-list "illegal message to vector-list object: ~a" msg)])))) this)))

(define make-class ;;;psuedocode
	(lambda (name fields methods)
		(define name
			(lambda (constr-values-from-methods)
				(letrec ((field field-from-constructor)
					(this (lambda msg.args)
						(case msg
							[(method-name-from-methods) (method-code-from-methods)]
							[...]
							[...]
							[(public-field-name) public-field-name] ;;;auto-gen
							[else (errorf name "illegal message to instance ~a" msg)]
							))))))))

class foo
 public static int x = 1
 private int y
 private int z
 public foo(int y)
   y = y
 public foo()
   y = 2
 public static inc()
   y = y + x
   return y
 private test()
   print("bar")

(class 'foo
	(('public 'static integer? 'x 1) ('private integer? y))
	(('public 'foo ((integer? 'y))
		((set! ('this y) y)))
	 ('public 'foo (())
	 	((set! ('this y) 2)))
	 ('public 'static 'inc (())
	 	((set! ('this y) (+ ('this y) ('this x)))))
	 ('private 'test (())
	 	((display ("bar"))))))

(class-exp foo
	((public-static-var integer? x 1) (private-var integer? y (void)))
	((public-constr-method foo ((integer? y (void)))
		(begin-exp
			(app-exp ((set-exp temp (constr-exp foo)))) (app-exp ((set-exp (app-exp ((var-exp temp) (lit-exp y))) (var-exp y))) (var-exp temp)))))
	((public-constr-method foo (())
		(begin-exp
			(app-exp ((set-exp temp (constr-exp foo)))) (app-exp ((set-exp (app-exp ((var-exp temp) (lit-exp y))) (lit-exp 2)))) (var-exp temp))))
	((public-static-method inc (())
		(begin-exp
			(app-exp ((set-exp (this y) (app-exp ((var-exp +) (app-exp ((var-exp this) (var-exp y))) (app-exp ((var-exp this) (var-exp x))))))))
	((private-method test (())
		(begin-exp
			(app-exp ((var-exp display) (lit-exp "bar")))))))