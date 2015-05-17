; This code is to be used for problem 2.

(define cps-snlist-recur
  (lambda (seed item-proc-cps list-proc-cps)
    (letrec
	([helper
	  (lambda (ls k)
	    (if (null? ls)
		(k seed)
		(let ([c (car ls)])
		  (if (or (pair? c)
			  (null? c))
		      (helper c
			      (lambda (car-v)
				(helper (cdr ls)
					(lambda (cdr-v)
					  (list-proc-cps car-v
							 cdr-v
							 k)))))
		      (helper (cdr ls)
			      (lambda (cdr-v)
				(item-proc-cps c cdr-v k)))))))])
      helper)))


; Test cases for the various problems (same as on the server):

;1

(split-cps '(a) (lambda (v) v)) ; answer ((a) ())
(split-cps '() (lambda (v) v)) ; answer (() ())
(split-cps '(a b c d e) cadr) ; answer (b d)
(split-cps '(a b c d e f) (lambda (v) (map (lambda (x) (list x x)) v))) 
  ; answer (((a c e) (a c e)) ((b d f) (b d f)))
(split-cps '(a b c d e f) reverse) ; answer ((b d f) (a c e))


; 2

(sn-list-flatten-cps '() (lambda (v) v)) ; answer ()
(sn-list-flatten-cps '(a) list)  ; answer ((a))
(sn-list-flatten-cps '((b) () (c (d e)) a) cdr)  ; answer (c d e a)
(sn-list-flatten-cps '((b) () ((c (d e) () f) a) ()) reverse) ; answer (a f e d c b)

; #3a

(eval-one-exp '
(let ([v (vector 0 0 0 0 0 0 0)])
  (for 5 (lambda (t) (vector-set! v (+ 1 t) 
				  (+ (vector-ref v t)
				     t))))
  v)) ; answer #(0 0 1 3 6 10 15)

(eval-one-exp '
(let ([v (vector 2 3 4 5 6 7 8)])
  (for 4 8 (lambda (t) (vector-set! v (- t 4) (+ (vector-ref v  (- t 3)) t))))
  v)
)  ; answer #(7 9 11 13 15 7 8)

(eval-one-exp '
(let ([v (let ([v (vector 2 3 4 5 6 7 8)])
	   (for 4 8 (lambda (t) (vector-set! v (- t 4) (+ (vector-ref v  (- t 3)) t))))
	   v)])
  (vector-set! v 3 100)
  v)
) ; answer #(7 9 11 100 15 7 8)



; #3b

(eval-one-exp '
(let ([f1 (case-lambda
	  [(x) (+ 2 x)]
	  [(x y) (* x y)]
	  )])
  (list (f1 4) (f1 5 6)))
) ; answer (6 30)

(eval-one-exp '
(letrec ([f1 (case-lambda
	      [(x) (+ 2 x)]
	      [(x y) (* x (f1 y))]
	      [(a b c) (f1 a (f1 b (f1 c)))]
	      )])
  (list (f1 4) (f1 3 4) (f1 2 3 4)))
) ; answer (6 18 52)

(eval-one-exp '
(let ([z 5])
  (letrec ([f1 (case-lambda
		[() (or (< z 3) (+ z 7))]
		[(x y z w v) 5 4]
		[(a b c) (let ([z 3]) (+ z z))])])
    (list (f1) (f1 0 0 0 0 0) (f1 2 3 4))))
) ; answer (12 4 6)






