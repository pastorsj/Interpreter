;;; Jason Lane, A15
;;; Question for #2:
;;; The in-class implementation of fib-memo does not use a hash function and knows whether a value
;;; is in the 'memo' using one comparison to the max value.

;;; Apply-continuation. Used for CPS
(define apply-continuation
  (lambda (k . list-of-values)
    (apply k list-of-values)))

(define a-c apply-continuation)

;;; #1a
(define member?-cps
	(lambda (x ls k)
		(if (null? ls) (apply-continuation k #f)
			(if (equal? x (car ls))
				(apply-continuation k #t)
				(member?-cps x (cdr ls) k)))))

;;; #1b
(define set?-cps
	(lambda (ls k)
		(cond
			[(null? ls) (a-c k #t)]
			[(not (pair? ls)) (a-c k #f)]
			[else (set?-cps
					(cdr ls)
					(lambda (set-cdr)
						(if set-cdr
							(a-c k (not (member?-cps (car ls) (cdr ls) (lambda (v) v))))
							(a-c k #f))))])))

;;; #1c
(define set-of-cps
	(lambda (ls k)
		(if (null? ls) (a-c k ls)
			(set-of-cps (cdr ls)
				(lambda (set-cdr)
					((lambda (member-cdr)
						(a-c k (if member-cdr set-cdr
							(cons (car ls) set-cdr))))
					(member?-cps (car ls) (cdr ls) (lambda (v) v))))))))

(define map-cps
	(lambda (proc ls k)
		(if (null? ls) (a-c k ls)
			(map-cps proc (cdr ls) (lambda (mapped-cdr) (a-c k (cons (proc (car ls) (lambda (v) v)) mapped-cdr)))))))

(define 1st-cps
	(lambda (ls k)
		(a-c k (car ls))))

(define domain-cps
	(lambda (ls k) (a-c k (set-of-cps (map-cps 1st-cps ls (lambda (v) v)) (lambda (v) v)))))

;;; #1d

(define make-cps
	(lambda (proc)
		(lambda (v k) (a-c k (proc v)))))

;;; #1e

(define andmap-cps
	(lambda (proc ls k)
		(if (null? ls) (a-c k #t)
			(proc (car ls) (lambda (v) (if v (andmap-cps proc (cdr ls) k) (a-c k #f)))))))


;;; #1f

(define cps-snlist-recur
	(lambda (base itproc lsproc)
		(letrec
			((helper (lambda (ls k)
				(if (null? ls)
					(a-c k base)
					(let ((c (car ls)))
						(if (list? c)
							(helper c (lambda (recar) (helper (cdr ls) (lambda (recdr) (lsproc recar recdr k)))))
							(helper (cdr ls) (lambda (recdr) (itproc c recdr k))))))))) helper)))

(define append-cps
	(lambda (ls1 ls2 k)
		(if (null? ls1) (a-c k ls2)
			(append-cps (cdr ls1) ls2 (lambda (app-cdr) (a-c k (cons (car ls1) app-cdr)))))))

(define sn-list-reverse-cps
	(cps-snlist-recur '()
		(lambda (x y k) (append-cps y (list x) k))
		(lambda (x y k) (append-cps y (list x) k))))

(define +-cps
	(lambda (x y k)
		(a-c k (+ x y))))

(define max-cps
	(lambda (x y k)
		(a-c k (if (> x y) x y))))

(define sn-list-occur-cps
	(lambda (s ls k)
		((cps-snlist-recur 0
			(lambda (x y k) (+-cps (if (eq? s x) 1 0) y k))
			(lambda (x y k) (+-cps x y k)))
	ls k)))

(define sn-list-depth-cps
	(cps-snlist-recur 1
		(lambda (x y k) (a-c k y))
		(lambda (x y k) (max-cps (+ x 1) y k))))

;;; #2

(define memoize
	(lambda (f hash equiv?)
		(let ((tab (make-hashtable hash equiv?)))
			(lambda args
				(if (hashtable-contains? tab args)
					(hashtable-ref tab args '())
					(let ((res (apply f args)))
						(hashtable-set! tab args res)
						res))))))

;;; #3 helper

(define subst-help
	(lambda (new old sls equality-pred?)
		(let loop((sls sls))
			(cond [(null? sls) (values #f sls)]
				  [(and (symbol? (car sls)) (equality-pred? (car sls) old)) (values #t (cons new (cdr sls)))]
				  [(symbol? (car sls))
				  		(cwv
				  			(lambda () (subst-help new old (cdr sls) equality-pred?))
				  			(lambda (x y) (values x (cons (car sls) y))))]
				  [else (cwv
				  			(lambda () (subst-help new old (car sls) equality-pred?))
				  			(lambda (x y) (if x (values x (cons y (cdr sls)))
				  							(cwv
				  								(lambda () (subst-help new old (cdr sls) equality-pred?))
				  								(lambda (x y) (values x (cons (car sls) y)))))))]))))

(define cwv call-with-values)

;;; #3

(define subst-leftmost
	(lambda (new old slist equality-pred?)
		(if (null? slist)
			slist
			(cwv (lambda () (subst-help new old slist equality-pred?)) (lambda (x y) y)))))