; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map box vals) env)))

(define extend-improper-env
  (lambda (syms vals env)
    (let loop ((i (length syms)) (vals vals) (ls '()))
      (if (= i 1)
	  (extend-env syms (append ls (list vals)) env)
	  (loop (- i 1) (cdr vals) (append ls (list (car vals))))))))

(define list-find-position
  (lambda (sym los k)
    (apply-k k (list-index (lambda (xsym) (eqv? sym xsym)) los))))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env-old
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env env sym succeed fail))))
      (recursively-extended-env-record
       (procnames idss bodies old-env)
       (let ([pos
	      (list-find-position sym procnames)])
	 (if (number? pos)
			(eval-exp (list-ref bodies pos) env)
	    (apply-env old-env sym succeed fail)))))))

(define apply-env
  (lambda (env sym succeed fail k) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record () (fail k))
      (extended-env-record (syms vals env)
        (list-find-position sym syms (apply-extended-k sym vals succeed fail env k)))
	       ;(let ((pos (list-find-position sym syms)))
      	  ; (if (number? pos)
	         ;  (succeed (list-ref vals pos))
	          ; (apply-env env sym succeed fail)))
      (recursively-extended-env-record (procnames idss bodies old-env)
        (list-find-position sym procnames (rec-env-k bodies env old-env sym succeed fail k))))))
       ;(let ([pos
	      ;(list-find-position sym procnames)])
	       ; (if (number? pos)
			    ;  (eval-exp (list-ref bodies pos) env)
	         ; (apply-env old-env sym succeed fail)))))))
