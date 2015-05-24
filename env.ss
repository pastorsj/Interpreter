; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env k)
    (apply-k k (extended-env-record syms (map box vals) env))))


(define extend-improper-env
  (lambda (syms vals env k)
    (let loop ((i (length syms)) (vals vals) (ls '()))
      (if (= i 1)
	  (extend-env syms (append ls (list vals)) env k)
	  (loop (- i 1) (cdr vals) (append ls (list (car vals))))))))

(define list-find-position
  (lambda (sym los k)
    (list-index (lambda (xsym) (eqv? sym xsym)) los k)))


(define list-index
  (lambda (pred ls k)
    (cond
     ((null? ls) (apply-k k #f))
     ((pred (car ls)) (apply-k k 0))
     (else (list-index pred (cdr ls) (list-index-k k))))))

(define apply-env
  (lambda (env sym succeed fail k) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record () (fail k))
      (extended-env-record (syms vals env)
        (list-find-position sym syms (apply-extended-k sym vals succeed fail env k)))
      (recursively-extended-env-record (procnames idss bodies old-env)
        (list-find-position sym procnames (rec-env-k bodies env old-env sym succeed fail k))))))