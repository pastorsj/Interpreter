(define apply-k
  (lambda (k v)
    (cases continuation k
      [init-k () v]
      [letrec-k (lbody k)
      	(eval-bodies lbody v k)]
      [eval-rands-k (done rem env k)
      	(if (null? rem) (apply-k k (append done (list v)))
      		(eval-exp (car rem) env (eval-rands-k (append done (list v)) (cdr rem) env k)))]
      [bodies-k (bodies env k)
      	(eval-bodies bodies env k)]
      [bodies-env-k (bodies k)
      	(eval-bodies bodies v k)]
      [if-k (env conds k)
      	(if v
      		(eval-exp (car conds) env k)
      		(if (null? (cdr conds))
      			(apply-k k '())
      			(eval-exp (cadr conds) env k)))]
      [app-k (rands env k)
      	(cases proc-val v
      		[clos-proc (vars body env2) (apply-proc v rands env k)]
      		[else (eval-rands rands env (app-rands-k v env k))])]
      [app-rands-k (proc env k)
            (apply-proc proc v env k)]
      [rands-ref-k (arg k)
      	(apply-k k (cons arg v))]
      [recursive-extend-k (idss env k)
        (apply-k k (recursively-extended-env-record (car v) idss (cadr v) env))]
      [map-k (args env k)
      	(map (lambda (x) (apply-proc (1st args) x env k)) v)]
      [member-k (ls env k)
      	(map-cps (lambda (x) (eval-exp x env (init-k))) ls (member-help-k v k))]
      [member-help-k (item k)
      	(apply-k k (member item v))]
      [set-replace-body-k (var body arg k)
        (replace-help var body arg (set-replace-both-k v k))]
      [set-replace-both-k (res k)
        (apply-k k (set-exp res v))]
      [replace-refs-k (vars args k)
        (if (symbol? (car vars))
          (apply-k k (list (cons (car vars) (car v)) (cadr v)))
          (replace-help (car vars) (cadr v) (car args) (ref-help-k v k)))]
      [ref-help-k (res k)
        (apply-k k (list (car res) v))]
      [lambda-ref-k (id k)
      	(apply-k k (lambda-exp id v))]
      [app-ref-k (k)
      	(apply-k k (app-exp v))]
      [replace-help-k (var exp arg k)
      	(replace-help var exp arg (replace-please-stop-k v k))]
      [replace-please-stop-k (res k)
      	(apply-k k (cons v res))]
      [extend-help-define-k (id env k)
        (extend-env (list id) (list v) env (set-define-k env k))]
      [set-k (x k)
      	(apply-k k (set-box! x v))]
      [set-define-k (env k)
        (apply-k k (set! global-env v))]
      [apply-extended-k (sym vals succeed fail env k)
        (if (number? v)
          (succeed (list-ref vals v) k)
          (apply-env env sym succeed fail k))]
      [rec-env-k (bodies env old-env sym succeed fail k)
        (if (number? v)
          (eval-exp (list-ref bodies v) env k)
          (apply-env old-env sym succeed fail k))]
      [while-if-k (body test env k)
      	(if v
		    (eval-bodies body env (while-k body test env k))
		    (apply-k k '()))]
      [while-k (body test env k)
      	(eval-exp test env (while-if-k body test env k))]
      [clos-ref-k (vars args env env2 body k)
      	(if ((list-of expression?) args)
      		(eval-rands args env2 (ref2-k v vars body env env2 k))
      		(eval-rands-ref vars args (ref3-k v body env env2 k)))]
      [ref2-k (res vars body env env2 k)
      	(eval-rands-ref vars v (ref3-k res body env env2 k))]
      [ref3-k (res body env env2 k)
      	(extend-env (car res) v (if (equal? (cadr res) body) env env2) (bodies-env-k (cadr res) k))]



      [else v])))

(define map-cps
	(lambda (x ls k)
		(apply-k k (map x ls))))
