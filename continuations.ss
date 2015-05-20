(define apply-k
  (lambda (k v)
    (cases continuation k
      [init-k () v]
      [if-k (env conds k)
      	(if v 
      		(eval-exp (car conds) env k)
      		(if (null? (cdr conds))
      			void
      			(eval-exp (cadr conds) env k)))]
      [app-k (rands env k)
      	(eval-rands rands env (app-rands-k v env k))]
      [app-rands-k (proc env k)
      	(cases proc proc-value
              [clos-proc (vars body env2) (apply-proc proc-value args env k)]
              [else (apply-proc proc-value (eval-rands args env (init-k)) env k)])]
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
      [ set-replace-both-k (res k)
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
      [else v])))

(define map-cps
	(lambda (x ls k)
		(apply-k k (map x ls))))