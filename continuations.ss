(define apply-k
  (lambda (k v)
    (cases continuation k
      [init-k () v]
      [recursive-extend-k (idss env k)
        (apply-k k (recursively-extended-env-record (car v) idss (cadr v) env))]
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
      [else v])))
