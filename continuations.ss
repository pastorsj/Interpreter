(define apply-k
  (lambda (k v)
    (cases continuation k
      [init-k () v]
      [else v])))