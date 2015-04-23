[let-exp (vars exp bodies)
	 (let ([new-env (extend-env vars
				    (eval-rands exps env)
				    env)])
	   (eval-bodies bodies new-env))]

[if-exp (test then else)
	(if (eval-exp id env)
	    (eval-exp true env)
	    (eval-exp false env))]

[lambda-exp (id body)
	    (closure id body env)]