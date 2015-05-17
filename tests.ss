(define load-all-tests
	(lambda ()
		(begin
		(load "Assignment_13/A13-test-code.ss")
		(load "Assignment_14/A14-test-code.ss")
		(load "Assignment_16/A16-test-code.ss")
		(load "Assignment_17/A17-test-code.ss")
		(run-all13)
		(run-all14)
		(run-all16)
		(run-all17))))

(define lt load-all-tests)

(lt)

(define r
	(lambda ()
		(begin
			(run-all13)
			(run-all14)
			(run-all16)
			(run-all17))))