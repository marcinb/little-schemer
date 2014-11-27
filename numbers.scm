(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
	(add1 (add n (sub1 m)))))))
