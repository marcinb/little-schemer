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

(define sub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
	(sub1 (sub n (sub1 m)))))))
