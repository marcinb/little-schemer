;; Increments given number by 1.
(define add1
  (lambda (n)
    (+ n 1)))

;; Decrements given number by 1.
(define sub1
  (lambda (n)
    (- n 1)))

;; Adds two non negative numbers.
(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
	(add1 (add n (sub1 m)))))))

;; Subtracts two non negative numbers.
(define sub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
	(sub1 (sub n (sub1 m)))))))
