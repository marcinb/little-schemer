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

;; Multiplies two non negative numbers.
(define mult
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
	(add n (mult n (sub1 m)))))))

;; Takes two non negative numbers as arguments.
;; Returns first number taken to the power of second one
(define pow
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else 
	(mult n (pow n (sub1 m)))))))

;; Divides two non negative natural numbers
(define div
  (lambda (n m)
    (cond
      ((lower? n m) 0)
      (else
	(add1 (div (sub n m) m))))))
