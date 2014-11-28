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

;; Takes a list of non negative numbers as an argument.
;; Returns number being sum of all list items.
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else 
	(add (car tup) (addtup (cdr tup)))))))

;; Takes two lists of non negative numbers as arguments.
;; Returns list of numbers being sum of corresponding numbers from given lists.
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons 
	      (add (car tup1) (car tup2)) 
	      (tup+ (cdr tup1) (cdr tup2)))))))

;; Takes two non negative numbers as arguments.
;; Returns #t if first number is greater than secong one,
;; returns #f otherwise.
(define greater?
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
	(greater? (sub1 n) (sub1 m))))))

;; Takes two non negative numbers as arguments.
;; Returns #t if first number is lower than secong one,
;; returns #f otherwise.
(define lower?
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
	(lower? (sub1 n) (sub1 m))))))

;; Compares values of two numbers.
;; Returns #t if both are equal, returns #f otherwise.
(define eql?
  (lambda (n m)
    (cond
      ((greater? n m) #f)
      ((lower? n m) #f)
      (else #t))))

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
