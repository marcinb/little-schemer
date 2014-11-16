;; Takes number as an argument.
;; Returns function that takes other number as an argument.
;; The returned fuction adds both numbers.
(define make-adder
  (lambda (x)
    (lambda (y)
      (+ x y))))

;; Takes functions `f` ang `g` as arguments.
;; Returns a function which takes any S-expression `x` as argument
;; the returned fuction applies function `g` to argument `x` and then applies `f` to result.
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define add10
  (lambda (x)
    ((compose (make-adder 5) (make-adder 5)) x)))

