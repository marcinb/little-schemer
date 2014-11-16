;; Takes any S-expression as an argument.
;; Answers #t if given expression is an atom,
;; answers #f otherwise.
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Takes list as an agrument.
;; Answers #t if all elements in the list are atoms,
;; answers #f otherwise.
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

;; Takes an atom and a list of atoms as arguments.
;; Answers #t if given atom is present in the list,
;; answers #f othewrise.
(define member?
  (lambda (e lat)
    (cond
      ((null? lat) #f)
      (else (or
              (eq? e (car lat))
              (member? e (cdr lat)))))))

;; Takes number as an argument.
;; Returns function that takes other number as an argument.
;; The returned fuction adds both numbers.
(define make-adder
  (lambda (x)
    (lambda (y)
      (+ x y))))
