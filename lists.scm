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

;; Takes atom and list as arguments.
;; Returns new list being a copy of given list with
;; the first occurence of given atom removed.
(define rember
  (lambda (e lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? e (car lat)) (cdr lat))
      (else
        (cons (car lat) (rember e (cdr lat)))))))

;; Takes list of lists as an argument
;; Returns a list of first S-expression of each internal list.
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else
        (cons (car (car l)) (firsts (cdr l)))))))

;; Takes list of lists as an argument
;; Returns a list of 2nd S-expression of each internal list.
(define seconds
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else
        (cons (car (cdr (car l))) (seconds (cdr l)))))))

;; Takes three arguments: atoms `new` and `old` and a list of atoms.
;; Returns a copy of given list with `new` atom inserted to the right
;; of the first occurence of `old` in the original list.
(define insert-r
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons old (cons new (cdr lat))))
      (else
        (cons (car lat) (insert-r new old (cdr lat)))))))

;; Takes three arguments: atoms `new` and `old` and a list of atoms.
;; Returns a copy of given list with `new` atom inserted to the left
;; of the first occurence of `old` in the original list.
(define insert-l
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new lat))
      (else
        (cons (car lat) (insert-l new old (cdr lat)))))))

;; Takes three arguments: atoms `new` and `old` and a list of atoms.
;; Returns a copy of given list with `old` atom removed and `new` atom
;; inserted in it's place.
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else
        (cons (car lat) (subst new old (cdr lat)))))))

;; Takes four arguments: atoms `new`, `o1` and `o2` and a list of atoms.
;; Returns a copy of given list withe the first occurence of either `o1` or `o2` removed
;; and `new` atom; inserted in it's place.
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or
         (eq? (car lat) o1)
         (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else
        (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

;; Takes atom and list as arguments.
;; Returns new list being a copy of given list with
;; all occurences of given atom removed.
(define multirember
  (lambda (e lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) e) (multirember e (cdr lat)))
      (else
        (cons (car lat) (multirember e (cdr lat)))))))

;; Takes three arguments: atoms `new` and `old` and a list of atoms.
;; Returns a copy of given list with `new` atom inserted to the right
;; of every occurence of `old`
(define multiinsert-r
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsert-r new old (cdr lat)))))
      (else
        (cons (car lat)
              (multiinsert-r new old (cdr lat)))))))

;; Takes three arguments: atoms `new` and `old` and a list of atoms.
;; Returns a copy of given list with all occurences of `old` atom removed and `new` atom
;; inserted in their place.
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else
        (cons (car lat) (multisubst new old (cdr lat)))))))
