#lang racket

;; Accumulates all combinations with the car element and without the car element in a list
;; The appends:
;; 1. (()) + ((3))
;; 2. (() (3)) + ((2) (2 3))
;; ...
;;
;; Lisp-wise it works out because the empty set follows from these rules when the input set is empty,
;; so every level introduces the list with the car element itself and combinations with all previous combinations
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x)
                          (cons (car s) x)) rest)))))
((lambda ()
   (display (subsets (list 1 2 3)))))

(println (cons 2 '()))
(println (list 2))
