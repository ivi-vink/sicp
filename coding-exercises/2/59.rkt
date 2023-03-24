#lang racket
;; unordered distinct list
(define (element-of-set? x myset)
  (cond ((null? myset) false)
        ((equal? x (car myset)) true)
        (else (element-of-set? x (cdr myset)))))

(define (adjoin-set x myset)
  (if (element-of-set? x myset)
    myset
    (cons x myset)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
(intersection-set (list 1 2 3) (list 'a 2 'c))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))
(union-set (list 1 2 3) (list 2 3 'c))
