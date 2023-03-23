#lang racket
;; unordered duplicates list
;; can be use if adjoin needs to be fast O(1)
;; append is linear so union is O(n)
;; Tried to make intersection better for case with lot of duplicates and large n
(define (element-of-set? x myset)
  (cond ((null? myset) false)
        ((equal? x (car myset)) true)
        (else (element-of-set? x (cdr myset)))))

(define (adjoin-set x myset)
  (cons x myset))

(define (union-set set1 set2)
  (append set1 set2))
(union-set (list 1 1 1 1 1 1 1 2 3) (list 2 2 2 2 2 3 'c))

(define (intersection-set set1 set2)
  (define (iter s1 s2 result)
    (cond ((or (null? s1) (null? s2)) result)
          ((and 
             (not (element-of-set? (car s1) result))
             (element-of-set? (car s1) s2))
           (iter (cdr s1) s2 (cons (car s1) result)))
          (else (iter (cdr s1) s2 result))))
  (iter set1 set2 '()))
(intersection-set (list 1 2 2 2 2 2 2 2 3) (list 'a 2 1 1 1 1 1 1 1 1 'c))
