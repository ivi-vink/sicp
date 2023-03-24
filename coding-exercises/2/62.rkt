#lang racket

;; In each branch of the problem we either terminate the process or we reduce the problem to a subproblem with set - (car set)
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) (cons (car set2) (union-set set1 (cdr set2))))
        ((null? set2) (cons (car set1) (union-set (cdr set1) set2)))
        ((= (car set1) (car set2)) (cons (car set2) (union-set (cdr set1) (cdr set2))))
        ((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))))

(define test-list (list 1 2))
(define test-list2 (list 4 5 6 7))
(union-set test-list test-list2)
  
