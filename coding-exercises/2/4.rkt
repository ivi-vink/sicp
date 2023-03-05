#lang racket
(define (cons x y)
  (lambda (m) (m x y)))

(define (car r)
  (r (lambda (p q) p)))

(define (cdr r)
  (r (lambda (p q) q)))

(define test-cons (cons 0 1))
(car test-cons)
(cdr test-cons)
