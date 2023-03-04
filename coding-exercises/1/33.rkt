#lang racket
(require "../../shared/chapter1.rkt")
(require sicp)

;; sum of squares of the prime numbers in the interval a to b
(define (prime? x)
 (all-miller-raban x))

(define (square-prime-sum a b)
  (filtered-accumulate
    prime?
    (lambda (x y) (+ x y))
    0
    (lambda (x) (* x x))
    a inc b))

(square-prime-sum 2 5)

(define (coprime-product n)
  (filtered-accumulate
    (coprimer n)
    (lambda (x y) (* x y))
    1
    (lambda (x) x)
    1 inc (- n 1)))

(coprime-product 10)
