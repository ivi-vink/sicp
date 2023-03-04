#lang racket
(require "../../shared/chapter1.rkt")
(require sicp)

(define (factorial n)
  (product (lambda (x) x) 1 inc n))

(define (pi-product n)
  (/ (product (lambda (x)
                (if (even? x)
                  x
                  (+ x 1)))
              2 inc n)
     (product (lambda (x)
                (if (even? x)
                  (+ x 1)
                  x))
              2 inc n)))

(* 1.0 (pi-product 1000))
