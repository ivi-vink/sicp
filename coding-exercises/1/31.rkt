#lang racket
(require sicp)

(define (id x) x)

(define (factorial n)
  (product id 1 inc n))

(define (product term a next b)
  (define (iter result a)
    (if (> a b)
      result
      (iter (* (term a) result) (next a))))
  (iter 1 a))

(define (even? x)
  (= (remainder x 2) 0))

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
