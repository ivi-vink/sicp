#lang racket
(require sicp)

;; recurse down
(define (cont-frac n d k)
  (define (recurse i)
   (if (> i k)
     0
     (/ (n i) (+ (d i) (recurse (inc i))))))
 (recurse 1))

;; iter up
(define (cont-frac n d k)
  (define (iter result i)
   (if (= i 0)
     result
     (iter
      (/ (n i) (+ (d i) result))
      (dec i))))
  (iter 0 k))

;; test
(cont-frac
  (lambda (i) 1.0)
  (lambda (i) 1.0)
  11.0)
