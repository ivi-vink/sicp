#lang racket
(require "../../shared/chapter1.rkt")
(require sicp)

(define (sqrt x)
  ((iterative-improve
     (close-enough? 0.0001)
     (lambda (g)
       (/ (+ g (/ x g)) 2)))
   1.0))

(sqrt 2)

(define (fixed-point f first-guess)
  ((iterative-improve
     (close-enough? 0.0001)
     f)
   first-guess))

(define (root-finder x n damp-fold)
  (fixed-point
   (average-damp
     (lambda (y) (/ x (power y (- n 1))))
     damp-fold)
   1.0))

(root-finder 2 64 6)
