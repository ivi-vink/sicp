#lang racket
(require "../../shared/chapter1.rkt")
(require sicp)

(define (root-finder x n damp-fold)
  (fixed-point
   (average-damp
     (lambda (y) (/ x (power y (- n 1))))
     damp-fold)
   1.0))

(root-finder 2 64 6)
