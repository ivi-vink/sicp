#lang racket
(require sicp)

(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))
