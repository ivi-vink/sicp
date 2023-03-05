#lang racket
(provide
  sub-interval)
(require "7.rkt")

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (print)
  (define a (make-interval 1 2))
  (define b (make-interval 0 2))
  (print-interval (sub-interval a b)))
(print)
