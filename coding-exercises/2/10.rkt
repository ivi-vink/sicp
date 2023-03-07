#lang racket
(require "../../shared/intervals.rkt")

(define (div-interval x y)
  (if (and (< 0 (lower-bound y)) (> 0 (upper-bound y)))
    (error "Division by interval spanning zero")
    (mul-interval 
      x
      (make-interval 
        (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y))))))
