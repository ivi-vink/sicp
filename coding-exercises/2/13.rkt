#lang racket
(require "../../shared/intervals.rkt")

(define (print)
  (let ((i1 (make-center-percent 12.0 5.0))
        (i2 (make-center-percent 12.0 5.0)))
    (print-interval (mul-interval i1 i2))
    (println (percent (mul-interval i1 i2)))))
  
(print)
