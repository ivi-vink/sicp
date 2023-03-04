#lang racket
(require "../../shared/chapter1.rkt")
(require sicp)

(golden-ratio)

(define (log1000)
  (fixed-point
    (lambda (x)
      (println x)
      (/ (log 1000) (log x)))
    2.0))

(define (log1000-average-damped)
  (fixed-point
    (average-damp
      (lambda (x)
        (println x)
        (/ (log 1000) (log x))) 1)
    2.0))

(log1000)
(println " *** second time")
(log1000-average-damped)
