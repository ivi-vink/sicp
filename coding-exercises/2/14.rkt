#lang racket
(require "../../shared/intervals.rkt")

(define (print-interval-percent i)
  (newline)
  (display "interval{")
  (display (center i))
  (display ",")
  (display (percent i))
  (display "}")
  (newline))

(define (lem1 i1 i2)
  (div-interval (mul-interval i1 i2)
                (add-interval i1 i2)))

(define (lem2 i1 i2)
  (let ((one (make-center-percent 1.0 0)))
    (div-interval 
      one
      (add-interval 
        (div-interval one i1)
        (div-interval one i2)))))

;; adding scales the heighest percent to the new center
(define (print-add)
  (let ((i1 (make-center-percent 100.0 2.0))
        (i2 (make-center-percent 200.0 3.0)))
    (newline)
    (println "*** add")
    (print-interval-percent i1)
    (print-interval-percent i2)
    (print-interval-percent (add-interval i1 i1))
    (print-interval-percent (add-interval i1 i2))
    (print-interval-percent (sub-interval i1 i1))
    (print-interval-percent (sub-interval i1 i2))))

;; multiplication and addition of positive intervals
;; adds percentage from both intervals
(define (print-mul)
  (let ((i1 (make-interval 2.0 8.0))
        (i2 (make-interval 2.0 8.0)))
    (newline)
    (println "*** mul")
    (print-interval-percent i1)
    (print-interval-percent i2)
    (print-interval-percent (div-interval i1 i1))
    (print-interval-percent (div-interval i1 i2))
    (print-interval-percent (mul-interval i1 i1))
    (print-interval-percent (mul-interval i1 i2))))
  
;; There are at least some problems as I understand:
;; 1. Repeated intervals in an equation are dependent on each other
;; 2. Multiplicative identity is undefined and should not be uncertain
(define (print-lem)
  (let ((i1 (make-center-percent 100.0 2.0))
        (i2 (make-center-percent 200.0 3.0)))
    (newline)
    (println "*** lem")
    (print-interval-percent i1)
    (print-interval-percent i2)
    (print-interval-percent (lem1 i1 i2))
    (print-interval-percent (lem2 i1 i2))))
(print-add)
(print-mul)
(print-lem)
