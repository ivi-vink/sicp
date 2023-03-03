#lang racket
(require sicp)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.0001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-ratio)
  (fixed-point 
    (lambda (x) (+ 1 (/ 1 x)))
    1.0))

(golden-ratio)
(/ (log 10) (log 2))
