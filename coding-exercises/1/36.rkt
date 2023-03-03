#lang racket
(require sicp)

(define (fixed-point damper f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.0001))
  (define (try guess)
    (let ((next (damper guess (f guess))))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-ratio)
  (fixed-point 
    (lambda (x) (+ 1 (/ 1 x)))
    1.0))

(define (log1000)
  (fixed-point
    (lambda (guess next)
      next)
    (lambda (x) (/ (log 1000) (log x)))
    2.0))

(define (log1000-average-damped)
  (define (average a b)
    (/ (+ a b) 2))
  (fixed-point
    (lambda (guess next)
      (average guess next))
    (lambda (x) (/ (log 1000) (log x)))
    2.0))

(log1000)
(println " *** second time")
(log1000-average-damped)
