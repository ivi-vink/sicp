#lang racket
(require "../../shared/chapter1.rkt")
(require sicp)

(define deriv (deriver 0.00001))

(define (newtons-method g guess)
  (define (newton-transform f)
    (lambda (x)
      (- x (/ (f x) ((deriv f) x)))))

  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(define (cube-root a b c guess)
  (newtons-method (cubic a b c) guess))

(cube-root 1 1 1 1)
