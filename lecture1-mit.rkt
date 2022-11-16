#lang sicp
(inc 487)
nil

the-empty-stream

(runtime)
(+ 3 (* 5 6) 8 2)

(define A (* 5 5))

(* A A)

(define B (+ A (* 5 A)))
B

;; is basically the same
(define (square x) (* x x))
(define square (lambda (x) (* x x)))
