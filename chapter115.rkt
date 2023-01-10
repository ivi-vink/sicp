#lang sicp
(define counter (box 0))

(define (cube x) (* x x x))
(define (p x) 
  (set-box! counter (+ (unbox counter) 1))
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3)))))
(sine 1250.0)
(println "\n")
(println (unbox counter))
