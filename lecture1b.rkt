#lang sicp
(define (p)
  (p))
(define (test x y)
  (if (= 0 x) 0 y))
;; (test 0 (p))

(define (sqrt-iter guess prev x)
  (if (good-enough? guess prev)
    guess
    (sqrt-iter (improve guess x) guess x)))

(define (abs x)
  (if (> x 0) x (- x)))

(define (good-enough? guess prev)
  (and 
    (> prev 0) 
    (< (abs (- 1 (/ guess prev))) 0.001)))

(define (improve guess x)
  (/ (+ (/ x guess) guess) 2))

(sqrt-iter 1 0 9.0)

(define (cube-improve guess x) 
  (/ 
    (+ (/ x (* guess guess)) (* 2 guess))
    3))
    

(define (cuberoot-iter guess prev x)
  (if (good-enough? guess prev)
    guess
    (cuberoot-iter (cube-improve guess x) guess x)))

(cuberoot-iter 1 0 8.0)
