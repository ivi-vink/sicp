#lang racket
;; import miller raban prime test
(load "coding-exercises/1/28.rkt")

;; sum of squares of the prime numbers in the interval a to b
(define (prime? x)
 (all-mr x))

(define (filtered-accumulate pred combiner null-value term a next b)
  (define (iter a result)
    (cond
      ((> a b) result)
      ((pred a) (iter
                   (next a)
                   (combiner
                     (term a)
                     result)))
      (else (iter (next a) result))))
  (iter a null-value))

(define (square-prime-sum a b)
  (filtered-accumulate
    prime?
    (lambda (x y) (+ x y))
    0
    (lambda (x) (* x x))
    a inc b))

(square-prime-sum 2 5)

(define (coprime-product n)
  (define (coprime? a)
     (if (= 1 (gcd a n))
       ((lambda ()
          (println a)
          true))
       false))
  (filtered-accumulate
    coprime?
    (lambda (x y) (* x y))
    1
    (lambda (x) x)
    1 inc (- n 1)))

(coprime-product 10)
