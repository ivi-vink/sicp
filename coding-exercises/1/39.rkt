#lang racket
(require sicp)

(define (odd? i)
  (= 1 (remainder i 2)))

;; iter up
(define (cont-frac n d k)
  (define (iter result i)
   (if (= i 0)
     result
     (iter
      (/ (n i) (+ (d i) result))
      (dec i))))
  (iter 0 k))

;; test
(define (tan-cf x k)
    (cont-frac
      (lambda (i) 
        (cond
          ((= i 1) x)
          ((> i 1) (* -1 (* x x)))
          (else (error "invalid argument to cont-frac"))))
      (lambda (i) (- (* 2 i) 1))
      k))

(tan-cf 2.0 10)
