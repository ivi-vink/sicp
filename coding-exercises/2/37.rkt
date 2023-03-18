#lang racket
(require "../../shared/lists.rkt")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m-row)
         (accumulate + 0 (map * m-row v))) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-col)
                  (dot-product m-row n-col)) cols)) m)))

(define test-m (list (list 1 2 3 4)
                     (list 4 5 6 6)
                     (list 6 7 8 9)))
(define test-n (list (list 1 2 3)
                     (list 4 5 6)
                     (list 6 7 8)
                     (list 6 7 8)))
(define test-v (list 1 2 3 4))
((lambda ()
   (newline)
   (display (matrix-*-vector test-m test-v))
   (newline)
   (display (transpose test-m))
   (newline)
   (display (matrix-*-matrix test-m test-n))))
