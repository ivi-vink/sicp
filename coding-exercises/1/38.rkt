#lang racket
(require sicp)

;; recurse down
(define (cont-frac n d k)
  (define (recurse i)
   (if (> i k)
     0
     (/ (n i) (+ (d i) (recurse (inc i))))))
 (recurse 1))

(define (euler-d i)
  (let ((next3 (+ i 1)))
   (if (= 0 (remainder next3 3))
      (* 2 (/ next3 3))
      1)))

;; test
(+ 2 (cont-frac
       (lambda (i) 1.0)
       euler-d
       16.0))
