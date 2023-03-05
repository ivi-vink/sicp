#lang racket

(define (make-rat n d)
  (define (sign x)
    (cond
      ((and (< x 0) (< d 0)) (* -1 x))
      ((and (< 0 x) (< d 0)) (* -1 x))
      (else x)))
  (let ((g (gcd n d)))
    (cons (sign (/ n g)) (abs (/ d g)))))
