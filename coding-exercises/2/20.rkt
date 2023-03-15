#lang racket
(require sicp)

(define (f x y . z) (+ x y (car z)))

(define (same-parity n . o)
  (define p (remainder n 2))
  (define (rec others)
    (cond ((null? others) others)
          ((= (remainder (car others) 2) p) (cons (car others) (rec (cdr others))))
          (else  (rec (cdr others)))))
  (cons n (rec o)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
