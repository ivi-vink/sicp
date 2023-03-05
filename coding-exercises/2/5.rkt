#lang racket
(require sicp)
(require "chapter1.rkt")

(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (cdr i)
  (define (iter n x)
    (if (divides? 3 n)
      (iter (/ n 3) (inc x))
      x))
  (iter i 0))

(define (car i)
  (define (iter n x)
    (if (divides? 2 n)
      (iter (/ n 2) (inc x))
      x))
  (iter i 0))


(define test-cons (cons 5 7))
(cdr test-cons)
(car test-cons)
