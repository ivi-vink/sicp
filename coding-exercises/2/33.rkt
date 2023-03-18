#lang racket
(require "../../shared/lists.rkt")

(define (map p sequence)
  (accumulate
    (lambda (x y)
      (cons (p x) y))
    '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y)) 0 sequence))

(define test-seq (list 1 2 3 4 5 6 7 8 9 10))
((lambda ()
   (display (map (lambda (x) (* x x)) test-seq))
   (newline)
   (display (append test-seq test-seq))
   (newline)
   (display (length test-seq))))
