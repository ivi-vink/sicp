#lang racket
(require "../../shared/lists.rkt")

(define (reverse-r sequence)
  (fold-right (lambda (x y)
                (append y (list x)))
              '()
              sequence))

(define (reverse-l sequence)
  (fold-left (lambda (y x) 
               (cons x y))
             '() 
             sequence))

(define test-list (list 1 2 3))
((lambda ()
   (display (reverse-r test-list))
   (display (reverse-l test-list))))
