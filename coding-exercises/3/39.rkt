#lang racket

(define x 10)
(define (parallel-execute (lambda () (set! x (* x x)))))
