#lang racket
(require "../../shared/chapter1.rkt")
(require sicp)


(define smooth (smoother 0.00001))

((repeated square 2) 5)
((smooth square) 5)
(((repeated smooth 3) square) 5)
