#lang racket
(require "../../shared/chapter1.rkt")
(require sicp)

(define (smallest-divisor n)
  (find-divisor n 2))

;; (pretty-print (smallest-divisor 199))
;; (pretty-print (smallest-divisor 1999))
;; (pretty-print (smallest-divisor 19999))
