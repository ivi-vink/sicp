#lang racket
(require "../../shared/chapter1.rkt")
(require sicp)

(define (cube x) (* x x x))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.0001)
(println "")
(simpson cube 0 1.0 500)
