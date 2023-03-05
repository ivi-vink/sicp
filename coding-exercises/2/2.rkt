#lang racket
(require "../../shared/chapter1.rkt")

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (make-point 36 7))

(define (midpoint-segment s)
  (make-point 
    (average (x-point (start-segment s)) (x-point (end-segment s))) 
    (average (y-point (start-segment s)) (y-point (end-segment s))))) 

(define start (make-point 36 7))
(define end (make-point 36 9))
(define line (make-segment start end))

(define mid (midpoint-segment line))
(print-point mid)
