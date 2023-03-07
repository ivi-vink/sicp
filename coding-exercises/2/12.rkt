#lang racket
(require "../../shared/intervals.rkt")

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (let ((toler (* c (/ p 100.0)))) 
    ((lambda (a b) 
       (if (> a b) 
         (make-interval b a)
         (make-interval a b)))
     (- c toler) 
     (+ c toler))))

(define (percent i)
  (abs (* 100 (/ (width i) (center i)))))

(define (print)
 (define t (make-center-width 10.0 6.1))
 (define tp (make-center-percent -10.0 50.0))
 (print-interval t)
 (print-interval tp)
 (println (percent t))
 (println (percent tp)))
(print)
