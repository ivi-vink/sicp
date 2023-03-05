#lang racket
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-church f g)
  (lambda (h) (lambda (x) ((g h) ((f h) x)))))

(define (print)
    (println (((add-1 zero) inc) 1))
    (println ((one inc) 1))

    (newline)
    (println (((add-1 (add-1 zero)) inc) 1))
    (println ((two inc) 1))

    (newline)
    (println (((add-church one two) inc) 1)))
    
(print)
