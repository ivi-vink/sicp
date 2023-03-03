#lang racket
(require sicp)

(define (square x)
  (* x x))

(define (expmod base e m)
  (cond 
    ((= e 0) 1)
    ((even? e)
     (remainder (square (expmod base (/ e 2) m)) m))
    (else
      (remainder (* base (expmod base (- e 1) m)) m))))

(define (fermat? a n)
  (= (expmod a n n) a))

(define (all-fermat n)
  (define (f a n)
      (cond
        ((= a 0) true)
        ((fermat? (- a 1) n) (f (- a 1) n))
        (else false)))
  (f n n))
