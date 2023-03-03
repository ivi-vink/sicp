#lang racket
(require sicp)

(define (square x)
  (* x x))

(define (signal b e m x)
  (cond
    ((= e (- m 1)) x) ;; end result
    ((= b (- m 1)) x) ;; base squared wil result in trivial root
    ((= x 1) 0) ;; non-trivial root
    (else x))) ;; no root found

(define (expmod base e m)
  (cond
    ((= e 0) 1)
    ((even? e)
     (signal base e m (remainder (square (expmod base (/ e 2) m)) m)))
    (else
      (remainder (* base (expmod base (- e 1) m)) m))))

(define (mr a n)
  (= (expmod a (- n 1) n) 1))

(define (all-mr n)
  (define (f a n)
      (cond
        ((<= a 2) true)
        ((mr (- a 1) n) (f (- a 1) n))
        (else false)))
  (f n n))

(all-mr 5)
