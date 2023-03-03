#lang racket
(require sicp)

(define (id x) x)

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (simpson f lower upper n)
  (define (h)
    (/ (- upper lower) n))
  (define (term a)
    (cond
      ((= a 0) (f a))
      ((= a upper) (f a))
      ((even? a) (* 2 (f a)))
      (else (* 4 (f a)))))

  (* (/ (h) 3.0)
     (sum term lower inc upper)))

(define (cube x) (* x x x))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 100)
(println "")
(simpson cube 0 1 100)
(println "")
(simpson cube 0 1 1000)
