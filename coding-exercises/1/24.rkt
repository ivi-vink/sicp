#lang racket
(require "../../shared/chapter1.rkt")
(require sicp)

(define expmod (expmoder (lambda (b e m x) x)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

(define (report-prime n t duration)
  (print n)
  (println " *** prime")
  (println duration)
  t)

(define (start-prime-test n start-time)
  (if (fast-prime? n 3)
    (report-prime n true (- (current-inexact-milliseconds) start-time))
    false))

(define (time-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (search-for-primes found n)
  (cond
    ((> found 2) "found three primes")
    ((time-prime-test n) (search-for-primes (+ found 1) (+ n 2)))
    (else (search-for-primes found (+ n 2)))))

(println " *** search for primes")
(search-for-primes 0 1001)
(println " *** search for primes")
(search-for-primes 0 10001)
(println " *** search for primes")
(search-for-primes 0 100001)
(println " *** search for primes")
(search-for-primes 0 1000001)
