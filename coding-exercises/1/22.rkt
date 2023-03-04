#lang racket
(require "../../shared/chapter1.rkt")
(require sicp)

(define (prime? n)
  (= n (smallest-divisor n)))

(define (report-prime n t duration)
  (print n)
  (println " *** prime")
  (println duration)
  t)

(define (start-prime-test n start-time)
  (if (prime? n)
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
