#lang racket
(require racket/pretty)
(require sicp)

(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))


(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

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
