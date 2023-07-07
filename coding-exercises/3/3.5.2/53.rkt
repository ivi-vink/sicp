#lang racket
(require "../../../shared/sicp-stream.rkt")
(newline) (print "54") (newline)
(define (add-streams s1 s2) (stream-map + s1 s2))
(define (scale-stream s factor) (stream-map (lambda (x) (* factor x)) s))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))
(define factorials (stream-cons 1 (mul-streams integers factorials)))
(stream-ref factorials 5)

(newline) (print "55") (newline)
(define (partial-sums s)
    (define me (stream-cons 0 (add-streams s me)))
    me)
(display-stream-until (partial-sums integers) 4)

(newline) (print "56") (newline)
(define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
           (let ((s1car (stream-car s1))
                 (s2car (stream-car s2)))
               (cond ((< s1car s2car)
                      (stream-cons s1car (merge (stream-cdr s1) s2)))
                     ((> s1car s2car)
                      (stream-cons s2car (merge s1 (stream-cdr s2))))
                     (else
                      (stream-cons s1car
                                   (merge (stream-cdr s1)
                                          (stream-cdr s2)))))))))
(define S (stream-cons 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

(newline) (print "57") (newline)
(define additions 0)
(define (add-streams-counted s1 s2) (stream-map (lambda (x y) (set! additions (+ additions 1)) (+ x y)) s1 s2))
(define fibs (stream-cons 0
                          (stream-cons 1
                                       (add-streams-counted (stream-cdr fibs)
                                                            fibs))))
(stream-ref fibs 20)
(println additions) (newline)
(println "Without memoization the number of additions is proportional to the additions of the previous steps since each time all additions of previous steps are recalculated, which is a characteristic of exponential functions.")
(println "To show it is you could do a proof by counting additions at each step. Or use the fact that fibonacci has the phi^2 relation.")

(newline) (print "58") (newline)
(define (expand num den radix)
    (stream-cons
     (quotient (* num radix) den)
     (expand (remainder (* num radix) den) den radix)))
(stream-ref (expand 1 7 10) 3)
(stream-ref (expand 3 8 10) 2)

(newline) (print "59") (newline)
(define (integrate-series s) (mul-streams s (stream-map / ones integers)))
(define exp-series (stream-cons 1 (integrate-series exp-series)))
(define cosine-series
    (stream-cons 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series
    (stream-cons 0 (integrate-series cosine-series)))

(newline) (print "60") (newline)
;; This was my solution after doodling how to traverse the coefficients.
(define (mul-series s1 s2)
    (stream-cons
     (* (stream-car s1) (stream-car s2))
     (add-streams
      (scale-stream (stream-cdr s2) (stream-car s1))
      (mul-series (stream-cdr s1) s2))))
(display-stream-until (add-streams (mul-series sine-series sine-series)
                                   (mul-series cosine-series cosine-series)) 8)

;; This one is also interesting: http://community.schemewiki.org/?sicp-ex-3.60
;; Instead of diagonal traversals in one direction, this one makes triangles.
(define (psum-mul-series s1 s2)
  (stream-cons
    (* (stream-car s1) (stream-car s2))
    (add-streams (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                              (scale-stream (stream-cdr s2) (stream-car s1)))
                 (stream-cons 0 (psum-mul-series (stream-cdr s1) (stream-cdr s2))))))
(display-stream-until (add-streams (psum-mul-series sine-series sine-series)
                                   (psum-mul-series cosine-series cosine-series)) 8)
