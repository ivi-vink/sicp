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
(stream-ref (partial-sums integers) 4)

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
(println additions) (newline) (println "Streams in racket are forced when car is called.")
