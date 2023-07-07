#lang racket
(require racket/stream)
(provide
 stream-cons
 stream-car
 stream-cdr
 the-empty-stream
 stream-null?
 display-line
 show
 display-stream
 display-stream-until
 stream-ref
 stream-for-each
 stream-enumerate-interval
 stream-map
 stream-filter)

;; (define (delay b) (lambda () b))
;; (define (force f) (f))
;; (define (cons-stream a b) (cons a (delay b)))
;; (define (cons-stream a b) (stream-cons  a b))
;; streams in racket by default are not eager for the head of the cons
(define (stream-car stream) (stream-first stream))
(define (stream-cdr stream) (stream-rest stream))
(define the-empty-stream (stream))
(define (stream-null? stream) (stream-empty? stream))
(define (display-line x) (newline) (display x))
(define (show x) (display-line x) x)
(define (display-stream s) (stream-for-each display-line s))
(define (display-stream-until s until)
    (cond ((< until 0) 'done)
          ((stream-null? s) 'done)
          (else (begin
                 (display-line (stream-car s))
                 (display-stream-until (stream-cdr s) (- until 1))))))

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (stream-cons
         low
         (stream-enumerate-interval (+ low 1) high))))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (stream-cons
         (apply proc (map stream-car argstreams))
         (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

(define (stream-filter pred s)
    (cond ((stream-null? s) the-empty-stream)
          ((pred (stream-car s)) (stream-cons  (stream-car s) (stream-filter pred (stream-cdr s))))
          (else (stream-filter pred (stream-cdr s)))))
