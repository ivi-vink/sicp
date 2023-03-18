#lang racket
(require "../../shared/lists.rkt")
(require "../../shared/chapter1.rkt")

(define (enumerate-interval j k)
  (define (iter i interval)
    (if (< i j) 
      interval
      (iter (- i 1) (cons i interval))))
  (iter k '()))
      
((lambda ()
   (newline)
   (display (enumerate-interval 2 5))))

(define (unique-pairs n) 
  (flatmap
    (lambda (i) 
      (map (lambda (j) (list j i)) 
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime? n)
  (fermat? 2 n))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair)
        (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

((lambda ()
   (display "unique-pairs")
   (newline)
   (display (unique-pairs 10))
   (newline)
   (display "prime-sum-pairs")
   (newline)
   (display (prime-sum-pairs 10))
   (newline)))
