#lang racket
(require sicp)

(define (cc-list amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (empty? kinds-of-coins)) 0)
        (else (+ (cc-list amount 
                          (cdr kinds-of-coins))
                 (cc-list (- amount 
                             (car kinds-of-coins)) 
                          kinds-of-coins)))))

(define us-coins (list 50 25 10 5 1))
(cc-list 100 us-coins)

(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(cc-list 100 uk-coins)
