#lang racket

;; linear scan required for this one, but on average we save some time because sometimes we can quickly 
;; adjoin small values and other times a full linear scan is required to add a high value.
(define (adjoin-set x myset)
  (cond ((null? myset) (cons x '()))
        ((= (car myset) x) myset)
        ((> (car myset) x) (cons x myset))
        (else (cons (car myset) 
                    (adjoin-set x (cdr myset))))))

(define test-set (list 1 2 3 4 5 7))
(adjoin-set 6 test-set)
(adjoin-set 8 test-set)
