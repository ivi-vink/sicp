#lang racket
(require sicp)
(require "../../shared/chapter1.rkt")

;; Here we are iterating forward, while consing in reverse, this would be a good way to reverse a list
;; The difference is that iterating is like going forward and recursing is like going backwards.
;; Recursing works because it is going in reverse and we also need to cons in reverse.
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items nil))
;; (square-list (list 1 2 3 4))

;; This attempts the reverse the consing direction by chaning the first in the pair with the second 
;; The result is that we get pairs that point to other pairs with car, which is not how a list works.
(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer (square (car things))))))
  (iter items nil))
;;(square-list2 (list 1 2 3 4))

;; One thing we could try is also growing the answer list forward somehow
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things) (append answer (list (square (car things)))))))
  (iter items (list)))
(square-list3 (list 1 2 3 4))
