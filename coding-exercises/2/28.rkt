#lang racket
(require sicp)

(define (fringe tree)
  (define (iter tree leaves-list)
    (cond ((null? tree) leaves-list)
          ((pair? (car tree))
           (iter (cdr tree) (append leaves-list (iter (car tree) '()))))
          (else
           (iter (cdr tree) (append leaves-list (list (car tree)))))))
  (iter tree '()))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
