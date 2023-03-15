#lang racket
(require "../../shared/lists.rkt")

;; grow in reverse method
;; or pointer reversal method in other languages
(define (reverse l)
  (define (iter l p)
   (cond
     ((null? l) p)
     (else
      (iter
        (cdr l)
        (cons (car l) p)))))
  (iter l (list)))

(define (print)
    (println (list 1 4 9 16 25))
    (reverse (list 1 4 9 16 25)))
(print)
