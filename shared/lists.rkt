#lang racket
(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (length items)
  (define (length-iter a c)
    (if (null? a)
      c
      (length-iter (cdr a) (+ 1 c))))
  (length-iter items 0))

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref
      (cdr items)
      (- 1 n))))

(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))
