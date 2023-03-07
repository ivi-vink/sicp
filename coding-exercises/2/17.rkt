#lang racket
(require sicp)

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (length items)
  (define (length-iter a c)
    (if (null? a)
      c
      (length-iter (cdr a) (inc c))))
  (length-iter items 0))

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref 
      (cdr items) 
      (dec n))))

(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

(define (print-list)
  (define odds (list 1 3 5 7 9 11))
  (println odds)
  (println (list-ref odds 2))
  (println (length odds))
  (println (append odds (list 22)))
  (println (last-pair odds)))

(print-list)
