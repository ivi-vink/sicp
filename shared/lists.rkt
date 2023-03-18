#lang racket
(provide accumulate
         accumulate-n
         fold-right
         fold-left)
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

(define (accumulate op initial sequence)
  (cond ((null? sequence) initial)
        (else (op (car sequence)
                  (accumulate op initial (cdr sequence))))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))
