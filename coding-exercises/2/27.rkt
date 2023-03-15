#lang racket
(require sicp)

(define (reverse l)
  (define (iter l p)
   (cond
     ((null? l) p)
     (else
      (iter
        (cdr l)
        (cons (car l) p)))))
  (iter l (list)))

(define (deep-reverse nested-list)
  (define (iter l p)
   (cond
     ((null? l) p)
     ((pair? (car l))
      (iter (cdr l)
            (cons (iter (car l) nil)
                  p)))
     (else
      (iter
        (cdr l)
        (cons (car l) p)))))
  (iter nested-list nil))

(deep-reverse (list (list 1 2) (list 3 4)))
