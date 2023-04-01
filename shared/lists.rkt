#lang racket
(provide accumulate
         accumulate-n
         fold-right
         fold-left
         flatmap
         enumerate-interval
         enumerate-windows
         find-first)

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

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval j k)
  (define (iter i interval)
    (if (< i j)
      interval
      (iter (- i 1) (cons i interval))))
  (iter k '()))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list j i))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (enumerate-windows seq n)
  (define (setup-n-window)
    (define (rec i things)
      (if (> i n)
        (list '() things)
        (let ((setup (rec (+ i 1)
                          (cdr things))))
          (cons (cons (car things) (car setup))
                (cdr setup)))))
    (rec 1 seq))
  (define (shift-window w item)
    (append (cdr w) (list item)))
  (define (iter result window things)
    (if (null? things)
      (cons window result)
      (iter (cons window result)
            (shift-window window (car things))
            (cdr things))))
  (let ((setup (setup-n-window)))
    (iter '() (car setup) (cadr setup))))

(enumerate-windows
  (enumerate-interval 1 4)
  2)

(define (find-first pred? seq)
  (cond ((null? seq) false)
        ((pred? (car seq)) (car seq))
        (else (find-first pred? (cdr seq)))))
