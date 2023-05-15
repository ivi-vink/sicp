#lang racket
;; 12
(require compatibility/mlist)
(define (last-pair x)
  (if (null? (mcdr x))
    x
    (last-pair (mcdr x))))
(define x (mlist 'a 'b))
(define y (mlist 'c 'd))
(define z (mappend x y))
x
(define w (mappend! x y))
x

;; 13
(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)
(define zp (make-cycle (mlist 'a 'b 'c)))
zp
;; last-pair would make an infinite loop

;; 14
;; The procedure reverses the list. And messes up the list that is bound in the global env
(define v (mlist 'a 'b 'c 'd))
(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (mcdr x)))
        (set-mcdr! x y)
        (loop temp x))))
  (loop x '()))
(define wp (mystery v))
v
wp

;; 15
;; 16
;; 17
(define (make-pair-counter)
  (define counted (list))
  (define (count-pairs x)
    (display counted)
    (if (or (not (pair? x))
            (memq x counted))
      0
      (begin
        (set! counted (cons x counted))
        (+ 1
           (count-pairs (car x))
           (count-pairs (cdr x))))))
  count-pairs)
((make-pair-counter) (list 'a 'b 'c))
(define bc (list 'b 'c))
((make-pair-counter) (cons bc bc))
((make-pair-counter) (cons bc (cdr bc)))
(define c (list 'c))
(define b (cons c c))
((make-pair-counter) (cons b b))

;; 18/19
;; This is actually only a valid way to check for cycles if the first pair in the list is also the state of the cycle.
;; A better way would be to check against all memory addresses in the list by hashing or counting.
(define (cycle? x)
  (define (iter v items)
    (cond ((null? items) false)
          ((eq? v items) true)
          (else (iter v (mcdr items)))))
  (iter x (mcdr x)))
(cycle? (mlist 'a 'b 'c))
(cycle? (make-cycle (mlist 'a 'b 'c)))
;; For 19 you need a famous trick with two pointers

;; 20
