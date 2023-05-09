#lang racket
(provide install-integer)
(require "../../../shared/data-directed-programming.rkt")

(define (install-integer put get)
  ;; local methods
  (define (tagme datum)
    (attach-tag 'integer datum))
  (define (make i)
    (inexact->exact (round i)))
  (define (raiseme i)
    (if (equal? (type-tag i) 'integer)
      ((get 'make 'rational) i 1)
      (error "cannot raise non integer in integer package")))
  ;; constructor
  (put 'make 'integer (lambda (x)
                        (newline)
                        (display (list "MAKE INTEGER --" x (make x)))
                        (tagme (make x))))

  (define (gcd-integer a b)
    (newline)
    (display (list "GCD-INTEGER -- " a b))
    (if (= b 0)
      a
      (gcd-integer b (remainder a b))))

  ;; methods
  (put 'add '(integer integer) (lambda (x y) (+ x y)))
  (put 'neg '(integer) (lambda (x) (- x)))
  (put 'sub '(integer integer) (lambda (x y) (- x y)))
  (put 'mul '(integer integer) (lambda (x y) (* x y)))
  (put 'div '(integer integer) (lambda (x y) (/ x y)))
  (put 'greatest-common-divisor '(integer integer) (lambda (a b)
                                                     (gcd-integer a b)))
  (put 'raise '(integer) raiseme)
  ;; sqrt and trig methods for complex nums
  (put 'sqr '(integer) sqr)
  (put 'sqrt '(integer) sqrt)
  (put 'atan '(integer integer) atan)
  (put 'cos  '(integer) cos)
  (put 'sin '(integer) sin)
  ;; predicates
  (put 'equ? '(integer integer) (lambda (x y) (= x y)))
  (put '=zero? '(integer) (lambda (x) (= 0 x)))
  'done)

