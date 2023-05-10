#lang racket
(provide install-real)
(require "../../../shared/data-directed-programming.rkt")

(define (install-real put get)
  (define threshold 0.00001)
  ;; local methods
  (define (tagme datum)
    (attach-tag 'real datum))
  (define (make i)
    (let ((n (exact->inexact i)))
      (if (< (abs n) threshold)
        0.0
        n)))
  (define (raiseme r)
    ((get 'make-from-real-imag 'complex) r 0))
  ;; constructor
  (put 'make 'real (lambda (x) (tagme (make x))))
  ;; methods

  (define (gcd-real a b)
    (if (and (integer? a)
             (integer? b))
      (if (= b 0)
        a
        (gcd-real b (remainder a b)))
      a))

  (put 'add '(real real) (lambda (x y) (tagme (make (+ x y)))))
  (put 'neg '(real) (lambda (x) (tagme (make (- x)))))
  (put 'sub '(real real) (lambda (x y) (tagme (make (- x y)))))
  (put 'mul '(real real) (lambda (x y) (tagme (make (* x y)))))
  (put 'div '(real real) (lambda (x y) (tagme (make (/ x y)))))
  (put 'raise '(real) raiseme)
  (put 'project '(real) (lambda (n)
                          ((get 'make 'rational) n 1.0)))
  (put 'greatest-common-divisor '(real real) (lambda (a b) (tagme (gcd-real a b))))
  ;; expt for integerizing factor
  (put 'expt '(real real) expt)
  ;; sqrt and trig methods for complex nums
  (put 'sqr '(real) sqr)
  (put 'sqrt '(real) sqrt)
  (put 'cos  '(real) cos)
  (put 'sin '(real) sin)
  (put 'atan '(real real) (lambda (x y) (atan x y)))
  ;; predicates
  (put 'equ? '(real real) (lambda (x y) (= x y)))
  (put '=zero? '(real) (lambda (x) (< x threshold)))
  'done)
