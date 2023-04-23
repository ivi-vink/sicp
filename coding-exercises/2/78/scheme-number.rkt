#lang racket
(provide install-scheme-number-package)
(require "../../../shared/data-directed-programming.rkt")


(define (install-scheme-number-package put)
  ;; interface part
  (define (typtag x)
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number) (lambda (x y) (typtag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (typtag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (typtag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (typtag (/ x y))))
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number) (lambda (x) (= 0 x)))
  (put 'make 'scheme-number (lambda (x) (typtag x)))
  'done)
