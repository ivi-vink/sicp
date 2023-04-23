#lang racket
(provide install-rational-package)
(require "../../../shared/data-directed-programming.rkt")


(define (install-rational-package put)
  ;; internal
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (define (sign x)
      (cond
        ((and (< x 0) (< d 0)) (* -1 x))
        ((and (< 0 x) (< d 0)) (* -1 x))
        (else x)))
    (let ((g (gcd n d)))
      (cons (sign (/ n g)) (abs (/ d g)))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; predicates
  (define (equ? x y)
    (and (equal? (numer x) (numer y))
         (equal? (denom x) (denom y))))
  (define (=zero? x)
    (equal? (numer x) 0))

  ;; interface
  (define (typetag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (typetag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (typetag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (typetag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (typetag (div-rat x y))))

  (put 'equ? '(rational rational)
       (lambda (x y) (equ? x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero? x)))

  (put 'make 'rational
       (lambda (x y) (typetag (make-rat x y))))
  'done)
