#lang racket
(provide install-rational)
(require "../../../shared/data-directed-programming.rkt")

(define (install-rational put get)
  ;; local methods
  (define (tagme x) (attach-tag 'rational x))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
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
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero? x)
    (equal? (numer x) 0))

  (define (make-rat n d)
    (define (sign x)
      (cond
        ((and (< x 0) (< d 0)) (* -1 x))
        ((and (< 0 x) (< d 0)) (* -1 x))
        (else x)))
    (let ((g (gcd n d)))
      (cons (sign (/ n g)) (abs (/ d g)))))

  (define (raiseme rat)
    ((get 'make 'real) (/ (numer rat) (denom rat))))

  ;; constructor
  (put 'make 'rational
       (lambda (x y) (tagme (make-rat x y))))

  ;; interface
  (put 'add '(rational rational)
       (lambda (x y) (tagme (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tagme (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tagme (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tagme (div-rat x y))))
  (put 'raise '(rational) raiseme)
  (put 'project '(rational) (lambda (rat)
                              ((get 'make 'integer) (/ (numer rat) (denom rat)))))
  ;; sqrt and trig methods for complex nums
  (put 'sqr '(rational) (lambda (r) (sqr (raiseme r))))
  (put 'sqrt '(rational) (lambda (r) (sqrt (raiseme r))))
  (put 'cos  '(rational) (lambda (r) (cos (raiseme r))))
  (put 'sin '(rational) (lambda (r) (sin (raiseme r))))
  (put 'atan '(rational rational) (lambda (r1 r2) (atan (raiseme r1) (raiseme r2))))

  ;; predicates
  (put 'equ? '(rational rational)
       (lambda (x y) (equ? x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero? x)))
  'done)
