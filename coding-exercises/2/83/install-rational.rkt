#lang racket
(provide install-rational)
(require "../../../shared/data-directed-programming.rkt")

(define (install-rational get put apply-fn)
  ;; import generic methods
  (define (=zero? a)
    (apply-fn '=zero? a))
  (define (equ? a b)
    (apply-fn 'equ? a b))
  (define (add a b)
    (apply-fn 'add a b))
  (define (neg a)
    (apply-fn 'neg a))
  (define (sub a b)
    (apply-fn 'sub a b))
  (define (mul a b)
    (apply-fn 'mul a b))
  (define (div a b)
    (newline)
    (display (list "Calling div from rat package" a b))
    (apply-fn 'div a b))
  (define (cos a)
    (apply-fn 'cos a))
  (define (sin a)
    (apply-fn 'sin a))
  (define (sqr a)
    (apply-fn 'sqr a))
  (define (sqrt a)
    (apply-fn 'sqrt a))
  (define (atan a b)
    (apply-fn 'atan a b))

  (define (gcd a b)
    (newline)
    (newline)
    (display (list "GCD RATIONAL -- " a b))
    (let ((proc (get 'greatest-common-divisor (list (type-tag a) (type-tag b)))))
      (if proc
        (proc (contents a) (contents b))
        (error "Not implemented -- " (list 'greatest-common-divisor a (type-tag a) b (type-tag b))))))


  ;; constructor and selectors
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (let ((numer-div (get 'div (list (type-tag n) (type-tag g))))
            (denom-div (get 'div (list (type-tag d) (type-tag g)))))
        (if (and numer-div denom-div)
          (list (numer-div (contents n) (contents g))
                (denom-div (contents d) (contents g)))
          (list n d)))))



  (define (numer x) (car x))
  (define (denom x) (cadr x))

  ;; ops
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (neg-rat rat)
    (make-rat (neg (numer rat))
              (denom rat)))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (equ?-rat x y)
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))
  (define (=zero?-rat x)
    (=zero? (numer x)))

  (define (make-rat-reduce-lowest-terms n d)
    (define (sign x)
      (cond
        ((and (< x 0) (< d 0)) (* -1 x))
        ((and (< 0 x) (< d 0)) (* -1 x))
        (else x)))
    (let ((g (gcd n d)))
      (cons (sign (/ n g)) (abs (/ d g)))))

  (define (dropme rat)
    (if (and (integer? (numer rat))
             (integer? (denom rat)))
      ((get 'make 'integer) (/ (numer rat) (denom rat)))
      (list 'undefined)))
  (define (raiseme rat)
    (if (and (number? (numer rat))
             (number? (denom rat)))
      ((get 'make 'real) (/ (numer rat) (denom rat)))
      (list 'undefined)))

  ;; constructor
  (put 'make 'rational
       (lambda (x y) (tagme (make-rat x y))))

  ;; interface
  (define (tagme x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tagme (add-rat x y))))
  (put 'neg '(rational)
       (lambda (rat) (tagme (neg-rat rat))))
  (put 'sub '(rational rational)
       (lambda (x y) (tagme (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tagme (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tagme (div-rat x y))))
  (put 'raise '(rational) raiseme)
  (put 'project '(rational) dropme)
  ;; expt for integerizing factor
  (put 'expt '(rational rational) (lambda (r1 r2) (expt (raiseme r1) (raiseme r2))))
  ;; sqrt and trig methods for complex nums
  (put 'sqr '(rational) (lambda (r) (sqr (raiseme r))))
  (put 'sqrt '(rational) (lambda (r) (sqrt (raiseme r))))
  (put 'cos  '(rational) (lambda (r) (cos (raiseme r))))
  (put 'sin '(rational) (lambda (r) (sin (raiseme r))))
  (put 'atan '(rational rational) (lambda (r1 r2) (atan (raiseme r1) (raiseme r2))))

  ;; predicates
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero?-rat x)))
  'done)
