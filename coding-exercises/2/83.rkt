#lang racket
(require "../../shared/data-directed-programming.rkt")
(require "../../shared/lists.rkt")
(require "./78/scheme-number.rkt")
(require "./78/install-rational-package.rkt")
(require "./78/install-complex-package.rkt")
;; We are basically making a data directed framework for arithmethic operations in this module
(define pkg (make-dispatch-table))
(define put (putter pkg))
(define get (getter pkg))
(define print-tbl (printer pkg))


(define coercion (make-dispatch-table))
(define put-coercion (putter coercion))
(define get-coercion (getter coercion))
(define print-coercion (printer coercion))

(define apply-generic (make-apply get))
(define apply-coercion (make-apply-with-coercion get get-coercion))

;; stores integers as exact racket integers
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
  (put 'make 'integer (lambda (x) (tagme (make x))))
  ;; methods
  (put 'add '(integer integer) (lambda (x y) (tagme (make (+ x y)))))
  (put 'sub '(integer integer) (lambda (x y) (tagme (make (- x y)))))
  (put 'mul '(integer integer) (lambda (x y) (tagme (make (* x y)))))
  (put 'div '(integer integer) (lambda (x y) (tagme (make (/ x y)))))
  (put 'raise '(integer) raiseme)
  ;; predicates
  (put 'equ? '(integer integer) (lambda (x y) (= x y)))
  (put '=zero? '(integer) (lambda (x) (= 0 x)))
  'done)

;; stores rationals as custom data object
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

  ;; predicates
  (put 'equ? '(rational rational)
       (lambda (x y) (equ? x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero? x)))
  'done)

;; stores reals as inexact racket numbers
(define (install-real put get)
  ;; local methods
  (define (tagme datum)
    (attach-tag 'real datum))
  (define (make i)
    (exact->inexact i))
  (define (raiseme r)
    ((get 'make-from-real-imag 'complex) r 0))
  ;; constructor
  (put 'make 'real (lambda (x) (tagme (make x))))
  ;; methods
  (put 'add '(real real) (lambda (x y) (tagme (make (+ x y)))))
  (put 'sub '(real real) (lambda (x y) (tagme (make (- x y)))))
  (put 'mul '(real real) (lambda (x y) (tagme (make (* x y)))))
  (put 'div '(real real) (lambda (x y) (tagme (make (/ x y)))))
  (put 'raise '(real) raiseme)
  (put 'project '(real) (lambda (n)
                          ((get 'make 'rational) (round n) 1)))
  ;; predicates
  (put 'equ? '(real real) (lambda (x y) (= x y)))
  (put '=zero? '(real) (lambda (x) (= 0 x)))
  'done)

;; stores complex numbers as custom data type
(install-integer put get)
(install-rational put get)
(install-real put get)
;; use from previous exercise
(install-complex-package apply-generic get put)
(put 'project '(complex) (lambda (z)
                           ((get 'make 'real) (apply-generic 'real-part z))))

;; test running
;; integer
(define (make-integer n)
  ((get 'make 'integer) n))
(define test-num (make-integer 3))

;; rational
(define (make-rat n d)
  ((get 'make 'rational) n d))
(define test-rat (make-rat 5 2))

;; real
(define (make-real n)
  ((get 'make 'real) n))
(define test-real (make-real 1.5))

;; complex
(define (make-complex x y)
  ((get 'make-from-real-imag 'complex) x y))
(define test-complex (make-complex 1 2))

(define (raisetower datum)
  (apply-coercion 'raise datum))

;; ((lambda ()
;;    (display test-num)
;;    (newline)
;;    (display test-rat)
;;    (newline)
;;    (display test-real)
;;    (newline)
;;    (display test-complex)
;;    (newline)
;;    (display (raisetower 1))
;;    (newline)
;;    (display (raisetower test-rat))
;;    (newline)
;;    (display (raisetower test-real))
;;    (newline)))


;; 84
(define (type-match? args)
  (not (find-first (lambda (x)
                     (not (equal? (type-tag x)
                                  (type-tag (car args)))))
                   args)))
(define (count-raises-until-top apply-generic datum)
  (define (iter i raised)
    (let ((result (apply-generic 'raise raised)))
      (if result
        (iter (+ i 1) result)
        i)))
  (iter 0 datum))



(define (highest-type apply-generic items)
  (cdr
    (foldl
     (lambda (raises item result)
       (cond ((< (car result) 0) (cons raises item))
             ((< raises (car result)) (cons raises item))
             (else result)))
     (cons -1 'nil)
     (map (lambda (x)
            (count-raises-until-top apply-generic x))
          items)
     (map type-tag items))))

(define (raise-until apply-generic type datum)
  (cond ((equal? type (type-tag datum)) datum)
        (else (let ((result (apply-generic 'raise datum)))
                (if result
                  (raise-until apply-generic type result)
                  false)))))

(define (raise-until-type-match apply-generic type items)
  (cond ((null? items) '())
        (else (let ((result (raise-until apply-generic type (car items))))
                (if result
                  (cons result (raise-until-type-match apply-generic type (cdr items)))
                  (error "Could not raise type --" (list type items)))))))

; (raise-until-type-match (make-apply-pred get)
;                         (highest-type (make-apply-pred get) (list 1 test-complex))
;                         (list 1 test-complex)))))))


(define (make-apply-with-raising apply-generic get)
  (lambda (op . args)
    (let ((result (apply apply-generic (cons op args))))
      (if result
        result
        (let ((raised-args (raise-until-type-match apply-generic (highest-type apply-generic args) args)))
          (let ((raised-result (apply apply-generic (cons op raised-args))))
            (if raised-result
               raised-result
               (error "Could not apply --" (list op args raised-args)))))))))

(define apply-with-raising (make-apply-with-raising (make-apply-pred get) get))
((lambda ()
   (newline)
   (apply-with-raising 'add 1 test-complex)))

;; 85
;; lowerable?
(define (project datum)
  (apply-generic 'project datum))
(project test-complex)
(project 1.5)
(project test-rat)
(define (equ? d1 d2)
  (apply-generic 'equ? d1 d2))

(define (can-drop? datum)
  (equ? (raisetower (project datum))
        datum))
(define (towerdrop datum)
  (cond ((and (get 'project (list (type-tag datum)))
              (can-drop? datum))
         (towerdrop (project datum)))
        (else datum)))
(towerdrop (make-complex 1 1))

(define (make-apply-with-raising-and-drop apply-generic get)
  (lambda (op . args)
    (let ((result (apply apply-generic (cons op args))))
      (if result
        (towerdrop result)
        (let ((raised-args (raise-until-type-match apply-generic (highest-type apply-generic args) args)))
          (let ((raised-result (apply apply-generic (cons op raised-args))))
            (if raised-result
               (towerdrop raised-result)
               (error "Could not apply --" (list op args raised-args)))))))))
(define apply-and-drop (make-apply-with-raising-and-drop (make-apply-pred get) get))
(apply-and-drop 'add
                1.0
                (make-complex 1 0))
