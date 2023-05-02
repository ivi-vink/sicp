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
  ;; sqrt and trig methods for complex nums
  (put 'sqr '(integer) sqr)
  (put 'sqrt '(integer) sqrt)
  (put 'atan '(integer) atan)
  (put 'cos  '(integer) cos)
  (put 'sin '(integer) sin)
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
  ;; sqrt and trig methods for complex nums
  (put 'sqr '(real) sqr)
  (put 'sqrt '(real) sqrt)
  (put 'cos  '(real) cos)
  (put 'sin '(real) sin)
  (put 'atan '(real real) (lambda (x y) (atan x y)))
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
(define (make-complex-rect x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-polar x y)
  ((get 'make-from-mag-ang 'complex) x y))
(define test-complex (make-complex 1 2))
(define test-complex-rect (make-complex-rect 1 2))
(define test-complex-polar (make-complex-rect 1 2))

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
(define apply-and-drop-test (make-apply-with-raising-and-drop (make-apply-pred get) get))
(apply-and-drop-test 'add
                1.0
                (make-complex 1 0))

;; 86
;; new package
(define generic-pkg (make-dispatch-table))
(define generic-put (putter generic-pkg))
(define generic-get (getter generic-pkg))
(define apply-and-drop (make-apply-with-raising-and-drop (make-apply-pred generic-get) generic-get))
(install-integer generic-put generic-get)
(install-rational generic-put generic-get)
(install-real generic-put generic-get)
(install-complex-package apply-and-drop generic-get generic-put)
(generic-put 'project '(complex) (lambda (z)
                                   ((generic-get 'make 'real) (apply-and-drop 'real-part z))))
;; Selectors and constructors of complex numbers packages need to become generic
;; We can try to raise to real numbers before passing it to the trig functions, but we need to do this for every possible type in the system.
;; So it is better to let the types themselves define trig functions
;;
;; Rectangular complex numbers:
;;   1. Make from mag angle uses cos and sin (not used by complex package however, since we always store make-from-mag-angle as polar?)
;;   2. Angle uses atan
;;   3. Magnitude uses sqr sqrt
;; Polar complex numbers:
;;   1. make from real imag uses sqr sqrt and atan (not used by complex package however, since we always store make-from-real-imag as rectangular?)
;;   2. Selectors use sine and cos
;;
;;   So at these for points we can try to raise to real numbers, but then it doesn't work
;;   for all numbers we potentially want to add to the system.
;;   So we are going to add these generic operations to the complex package.
;;   NOTE(mike): mixed types are not supported in my system, but they could be if we use the raising apply generic probably!
;;               for example we can mul a real and rational with the raising applier
(apply-and-drop 'angle (make-complex-rect test-rat test-rat))
(apply-and-drop 'magnitude (make-complex-rect test-rat test-rat))
(apply-and-drop 'real-part (make-complex-polar test-rat test-rat))
(apply-and-drop 'imag-part (make-complex-polar test-rat test-rat))
