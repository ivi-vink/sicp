#lang racket
(provide install-arithmetic-package
         make-integer
         test-integer
         make-rat
         test-rat
         make-real
         test-real
         make-complex
         test-complex
         make-complex-rect
         test-complex-rect
         make-complex-polar
         test-complex-polar
         term
         dense-termlist
         sparse-termlist
         make-polynomial
         test-poly1
         test-poly2
         test-poly3
         =zero?
         equ?
         add
         sub
         mul
         div
         sinme
         cosme
         atanme
         sqrme
         sqrtme
         raiseme
         dropme)

(require "./install-integer.rkt"
         "./install-rational.rkt"
         "./install-real.rkt"
         "./install-complex.rkt"
         "./polynomials.rkt"
         "../../../shared/data-directed-programming.rkt")


(define pkg (make-dispatch-table))
(define get (getter pkg))
(define put (putter pkg))
(define print-tbl (printer pkg))
(define apply-fn (make-apply-with-raising-and-drop
                   get))

(install-integer put get)
(install-rational get put apply-fn)
(install-real put get)
(install-complex apply-fn get put)
(install-polynomial get put apply-fn)

(define (install-arithmetic-package)
  (list get put apply-fn))

;; constructors
;; integer
(define (make-integer n)
  ((get 'make 'integer) n))
(define test-integer (make-integer 3))

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
(define test-complex-polar (make-complex-polar (apply-fn 'magnitude test-complex)
                                               (apply-fn 'angle test-complex)))

;; polynomial
(define (term order coeff)
  ((get 'make-from-order-coeff 'term) order coeff))
(define (sparse-termlist . terms)
  ((get 'make-from-terms 'sparse-termlist) terms))
(define (dense-termlist . terms)
  ((get 'make-from-terms 'dense-termlist) terms))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define test-poly1 (make-polynomial 'x (sparse-termlist
                                         (term 1 test-integer))))
(define test-poly2 (make-polynomial 'x (sparse-termlist
                                         (term 100 test-complex)
                                         (term 2 test-real)
                                         (term 1 test-rat)
                                         (term 0 test-integer))))
(define test-poly3 (make-polynomial 'x (sparse-termlist
                                         (term 50 test-rat)
                                         (term 0 2))))

;; generic methods
(define (equ? a1 a2)
  (apply-fn 'equ? a2))
(define (=zero? datum)
  (apply-fn '=zero? datum))
(define (add a1 a2)
  (apply-fn 'add a1 a2))
(define (neg a)
  (apply-fn 'neg a))
(define (sub a1 a2)
  (apply-fn 'sub a1 a2))
(define (mul a1 a2)
  (apply-fn 'mul a1 a2))
(define (div a1 a2)
  (apply-fn 'div a1 a2))
(define (raiseme datum)
  (apply-fn 'raise datum))
(define (dropme datum)
  (apply-fn 'project datum))
(define (sqrme datum)
  (apply-fn 'sqr datum))
(define (sqrtme datum)
  (apply-fn 'sqrt datum))
(define (cosme datum)
  (apply-fn 'cos datum))
(define (sinme datum)
  (apply-fn 'sin datum))
(define (atanme a1 a2)
  (apply-fn 'atan a2))
