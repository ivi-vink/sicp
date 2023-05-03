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
         test-complex-polar)
(require "./install-integer.rkt"
         "./install-rational.rkt"
         "./install-real.rkt"
         "./install-complex.rkt"
         "../../../shared/data-directed-programming.rkt")


(define pkg (make-dispatch-table))
(define get (getter pkg))
(define put (putter pkg))
(define print-tbl (printer pkg))
(define apply-fn (make-apply-with-raising-and-drop
                   get))

(install-integer put get)
(install-rational put get)
(install-real put get)
(install-complex apply-fn get put)

(define (install-arithmetic-package)
  (list get put apply-fn))

;; test running
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
(define test-complex-polar (make-complex-rect 1 2))
