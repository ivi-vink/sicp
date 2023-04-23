#lang racket
(require "../../shared/data-directed-programming.rkt")
(require "./78/scheme-number.rkt")
(require "./78/install-rational-package.rkt")
(require "./78/install-complex-package.rkt")
;; We are basically making a data directed framework for arithmethic operations in this module
(define pkg (make-dispatch-table))
(define put (putter pkg))
(define get (getter pkg))
(define print-tbl (printer pkg))
(define apply-generic (make-apply put get))

(install-scheme-number-package put)
(install-rational-package put)
(install-complex-package apply-generic get put)

;; test running
;; num
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define test-num (make-scheme-number 3))

;; rational
(define (make-rat n d)
  ((get 'make 'rational) n d))
(define test-rat (make-rat 1 2))

;; complex
(define (make-complex x y)
  ((get 'make-from-real-imag 'complex) x y))
(define test-complex (make-complex 1 2))

((lambda ()
   (println "add sub mul div")
   (print (apply-generic 'add test-num test-num))
   (print (apply-generic 'add test-rat test-rat))
   (print (apply-generic 'add test-complex test-complex))
   (newline)
   (print (apply-generic 'sub test-num test-num))
   (print (apply-generic 'sub test-rat test-rat))
   (print (apply-generic 'sub test-complex test-complex))
   (newline)
   (print (apply-generic 'div test-num test-num))
   (print (apply-generic 'div test-rat test-rat))
   (print (apply-generic 'div test-complex test-complex))
   (newline)
   (print (apply-generic 'mul test-num test-num))
   (print (apply-generic 'mul test-rat test-rat))
   (print (apply-generic 'mul test-complex test-complex))))

;; 78
((lambda ()
   (newline)
   ;; Should be represented just as a scheme number
   (display test-num)))

;; 79
(define (equ? a b)
  (apply-generic 'equ? a b))
((lambda ()
   (newline)
   (display "testing equ?")
   (newline)
   (println (equ? test-num test-num))
   (println (equ? test-rat test-rat))
   (println (equ? test-complex test-complex))))

;; 80
(define (=zero? n)
  (apply-generic '=zero? n))
((lambda ()
   (newline)
   (println (=zero? test-num))
   (println (=zero? test-rat))
   (println (=zero? test-complex))))
