#lang racket
(provide install-complex-package)
(require "../../../shared/data-directed-programming.rkt")
(require "./complex-rectangular.rkt")
(require "./complex-polar.rkt")


(define (install-complex-package apply-generic get put)
  ;; install and import methods
  (install-rectangular-package put)
  (install-polar-package put)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))

  ;; internal
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; predicates (...) -> bool
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (= (real-part z)) (= (imag-part z))))

  ;; interface
  (define (typetag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'add '(complex complex)
       (lambda (z1 z2) (typetag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (typetag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (typetag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (typetag (div-complex z1 z2))))

  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ? z1 z2)))
  (put '=zero? '(complex)
       (lambda (z1) (=zero? z1)))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (typetag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (typetag (make-from-mag-ang r a))))
  'done)

