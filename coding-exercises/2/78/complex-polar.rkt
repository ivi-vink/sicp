#lang racket
(provide install-polar-package)
(require "../../../shared/data-directed-programming.rkt")

(define (install-polar-package apply-fn put)
  ;; import methods
  (define (mul a b)
    (apply-fn 'mul a b))
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

  ;; selectors
  (define (magnitude z)
      (car z)
    (define (angle z)
      (cdr z))
    (define (make-from-mag-ang r a)
      (cons r a)))

    ;; generic selectors
  (define (real-part z)
      (mul (magnitude z)
         (cos (angle z))))
  (define (imag-part z)
    (mul (magnitude z)
       (sin (angle z))))

    ;; constructor
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (sqr x) (sqr y)))
          (atan y x)))

  ;; register in data-driven package)
  (define (typtag x)
    (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar (lambda (x y) (typtag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda (r a) (typtag (make-from-mag-ang r a))))
  'done)
