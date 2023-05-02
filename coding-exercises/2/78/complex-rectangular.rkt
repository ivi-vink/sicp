#lang racket
(provide install-rectangular-package)
(require "../../../shared/data-directed-programming.rkt")


(define (install-rectangular-package apply-generic put)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) (apply-generic 'sqrt (+ (apply-generic 'sqr (real-part z))
                                                (apply-generic 'sqr (real-part z)))))
  (define (angle z)
    (apply-generic 'atan (imag-part z)
          (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a))
          (* r (sin a))))
  ;; interface part
  (define (typtag x)
    (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (typtag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (typtag (make-from-mag-ang r a))))
  'done)
