#lang racket
(provide install-complex)
(require "../../../shared/data-directed-programming.rkt")

(define (install-rectangular-package apply-fn put)
  ;; import methods
  (define (add a b)
    (apply-fn 'add a b))
  (define (sub a b)
    (apply-fn 'sub a b))
  (define (mul a b)
    (apply-fn 'mul a b))
  (define (div a b)
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

  ;; selectors
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  ;; generic selectors
  (define (magnitude z) (sqrt (add (sqr (real-part z))
                                   (sqr (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))

  ;; constructors
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (mul r (cos a))
          (mul r (sin a))))

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
      (car z))
  (define (angle z)
    (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))

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

(define (install-complex apply-fn get put)
  ;; import methods
  (define (add a b)
    (apply-fn 'add a b))
  (define (sub a b)
    (apply-fn 'sub a b))
  (define (mul a b)
    (apply-fn 'mul a b))
  (define (div a b)
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
  (define (=zero? a)
    (apply-fn '=zero? a))
  (define (equ? a b)
    (apply-fn 'equ? a b))
  (install-rectangular-package apply-fn put)
  (install-polar-package apply-fn put)

  ;; constructors
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; selectors
  (define (real-part z)
    (apply-fn 'real-part z))
  (define (imag-part z)
    (apply-fn 'imag-part z))
  (define (magnitude z)
    (apply-fn 'magnitude z))
  (define (angle z)
    (apply-fn 'angle z))

  ;; internal
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

  ;; predicates (...) -> bool
  (define (complex-equ? z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
  (define (complex-=zero? z)
    (and (=zero? (real-part z))
         (=zero? (imag-part z))))

  ;; interface
  (define (typetag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'add '(complex complex)
       (lambda (z1 z2) (typetag (add-complex z1 z2))))
  (put 'neg '(complex)
       (lambda (z) (typetag (make-from-real-imag (- (real-part z))
                                                 (- (imag-part z))))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (typetag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (typetag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (typetag (div-complex z1 z2))))

  (put 'equ? '(complex complex)
       (lambda (z1 z2) (complex-equ? z1 z2)))
  (put '=zero? '(complex)
       (lambda (z1) (complex-=zero? z1)))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (typetag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (typetag (make-from-mag-ang r a))))

  (put 'project '(complex) (lambda (z)
                             ((get 'make 'real) (apply-fn 'real-part z))))
  'done)
