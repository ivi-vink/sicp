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


(define coercion (make-dispatch-table))
(define put-coercion (putter coercion))
(define get-coercion (getter coercion))
(define print-coercion (printer coercion))

(define apply-generic (make-apply get))

(install-scheme-number-package put)
(install-rational-package put)
(install-complex-package apply-generic get put)

;; 81
;;a
;; type-tags are '(complex complex)
;; proc is false because exp is only defined for '(scheme-number scheme-number)
;; type1 and type2 are both 'complex
;; a1 a2 should be the complex tagged data
;; So now we are getting coercions, Louis' coercions of complex->complex and scheme-number->scheme-number are installed.
;; Louis' coercion complex->complex are also the only option for t1->t2 and t2->t1
;; This means that (apply-generic 'exp a1 a2) is called again, and we start over again from the beginning in a loop.
;;b
;; He is mistaken.
;; If there is no procedure for the two arguments with the same type, then coercing the type to itself will not change the result of the lookup in the dispatch table.
;;c
(define (make-apply-with-coercion get get-coercion)
  (lambda (op . args)
     (display args)
     (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
         (cond (proc
                 (apply proc (map contents args)))
               ((and (not (equal? (car type-tags)
                                  (cadr type-tags)))
                     (= (length args) 2))
                (let ((type1 (car type-tags))
                      (type2 (cadr type-tags))
                      (a1 (car args))
                      (a2 (cadr args)))
                  (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                            (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                            (apply-generic op a1 (t2->t1 a2)))
                          (else
                            (error "No method for these types" (list op type-tags)))))))
               (else (error "No method for these types -- APPLY-GENERIC"
                            (list op type-tags))))))))

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)
(define (ex x y) (apply-generic 'exp x y))
(put 'exp '(scheme-number scheme-number) (lambda (x y) (attach-tag 'scheme-number (expt x y))))
(define apply-generic-with-coercion (make-apply-with-coercion get get-coercion))

;; no method and no infinite recursion
;; (apply-generic-with-coercion 'exp ((get 'make-from-real-imag 'complex) 10 2) ((get 'make-from-real-imag 'complex) 10 3))

;; 82
;; So in the tower of types (numbers, rationals, reals, complex) of the arithmetic package this method covers most cases?
;; Because we can always coerce the lower arguments to the argument with the highest type.
;;
;; The cases that are not covered are for example ones where only a subset of the arguments needs to be coerced to find a suitable mixed type method, since we always coerce all arguments in this strategy.
(define (make-apply-with-coercion-args get get-coercion)

  ;; try to coerce all arguments to a type
  (define (coerce-or-fail t args)
    (define (iter coerced remaining-args)
      (cond ((null? remaining-args) coerced)
            ((equal? t (type-tag (car remaining-args)))
             (append coerced (car remaining-args)))
            (else (let ((t->arg (get-coercion t (type-tag (car remaining-args)))))
                   (if t->arg
                     (append coerced (t->arg t))
                     false)))))
    (iter '() args))

  ;; try to coerce all arguments to the type of one them
  (define (try-coerce type-tags op args)
    (define (iter havent-tried)
      (if (null? havent-tried)
        (error "No method for these types" (list op type-tags))
        (let ((coerced-args (coerce-or-fail (type-tag (car havent-tried)) args)))
          (if coerced-args
            (apply apply-generic (cons op coerced-args))
            (iter (cdr havent-tried))))))
    (iter type-tags))

  (lambda (op . args)
     (display args)
     (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
         (if proc
             (apply proc (map contents args))
             (try-coerce type-tags op args))))))

