#lang racket
(require "../../shared/symbolic-differentiation.rkt")
;; infix form with full parentheses
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else (list a1 '+ a2))))
(define (addend s)
  (car s))
(define (augend s)
  (caddr s))
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        (else (list m1 '* m2))))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier m)
  (car m))
(define (multiplicand m)
  (caddr m))
(define deriv (make-deriv
                make-sum
                sum?
                addend
                augend
                make-product
                product?
                multiplier
                multiplicand))
(deriv '(x + 3) 'x)
(deriv '(x * y) 'x)
(deriv '(x + (3 * (x + (y + 2)))) 'x)

;; infix form without full parentheses
;; trick was to use recursion to evaluate lower precedence terms first, which
;; means they are applied later
(define (infix-make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else (list a1 '+ a2))))
(define (infix-sum? s)
  (cond ((null? s) false)
        ((eq? (car s) '+) true)
        (else (infix-sum? (cdr s)))))
(define (infix-addend s)
  (define (rec seq)
    (cond ((null? seq) seq)
          ((eq? (car seq) '+) '())
          (else (cons (car seq) (rec (cdr seq))))))
  (let ((a (rec s)))
    (if (null? (cdr a))
      (car a)
      a)))
(define (infix-augend s)
  (cond ((null? s) '())
        ((eq? (car s) '+) (if (null? (cddr s)) (cadr s) (cdr s)))
        (else (infix-augend (cdr s)))))
(define test-sum '(a * b * c + d))
(define test-sum2 '(a + (b + c * d)))
(infix-sum? test-sum)
(infix-addend test-sum)
(infix-augend test-sum)
(infix-sum? test-sum2)
(infix-addend test-sum2)
(infix-augend test-sum2)

(define (infix-make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        (else (list m1 '* m2))))
(define (infix-product? p)
  (and (pair? p) (eq? (cadr p) '*)))
(define (infix-multiplier p)
  (car p))
(define (infix-multiplicand p)
  (if (null? (cdddr p))
    (caddr p)
    (cddr p)))
(define test-prd '(a * b * c))
(define test-prd2 '(c * d))
(infix-product? test-prd)
(infix-multiplier test-prd)
(infix-multiplicand test-prd)

(define infix-deriv (make-deriv
                      infix-make-sum
                      infix-sum?
                      infix-addend
                      infix-augend
                      infix-make-product
                      infix-product?
                      infix-multiplier
                      infix-multiplicand))
(infix-deriv '(3 * x * x + 2 * x + 3 * 4 * x) 'x)
(infix-deriv '(x * y) 'x)
(infix-deriv '(x + 3 * (x + y + 2)) 'x)
