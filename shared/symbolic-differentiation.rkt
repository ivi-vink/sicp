#lang racket
(provide make-deriv
         variable?
         same-variable?
         =number?)
         
(define (variable? x) (symbol? x))
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))

(define (=number? x num)
  (and (number? x) (= x num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1)
              (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (make-exponent e p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) e)
        (else (list '** e p))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) 
  (cond ((null? (cdddr s)) (caddr s))
        (else (cons '+ (cddr s)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p)
  (cadr p))
(define (multiplicand p)
  (cond ((null? (cdddr p)) 
         (caddr p))
        (else (cons '* (cddr p)))))

(define (exponent? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base expo)
  (cadr expo))
(define (exponent expo)
  (caddr expo))


(define (make-deriv
          make-sum
          sum?
          addend
          augend
          make-product
          product?
          multiplier
          multiplicand)
  (define (deriv expr var)
   (cond ((number? expr) 0)
         ((variable? expr)
          (if (same-variable? expr var) 1 0))
         ((sum? expr)
          (make-sum (deriv (addend expr) var)
                    (deriv (augend expr) var)))
         ((product? expr)
          (make-sum
             (make-product 
               (multiplier expr)
               (deriv (multiplicand expr) var))
             (make-product 
               (deriv (multiplier expr) var)
               (multiplicand expr))))
         ((exponent? expr)
          (make-product
            (deriv (base expr) var)
            (make-product
              (exponent expr)
              (make-exponent (base expr) (- (exponent expr) 1)))))
         (else
           (error "unkown expression type -- DERIV" expr))))
  deriv)
