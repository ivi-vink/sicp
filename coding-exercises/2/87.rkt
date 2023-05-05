#lang racket
(require "../../shared/data-directed-programming.rkt")
(require "./83/install.rkt")
;; arithmetic package
(define get-put-apply (install-arithmetic-package))
(define get (car get-put-apply))
(define put (cadr get-put-apply))
(define apply-fn (caddr get-put-apply))

(define (install-polynomial-package put)
  ;; internal procedures
  (define (tagme p)
    (attach-tag 'polynomial p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))

  ;; terms
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; termlists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  ;; polys
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; ops
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1)
                        (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((> (order t2)
                        (order t1))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term
                          (order t1)
                          (apply-fn 'add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

  (define (neg-poly poly)
    (make-poly
      (variable poly)
      (map (lambda (term)
             (make-term
               (order term)
               (apply-fn 'neg (coeff term))))
           (term-list poly))))
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term (make-term
                       (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
                     (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY" (list p1 p2))))
  (define (polynomial-=zero? poly)
    (define (rec term-list)
      (cond ((empty-termlist? term-list) true)
            ((not (apply-fn '=zero? (coeff (first-term term-list)))) false)
            (else (rec (rest-terms term-list)))))
    (rec (term-list poly)))
  (put '=zero? '(polynomial) polynomial-=zero?)
  ;;interface
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tagme (add-poly p1 p2))))
  (put 'neg '(polynomial) (lambda (p) (tagme (neg-poly p))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tagme (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tagme (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tagme (make-poly var terms))))
  'done)

(install-polynomial-package put)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define test-poly1 (make-polynomial 'x (list
                                         (list 1 test-integer))))
(define test-poly2 (make-polynomial 'x (list
                                         (list 100 test-complex)
                                         (list 3 test-real)
                                         (list 1 test-rat)
                                         (list 0 test-integer))))
(define test-poly3 (make-polynomial 'x (list
                                         (list 1 2)
                                         (list 0 2))))
((lambda ()
   (newline)
   (display (add test-poly2 test-poly2))
   (newline)
   (display (mul test-poly2 test-poly2))
   (newline)
   (display (mul test-poly3 test-poly3))))

;;87
(=zero? test-poly3)
(=zero? (make-polynomial 'x (list (list 1000 0))))

;; 88
;; what is meant with negation here? Negation of a number? Making a negative number?
;; Guess that would be handy if we need to subtract a lot of terms.
((lambda ()
   (newline)
   (display (sub test-poly1 test-poly3))
   (newline)
   (display (sub test-poly1 test-poly2))))
