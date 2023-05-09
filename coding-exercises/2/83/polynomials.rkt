#lang racket
(provide install-polynomial)
(require "../../../shared/data-directed-programming.rkt")

(define (install-term-package get put apply-fn)
  (define (tagme term)
    (attach-tag 'term term))
  (put 'make-from-order-coeff 'term (lambda (order coeff) (tagme (list order coeff))))
  (put 'order '(term) (lambda (term) (car term)))
  (put 'coeff '(term) (lambda (term) (cadr term))))

(define (install-sparse-termlist-package get put apply-fn)
  ;; methods imported from term package
  (define (order term)
    ((get 'order '(term)) term))
  (define (coeff term)
    ((get 'coeff '(term)) term))
  (define (make-term order coeff)
    ((get 'make-from-order-coeff 'term) order coeff))

  (define (=zero? a)
    (apply-fn '=zero? a))
  (define (equ? a b)
    (apply-fn 'equ? a b))
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
  ;; export first term as typed term
  (define (first-term term-list)
    (let ((term (car term-list)))
      (make-term (car term)
                 (cdr term))))
  (define (rest-terms term-list) (cdr term-list))

  ;; ops
  ;; map neg over internal storage type (order coeff)
  (define (neg termlist)
    (map (lambda (term)
           (cons
             (car term)
             (apply-fn 'neg (cdr term))))
         termlist))

  ;; convert term contents to our format
  (define (term-contents->order-coeff-pair term)
    (cons (order term)
          (coeff term)))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons (term-contents->order-coeff-pair term)
            term-list)))

  ;; preds
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))

  ;; constructors
  ;; store as ((order coeff)), list of untyped (order coeff) pairs
  (define (term->order-coeff-pair term)
    (cons (apply-fn 'order term)
          (apply-fn 'coeff term)))
  (define (make-from-terms terms)
    (map term->order-coeff-pair terms))


  ;; interface methods
  (define (tagme datum)
    (attach-tag 'sparse-termlist datum))
  ;; constructors
  (put 'make-from-terms 'sparse-termlist (lambda (t) (tagme (make-from-terms t))))
  ;; ops
  (put 'neg '(sparse-termlist) (lambda (termlist) (tagme (neg termlist))))
  (put 'adjoin-term '(term sparse-termlist) (lambda (term termlist) (tagme (adjoin-term term termlist))))
  (put 'rest-terms '(sparse-termlist) (lambda (termlist) (tagme (rest-terms termlist))))
  ;; term selector
  (put 'first-term '(sparse-termlist) first-term)
  ;; pred
  (put 'empty-termlist? '(sparse-termlist) empty-termlist?)
  (put 'the-empty-termlist 'sparse-termlist (lambda () (tagme (the-empty-termlist)))))

(define (install-dense-termlist-package get put apply-fn)
  ;; methods imported from term package
  (define (order term)
    ((get 'order '(term)) term))
  (define (coeff term)
    ((get 'coeff '(term)) term))
  (define (make-term order coeff)
    ((get 'make-from-order-coeff 'term) order coeff))

  (define (=zero? a)
    (apply-fn '=zero? a))
  (define (equ? a b)
    (apply-fn 'equ? a b))
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
  ;; export first term as typed term
  (define (first-term term-list)
    (let ((term (car term-list)))
      (make-term (- (length term-list) 1)
                 term)))
  (define (rest-terms term-list) (cdr term-list))

  ;; ops
  ;; map neg over internal storage type (coeff ...)
  (define (neg termlist)
    (map (lambda (term)
           (apply-fn 'neg term))
         termlist))

  ;; convert term contents to our format
  (define (term-contents->dense-format term)
    (coeff term))
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((= (order term)
              (length term-list))
           (cons (term-contents->dense-format term) term-list))
          (else (adjoin-term term (cons 0 term-list)))))

  ;; preds
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))

  ;; constructors
  ;; store as ((order coeff)), list of untyped (order coeff) pairs
  (define (make-from-terms terms)
    (if (and (pair? terms)
             (not (equal? 'term (type-tag (car terms)))))
      (error "Make-from-terms encountered non-term --" terms)
      (cond ((null? terms) (the-empty-termlist))
            (else (adjoin-term (contents (car terms))
                               (make-from-terms (cdr terms)))))))


  ;; interface methods
  (define (tagme datum)
    (attach-tag 'dense-termlist datum))
  ;; constructors
  (put 'make-from-terms 'dense-termlist (lambda (t) (tagme (make-from-terms t))))
  ;; ops
  (put 'neg '(dense-termlist) (lambda (termlist) (tagme (neg termlist))))
  (put 'adjoin-term '(term dense-termlist) (lambda (term termlist) (tagme (adjoin-term term termlist))))
  (put 'rest-terms '(dense-termlist) (lambda (termlist) (tagme (rest-terms termlist))))
  ;; term selector
  (put 'first-term '(dense-termlist) first-term)
  ;; pred
  (put 'empty-termlist? '(dense-termlist) empty-termlist?)
  (put 'the-empty-termlist 'dense-termlist (lambda () (tagme (the-empty-termlist)))))

(define (install-polynomial get put apply-fn)
  ;; import methods

  (define (=zero? a)
    (apply-fn '=zero? a))
  (define (equ? a b)
    (apply-fn 'equ? a b))
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

  ;; internal procedures
  (define (tagme p)
    (attach-tag 'polynomial p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))

  ;; terms
  (install-term-package get put apply-fn)
  (define (make-term order coeff)
    ((get 'make-from-order-coeff 'term) order coeff))
  (define (order term)
    (apply-fn 'order term))
  (define (coeff term)
    (apply-fn 'coeff term))

  ;; termlists
  (install-sparse-termlist-package get put apply-fn)
  (install-dense-termlist-package get put apply-fn)
  (define (make-term-list terms)
    ((get 'make-from-terms 'sparse-termlist) terms))
  (define (empty-termlist? termlist)
    (apply-fn 'empty-termlist? termlist))
  (define (the-empty-termlist termlist)
    ((get 'the-empty-termlist (type-tag termlist))))
  (define (first-term termlist)
    (apply-fn 'first-term termlist))
  (define (adjoin-term term termlist)
    (apply-fn 'adjoin-term term termlist))
  (define (rest-terms termlist)
    (apply-fn 'rest-terms termlist))

  ;; polys
  (define (ensure-termlist termlist)
    (if (or
          (equal? 'sparse-termlist (type-tag termlist))
          (equal? 'dense-termlist (type-tag termlist)))
      termlist
      (error "Unsupported type-tag for termlist --" termlist)))
  (define (make-poly variable term-list)
    (cons variable (ensure-termlist term-list)))
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
                          (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

  (define (neg-poly p)
    (make-poly (variable p)
               (apply-fn 'neg (term-list p))))

  (define (sub-terms L1 L2)
    (add-terms L1
               (apply-fn 'neg L2)))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-poly
        (variable p1)
        (sub-terms (term-list p1)
                   (term-list p2)))
      (error "Polys not in same var -- MUL-POLY" (list p1 p2))))


  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist L)
      (let ((t2 (first-term L)))
        (adjoin-term (make-term
                       (add (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
                     (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist L1)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY" (list p1 p2))))

  (define (div-terms L1 L2)
    (newline)
    (newline)
    (display (list "DIV-TERMS -- " L1 L2))
    (newline)
    (if (empty-termlist? L1)
      (list (the-empty-termlist L1)
            (the-empty-termlist L1))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist L1) L1)
          (let ((new-c (div (coeff t1) (coeff t2)))
                (new-o (sub (order t1) (order t2))))
            (newline)
            (display (list "NEW-TERM coeff --" new-c (type-tag (coeff t1))
                           (type-tag (coeff t2))))
            (let ((rest-of-result (div-terms
                                    (sub-terms L1
                                               (mul-terms
                                                 (make-term-list (list (make-term new-o new-c)))
                                                 L2))
                                    L2)))
              (list
                (adjoin-term
                  (make-term new-o new-c)
                  (car rest-of-result))
                (cadr rest-of-result))))))))

  (define (remainder-terms L1 L2)
    (let ((result-remainder-list (div-terms L1 L2)))
      (cadr result-remainder-list)))

  (define (gcd-terms a b)
    (newline)
    (newline)
    (display (list a b (empty-termlist? b)))
    (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

  (define (gcd-poly p1 p2)
    (newline)
    (newline)
    (display (list p1 p2))
    (if (same-variable? (variable p1)
                        (variable p2))
      (cons
        (variable p1)
        (gcd-terms (term-list p1)
                   (term-list p2)))
      (error "Polys not in same var -- DIV-POLY" (list p1 p2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (let ((result (div-terms (term-list p1)
                               (term-list p2))))
        (if (empty-termlist? (cadr result))
          (cons (variable p1)
                (car result))
          (cons (variable p1)
                result)))
      (error "Polys not in same var -- DIV-POLY" (list p1 p2))))

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
  (put 'sub '(polynomial polynomial) (lambda (p1 p2)
                                       (tagme (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tagme (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (tagme (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tagme (make-poly var terms))))
  (put 'greatest-common-divisor '(polynomial polynomial) (lambda (a b) (tagme (gcd-poly a b))))

  'done)
