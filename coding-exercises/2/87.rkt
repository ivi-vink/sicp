#lang racket
(require "../../shared/data-directed-programming.rkt")
(require "./83/install.rkt")
;; arithmetic package
(define get-put-apply (install-arithmetic-package))
(define get (car get-put-apply))
(define put (cadr get-put-apply))
(define apply-fn (caddr get-put-apply))

(define (install-term-package put)
  (define (tagme term)
    (attach-tag 'term term))
  (put 'make-from-order-coeff 'term (lambda (order coeff) (tagme (list order coeff))))
  (put 'order '(term) (lambda (term) (car term)))
  (put 'coeff '(term) (lambda (term) (cadr term))))

(define (install-sparse-termlist-package put)
  ;; methods imported from term package
  (define (order term)
    ((get 'order '(term)) term))
  (define (coeff term)
    ((get 'coeff '(term)) term))
  (define (make-term order coeff)
    ((get 'make-from-order-coeff 'term) order coeff))

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

(define (install-dense-termlist-package put)
  ;; methods imported from term package
  (define (order term)
    ((get 'order '(term)) term))
  (define (coeff term)
    ((get 'coeff '(term)) term))
  (define (make-term order coeff)
    ((get 'make-from-order-coeff 'term) order coeff))

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

(define (install-polynomial-package put get)
  ;; internal procedures
  (define (tagme p)
    (attach-tag 'polynomial p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))

  ;; terms
  (install-term-package put)
  (define (make-term order coeff)
    ((get 'make-from-order-coeff 'term) order coeff))
  (define (order term)
    (apply-fn 'order term))
  (define (coeff term)
    (apply-fn 'coeff term))

  ;; termlists
  (install-sparse-termlist-package put)
  (install-dense-termlist-package put)
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
                          (apply-fn 'add (coeff t1) (coeff t2)))
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
                       (+ (order t1) (order t2))
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
    (if (empty-termlist? L1)
      (list (the-empty-termlist L1) (the-empty-termlist L1))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist L1) L1)
          (let ((new-c (div (coeff t1) (coeff t2)))
                (new-o (- (order t1) (order t2))))
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

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (cons
        (variable p1)
        (div-terms (term-list p1)
                   (term-list p2)))
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
  'done)

(install-polynomial-package put get)
(define (term order coeff)
  ((get 'make-from-order-coeff 'term) order coeff))
(define (sparse-termlist . terms)
  ((get 'make-from-terms 'sparse-termlist) terms))
(define (dense-termlist . terms)
  ((get 'make-from-terms 'dense-termlist) terms))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define test-poly1 (make-polynomial 'x (sparse-termlist
                                         (term 1 test-integer))))
(define test-poly2 (make-polynomial 'x (sparse-termlist
                                         (term 100 test-complex)
                                         (term 2 test-real)
                                         (term 1 test-rat)
                                         (term 0 test-integer))))
(define test-poly3 (make-polynomial 'x (sparse-termlist
                                         (term 50 test-rat)
                                         (term 0 2))))
((lambda ()
   (newline)
   (display (add test-poly2 test-poly2))
   (newline)
   (display (mul test-poly2 test-poly2))
   (newline)
   (display (mul test-poly3 test-poly3))))

;;87
(=zero? test-poly3)
(=zero? (make-polynomial 'x (sparse-termlist
                              (term 1000 0))))

;; 88
;; what is meant with negation here? Negation of a number? Making a negative number?
;; Guess that would be handy if we need to subtract a lot of terms.
((lambda ()
   (newline)
   (display (sub test-poly1 test-poly3))
   (newline)
   (display (sub test-poly1 test-poly2))))

;; 89/90
;; First we made the polynomial package generic for sparse polys
;; Then we added dense polys as allowed types just lists where the length of the sublist until the term is the order of the term
;; When we do this, we can even put some heuristics to decide to save polys in the optimal format by scanning and reconstructing the term list during poly construction.
;; For now the type of the second argument is used if both become empty at the same time, otherwise the one with more terms is used
(define test-dense-poly (make-polynomial 'x (dense-termlist
                                              (term 10 3)
                                              (term 0 1))))
((lambda ()
   (newline)
   (display test-dense-poly)
   (newline)
   (display (add test-dense-poly test-poly3))
   (newline)
   (display (mul test-dense-poly test-poly3))
   (newline)
   (display (sub test-dense-poly test-poly3))))

;; 91
;; Fill in the gaps excercise, should be easy right? :^) (NOTE(mike): it wasn't.)
((lambda ()
   (newline)
   (display (div test-poly3 test-poly3))
   (newline)
   (display (div (make-polynomial 'x (dense-termlist (term 3 100)))
                 (make-polynomial 'x (dense-termlist (term 3 10)))))
   (newline)
   (display (div test-poly2 test-poly1))
   (newline)
   (newline)
   (display (div test-poly2 test-poly3))))
