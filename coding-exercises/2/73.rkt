#lang racket
(require "../../shared/lists.rkt")

;; a. Number and variable are primitives that return a boolean.
;;      But what you want is a type assertion that has the same signature as the rest.

;;      The operator procedure for example returns the symbol that indicates whether the expression is a sum or product.
;;      Get fetches a procedure from the dispatch table that defines how to take the derivative of the expression. But that is not a problem because you can define 
;;      the derivatives of a number or variable as procedures. This is one step that needs to be done first.
;;
;;      The get and put procedures on the dispatch table just need to support the type-tag of the expression, which is not supported for booleans. (i think?)
;;      So you might type tag variables and expressions. But this changes the underlying representation of the expression. Which we are not interested in right now.

;; b.
(define (variable? x) (symbol? x))
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))
(define (make-eq-type? type)
  (lambda (d)
    (eq? (type-tag d) type)))

(define (make-dispatch-table)
  (define dispatch-table '())
  (define (get op type)
      (let ((op-entry (find-first (make-eq-type? op)
                                  dispatch-table)))
        (if (pair? op-entry)
          (let ((installed-types (cdr op-entry)))
            (let ((dispatch-proc-entry (find-first (make-eq-type? type)
                                                   installed-types)))
              (if (pair? dispatch-proc-entry)
                (cdr dispatch-proc-entry)
                (error "Bad op or op not defined for type -- GET" op type dispatch-proc-entry))))
          (error "Not found or bad entry -- GET" op type op-entry))))
  (define (put op type item)
    (if (find-first (make-eq-type? op) dispatch-table)
      (set! dispatch-table (map (lambda (op-entry) ;;just copy the table for now, don't want to mutate yet
                                  (if (not (eq? (type-tag op-entry) op))
                                    op-entry
                                    (attach-tag op
                                      (let ((installed-types (map (lambda (type-entry)
                                                                   (if (not (eq? (type-tag type-entry) type))
                                                                     type-entry
                                                                     (attach-tag type item)))
                                                              (cdr op-entry))))
                                        (if (find-first (make-eq-type? type) installed-types)
                                          installed-types
                                          (cons (attach-tag type item) installed-types))))))
                             dispatch-table))
      (set! dispatch-table (cons
                             (attach-tag op
                                (list (attach-tag type item)))
                             dispatch-table))))
  (list dispatch-table get put))
(define (getter t)
  (cadr t))
(define (putter t)
  (caddr t))

;; prefix combination notation of expression? (+ a b)
(define (operator ex)
  (car ex))

(define (operands ex)
  (cdr ex))

(define t (make-dispatch-table))
(define get (getter t))
(define put (putter t))
(define (deriv ex var)
  (cond ((number? ex) 0)
        ((variable? ex) (if (same-variable? ex var) 1 0))
        (else ((get 'deriv (operator ex)) (operands ex) var))))

(define (=number? x num)
  (and (number? x) (= x num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1)
              (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (addend s) (car s))
(define (augend s)
  (cond ((null? (cddr s)) (cadr s))
        (else (cons '+ (cdr s)))))

(put 'deriv '+ (lambda (ex var)
                 (make-sum
                   (deriv (addend ex) var)
                   (deriv (augend ex) var))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (multiplier p)
  (car p))
(define (multiplicand p)
  (cond ((null? (cddr p))
         (cadr p))
        (else (cons '* (cdr p)))))

(put 'deriv '* (lambda (ex var)
                 (make-sum
                   (make-product
                     (multiplier ex)
                     (deriv (multiplicand ex) var))
                   (make-product
                     (deriv (multiplier ex) var)
                     (multiplicand ex)))))
(deriv '(+ (* 3 x) (* 2 x)) 'x)
