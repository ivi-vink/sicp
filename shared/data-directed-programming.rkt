#lang racket
(provide
  make-apply-with-raising-and-drop
  make-apply-with-coercion
  make-apply-pred
  make-apply
  make-dispatch-table
  printer
  getter
  putter
  attach-tag
  type-tag
  contents
  find-type)
(require "./lists.rkt")

;; Type tagged data
(define (attach-tag type-tag contents)
  (cond ((exact-integer? contents) contents)
        ((inexact-real? contents) contents)
        ((number? contents) contents)
        ((symbol? contents) contents)
        (else (cons type-tag contents))))
(define (type-tag datum)
  (cond
    ((pair? datum) (car datum))
    ((symbol? datum) 'symbol)
    ((exact-integer? datum) 'integer)
    ((inexact-real? datum) 'real)
    ((number? datum) 'scheme-number)
    ((boolean? datum) 'boolean)
    (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond
    ((pair? datum) (cdr datum))
    ((number? datum) datum)
    ((symbol? datum) datum)
    (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (find-type type seq)
  (define (rec items)
   (cond ((null? items) false)
         ((equal? type (type-tag (car items))) (car items))
         (else (rec (cdr items)))))
  (rec seq))


;; dispatch table abstraction barrier/ encapsulation
;; ('op (list ('(types) item)))
(define (make-dispatch-table)
  (define dispatch-table '())
  (define (printer)
    (newline)
    (println dispatch-table))
  (define (get op types)
    (let ((op-datum (find-type op dispatch-table)))
      (if op-datum
        (let ((proc-datum (find-type types (contents op-datum))))
          (if (pair? proc-datum)
            (contents proc-datum)
            false))
        false)))

  (define (update-op-datum op-datum types proc)
    (attach-tag (type-tag op-datum)
                (if (find-type types (contents op-datum))
                  (map (lambda (proc-datum)
                        (if (not (eq? (type-tag proc-datum) types))
                          proc-datum
                          (attach-tag (type-tag proc-datum) proc)))
                       (contents op-datum))
                  (cons (attach-tag types proc) (contents op-datum)))))

  (define (put op types item)
    (if (find-type op dispatch-table)
      (set! dispatch-table
        (map (lambda (op-datum) ;;just copy the table for now, don't want to mutate yet
               (if (not (equal? (type-tag op-datum) op))
                 op-datum
                 (update-op-datum op-datum types item)))
             dispatch-table))
      (set! dispatch-table (cons
                             (attach-tag op
                                (list (attach-tag types item)))
                             dispatch-table))))
  (list dispatch-table get put printer))

(define (getter t)
  (cadr t))
(define (putter t)
  (caddr t))
(define (printer t)
  (cadddr t))

(define (make-apply get)
  (lambda (op . args)
     (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
         (if proc
           (apply proc (map contents args))
           (error
             "No method for these types -- APPLY-GENERIC"
             (list op type-tags)))))))

(define (make-apply-pred get)
  (lambda (op . args)
     (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
         (if proc
           (apply proc (map contents args))
           false)))))

(define (make-apply-pred-symbol get)
  (lambda (op . args)
     (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
         (if proc
           (apply proc (map contents args))
           false)))))

(define (make-apply-with-coercion get get-coercion)
  (define (make-apply get)
    (lambda (op . args)
       ;; (display args)
       (let ((type-tags (map type-tag args)))
         (let ((proc (get op type-tags)))
           (if proc
             (apply proc (map contents args))
             false)))))
  (define apply-generic (make-apply get))

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
        (error "no method for these types --" (list op type-tags))
        (let ((coerced-args (coerce-or-fail (type-tag (car havent-tried)) args)))
          (if coerced-args
            (apply apply-generic (cons op coerced-args))
            (iter (cdr havent-tried))))))
    (iter type-tags))

  (lambda (op . args)
     ;; (display args)
     (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
         (if proc
             (apply proc (map contents args))
             (try-coerce type-tags op args))))))

;; Generic apply that can raise and drop methods in a tower of types
(define (type-match? args)
  (not (find-first (lambda (x)
                     (not (equal? (type-tag x)
                                  (type-tag (car args)))))
                   args)))
(define (count-raises-until-top get datum)
  (define (iter i raised)
    (let ((proc (get 'raise (list (type-tag raised)))))
      (if proc
        (iter (+ i 1) (proc (contents raised)))
        i)))
  (iter 0 datum))

(define (highest-type get items)
  (cdr
    (foldl
     (lambda (raises item result)
       (cond ((< (car result) 0) (cons raises item))
             ((< raises (car result)) (cons raises item))
             (else result)))
     (cons -1 'nil)
     (map (lambda (x)
            (count-raises-until-top get x))
          items)
     (map type-tag items))))

(define (raise-until get type datum)
  (cond ((equal? type (type-tag datum)) datum)
        (else (let ((proc (get 'raise (list (type-tag datum)))))
                (if proc
                  (raise-until get type (proc (contents datum)))
                  false)))))

(define (raise-until-type-match get type items)
  (cond ((null? items) '())
        (else (let ((result (raise-until get type (car items))))
                (if result
                  (cons result (raise-until-type-match get type (cdr items)))
                  (error "Could not raise type --" (list type items)))))))

; (raise-until-type-match (make-apply-pred get)
;                         (highest-type (make-apply-pred get) (list 1 test-complex))
;                         (list 1 test-complex)))))))


(define (make-apply-with-raising apply-generic get)
  (lambda (op . args)
    (let ((result (apply apply-generic (cons op args))))
      (if result
        result
        (let ((raised-args (raise-until-type-match get (highest-type get args) args)))
          (let ((raised-result (apply apply-generic (cons op raised-args))))
            (if raised-result
               raised-result
               (error "Could not apply --" (list op args raised-args)))))))))

(define (make-apply-with-raising-and-drop get)
  (define local-apply (make-apply get))

  (define (raisetower datum)
    (local-apply 'raise datum))

  (define (project datum)
    (local-apply 'project datum))

  (define (equ? d1 d2)
    (local-apply 'equ? d1 d2))

  (define (can-drop? datum)
    (let ((dropped (project datum)))
      (let ((raised (raisetower dropped)))
        (and (get 'equ? (list (type-tag raised) (type-tag datum)))
             (equ? raised datum)))))

  (define (projectable? datum)
    (get 'project (list (type-tag datum))))
  (define (towerdrop datum)
    (if (projectable? datum)
      (cond ((can-drop? datum)
             (towerdrop (project datum)))
            (else datum))
      datum))

  (lambda (op . args)
    (let ((proc (get op (map type-tag args))))
      (if proc
        (towerdrop (apply proc (map contents args)))
        (let ((raised-args (raise-until-type-match get (highest-type get args) args)))
          (newline)
          (display (list "RAISED -- " raised-args))
          (newline)
          (let ((raised-proc (get op (map type-tag raised-args))))
            (if raised-proc
               (towerdrop (apply raised-proc (map contents raised-args)))
               (error "Could not apply --" (list op raised-args)))))))))
