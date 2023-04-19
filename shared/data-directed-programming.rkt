#lang racket
(provide
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
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

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
    (display dispatch-table))
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

(define (make-apply put get)
  (lambda (op . args)
     (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
         (if proc
           (apply proc (map contents args))
           (error
             "No method for these types -- APPLY-GENERIC"
             (list op type-tags)))))))
