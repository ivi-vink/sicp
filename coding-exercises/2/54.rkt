#lang racket

(define test-list '(this is a list))
(define nested-test-list '(this (is a) list))
(equal? test-list test-list)
(equal? nested-test-list nested-test-list)

;; Wouldn't it be interesting to define other equals? like only the leaves equal regardless of depth?
(define (rec-equal? seq1 seq2)
  (cond ((or (null? seq1) (null? seq2)) true)
        ((and (pair? (car seq1)) (pair? (car seq2)))
         (and (rec-equal? (car seq1) (car seq2))
              (rec-equal? (cdr seq1) (cdr seq2))))
        (else (and (eq? (car seq1) (car seq2))
                   (rec-equal? (cdr seq1) (cdr seq2))))))

(rec-equal? test-list test-list)
(rec-equal? nested-test-list test-list)
