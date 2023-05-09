#lang racket
(require "../../shared/data-directed-programming.rkt")
(require "./83/install.rkt")
;; arithmetic package
(define get-put-apply (install-arithmetic-package))
(define get (car get-put-apply))
(define put (cadr get-put-apply))
(define apply-fn (caddr get-put-apply))

;; something
(define p1 (make-polynomial 'x (sparse-termlist
                                 (term 2 1) (term 0 1))))
(define p2 (make-polynomial 'x (sparse-termlist
                                 (term 3 1) (term 0 1))))
(define rf (make-rat p2 p1))
((lambda ()
   (newline)
   (newline)
   (newline)
   (newline)
   (display (list "RESULT --" rf))
   (newline)
   (newline)
   (display (list "RESULT --" (add rf rf)))
   (newline)
   (newline)
   (define test-p1 (make-polynomial 'x
                                    (sparse-termlist
                                      (term 4 1)
                                      (term 3 -1)
                                      (term 2 -2)
                                      (term 1 2))))
   (define test-p2 (make-polynomial 'x
                                    (sparse-termlist
                                      (term 3 1)
                                      (term 1 -1))))

   (display (list "RESULT GCD --" (greatest-common-divisor test-p1 test-p2)))))

;; 95
;; I represented reals as inexact decimals so our gcd breaks down here
;; Even more, I didn't bother with inexact gcd and the real gcd just returns the dividend
((lambda ()
   (define p1 (make-polynomial 'x
                               (sparse-termlist
                                 (term 2 1) (term 1 -2) (term 0 1))))
   (define p2 (make-polynomial 'x
                               (sparse-termlist
                                 (term 2 11) (term 0 7))))
   (define p3 (make-polynomial 'x
                               (sparse-termlist
                                 (term 1 13) (term 0 5))))
   (define q1 (mul p1 p2))
   (define q2 (mul p1 p3))
   (newline)
   (newline)
   (newline)
   (newline)
   (newline)
   (display q1)
   (newline)
   (display q2)
   (newline)
   (display (greatest-common-divisor q1 q2))))

;; 96
