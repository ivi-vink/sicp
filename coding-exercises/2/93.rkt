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
   (display rf)
   (newline)
   (display (add rf rf))))
