#lang racket
(require "../../shared/data-directed-programming.rkt")
(require "./83/install.rkt")
;; arithmetic package
(define get-put-apply (install-arithmetic-package))
(define get (car get-put-apply))
(define put (cadr get-put-apply))
(define apply-fn (caddr get-put-apply))

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
