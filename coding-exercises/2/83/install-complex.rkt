#lang racket
(provide install-complex)
(require "../78/install-complex-package.rkt"
         "../../../shared/data-directed-programming.rkt")

(define (install-complex apply-and-drop get put)
    (install-complex-package apply-and-drop get put)
    (put 'project '(complex) (lambda (z)
                               ((get 'make 'real) (apply-and-drop 'real-part z)))))
