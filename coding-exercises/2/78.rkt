#lang racket
(require "../../shared/data-directed-programming.rkt")
;; We are basically making a data directed framework for arithmethic operations in this module
(define pkg (make-dispatch-table))
(define put (putter pkg))
(define get (getter pkg))
(define apply-generic (make-apply put get))

