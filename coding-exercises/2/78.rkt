#lang racket
(require "../../shared/data-directed-programming.rkt")
(require "./78/complex-rectangular.rkt")
(require "./78/complex-polar.rkt")
;; We are basically making a data directed framework for arithmethic operations in this module
(define pkg (make-dispatch-table))
(define put (putter pkg))
(define get (getter pkg))
(define print-tbl (printer pkg))
(define apply-generic (make-apply put get))

(install-rectangular-package put)
(install-polar-package put)
