#lang racket

;; (apply-generic 'magnitude z)
;; This strips the complex type-tag from z and passes the contents to the generic magnitude procedure defined in the complex package.
;; The magnitude procedure is responsible for dispatching the operation to the magnitude procedure of either the rectangular or the polar form representation.
;; (apply-generic 'magnitude z)
;; This strips away the rectangular type tag of the args and passes the contents of the args to the corresponding 'magnitude '(rectangular) procedure.
;; In the end the rectangular magnitude procedure's return value is bubbled up to the calling function.
(print "hi")
