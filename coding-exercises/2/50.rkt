#lang racket
(require "../../shared/pict.rkt")
(require "../../shared/lists.rkt")
(require sicp-pict)

(define (flip-horiz painter)
  (transform-painter
    painter
    (make-vect 1 0)
    (make-vect 0 0)
    (make-vect 1 1)))

(define (flip-vert painter)
  (transform-painter
   painter
   (make-vect 0 1)
   (make-vect 1 1)
   (make-vect 0 0)))

(define (<-rotate180 painter)
  (flip-horiz (flip-vert painter)))
(define (<-rotate270 painter)
  (transform-painter
   painter
   (make-vect 0 1)
   (make-vect 0 0)
   (make-vect 1 1)))

(paint einstein)
(paint (flip-horiz einstein))
(paint (flip-vert einstein))
(paint (<-rotate180 einstein))
(paint (<-rotate270 einstein))
