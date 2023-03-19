#lang racket
(require "../../shared/pict.rkt")

(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((top-painter
            (transform-painter
              painter2
              split-point
              (make-vect 1 0.5)
              (make-vect 0 1)))
          (bot-painter
            (transform-painter
              painter1
              (make-vect 0 0)
              (make-vect 1 0)
              split-point)))
      (lambda (frame)
        (top-painter frame)
        (bot-painter frame)))))

(define (below2 painter1 painter2)
  (<-rotate180
    (<-rotate270
     (beside (<-rotate270 painter1)
             (<-rotate270 painter2)))))

(paint (below einstein einstein))
(paint (below2 einstein einstein))
