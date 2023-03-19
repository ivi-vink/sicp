#lang racket
(require "../../shared/pict.rkt")

(define (wave frame)
  (let ((wave-points
          (list
            (make-vect 0.24 0)
            (make-vect 0.37 0.5)
            (make-vect 0.3 0.6)
            (make-vect 0.15 0.5)
            (make-vect 0 0.7)
            (make-vect 0 0.85)
            (make-vect 0.15 0.65)
            (make-vect 0.35 0.7)
            (make-vect 0.4 0.7)
            (make-vect 0.3 0.85)
            (make-vect 0.36 1)
            (make-vect 0.57 1)
            (make-vect 0.62 0.855)
            (make-vect 0.55 0.7)
            (make-vect 0.65 0.7)
            (make-vect 1 0.35)
            (make-vect 1 0.25)
            (make-vect 0.55 0.52)
            (make-vect 0.7 0)
            (make-vect 0.55 0)
            (make-vect 0.45 0.4)
            (make-vect 0.34 0)))
        (smile
          (list
            (make-vect 0.4 0.8)
            (make-vect 0.45 0.75)
            (make-vect 0.5 0.75)
            (make-vect 0.53 0.78))))
    ((segments->painter
               (append
                 (line-points->segments smile)
                 (line-points->segments
                   (close-line
                     wave-points)))) frame)))
(paint wave)

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left up)
            (bottom-right right)
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))
(paint (corner-split einstein 4))

(define (square-limit painter n)
  (let ((s (lambda (f)
             (lambda (painter) (f (corner-split (flip-horiz painter) n))))))
    ((square-of-four
      (s flip-horiz)
      (s (lambda (x) x))
      (s <-rotate180)
      (s flip-vert))
     painter)))

(paint (square-limit einstein 2))
