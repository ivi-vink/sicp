#lang racket
(provide
  make-vect
  xcor-vect
  ycor-vect
  add-vect
  sub-vect
  scale-vect
  test-vect
  origin-frame
  edge1-frame
  edge2-frame
  test-frame
  make-segment
  start-segment
  end-segment
  paint
  segments->painter)
(require sicp-pict)
(require "lists.rkt")

(define (xcor-vect v)
  (vector-xcor v))
(define (ycor-vect v)
  (vector-ycor v))

(define (add-vect . v)
  (make-vect
    (fold-right + 0 (map xcor-vect v))
    (fold-right + 0 (map ycor-vect v))))
(define (sub-vect . v)
  (let ((xcors (map xcor-vect v))
        (ycors (map ycor-vect v)))
   (make-vect
     (- (car xcors) (fold-right + 0 (cdr xcors)))
     (- (car ycors) (fold-right + 0 (cdr ycors))))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
(define test-vect (make-vect 1 2))

(define (origin-frame frame)
  (frame-origin frame))
(define (edge1-frame frame)
  (frame-edge1 frame))
(define (edge2-frame frame)
  (frame-edge2 frame))
(define test-frame (make-frame
                     (make-vect 1 2)
                     (make-vect 2 4)
                     (make-vect 3 6)))

(define (start-segment s)
  (segment-start s))
(define (end-segment s)
  (segment-end s))
