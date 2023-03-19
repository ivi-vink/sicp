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
  segments->painter
  transform-painter
  beside
  einstein
  flip-horiz
  flip-vert
  <-rotate180
  <-rotate270
  enumerate-corners
  enumerate-segments
  close-line
  line-points->segments
  up-split
  right-split
  below
  square-of-four)
(require sicp-pict "lists.rkt")

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


(define (enumerate-corners frame)
  (list
    (origin-frame frame)
    (edge1-frame frame)
    (add-vect
      (edge1-frame frame)
      (edge2-frame frame))
    (edge2-frame frame)))

(define (close-line line-points)
  (append line-points (list (car line-points))))

(define (enumerate-segments frame)
  (map
    (lambda (pair)
      (make-segment (car pair) (cadr pair)))
    (enumerate-windows
      (close-line (enumerate-corners frame))
      2)))

(define (line-points->segments points)
  (map
   (lambda (pair)
     (make-segment (car pair) (cadr pair)))
   (enumerate-windows
     points
     2)))


(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside smaller (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below smaller (beside smaller smaller)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((t (beside (tl painter) (tr painter)))
          (b (beside (bl painter) (br painter))))
      (below b t))))
