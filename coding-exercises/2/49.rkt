#lang racket
(require "../../shared/pict.rkt")
(require "../../shared/lists.rkt")

(define (outline frame)
  (let ((corner
          (add-vect
            (edge1-frame frame)
            (edge2-frame frame)))
        (origin (origin-frame frame))
        (e1 (edge1-frame frame))
        (e2 (edge2-frame frame)))
    ((segments->painter
       (list (make-segment origin e1)
             (make-segment e1 corner)
             (make-segment corner e2)
             (make-segment e2 origin))) frame)))
(paint outline)

(define (x frame)
  ((segments->painter
     (list (make-segment (origin-frame frame)
                         (add-vect
                           (edge1-frame frame)
                           (edge2-frame frame)))
           (make-segment (edge1-frame frame)
                         (edge2-frame frame))))
   frame))
(paint x)

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (let ((midpoint
            (make-vect
              (/ (+ (xcor-vect end) (xcor-vect start)) 2)
              (/ (+ (ycor-vect end) (ycor-vect start)) 2))))
      midpoint)))

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

(define (diamond frame)
  ((segments->painter
     (line-points->segments
       (close-line
         (map
           midpoint-segment
           (enumerate-segments frame)))))
   frame))
(paint diamond)

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
            (make-vect 0.34 0))))
    ((segments->painter
               (append
                 (enumerate-segments frame)
                 (line-points->segments
                  (close-line
                    wave-points)))) frame)))
(paint wave)
