#lang racket
(require (only-in sicp-pict 
                  paint 
                  segments->painter))
(require "../../shared/pict.rkt")

(define (outline frame)
  (display frame)
  (let ((corner 
          (add-vect 
            (edge1-frame frame)
            (edge2-frame frame)))
        (origin (origin-frame frame))
        (e1 (edge1-frame frame))
        (e2 (edge2-frame frame)))
    (segments->painter 
      (list (make-segment origin e1)
            (make-segment e1 corner)
            (make-segment corner e2)
            (make-segment e2 origin)))))
(paint outline)
