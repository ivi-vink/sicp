#lang racket
(require "2.rkt")

(define (perimeter rectangle)
  (* 2 (+ (len (width rectangle)) (len (height rectangle)))))
(define (area rectangle) 
  (* (len (width rectangle)) (len (height rectangle)))) 

;; (define (make-rectangle w h)
;;  (cons w h))
;; (define (width rectangle)
;;   (car rectangle))
;; (define (height rectangle)
;;   (cdr rectangle))

(define (make-rectangle w h)
  (lambda (pick)
    (if pick w h)))
(define (width rectangle)
  (rectangle true))
(define (height rectangle)
  (rectangle false))


(define test-rectangle
  (make-rectangle
    (make-segment
      (make-point 0 0) (make-point 2 0))
    (make-segment
      (make-point 0 0) (make-point 0 2))))

(define (print-rectangle r)
  (newline)
  (display "rectangle{")
  (display "width:")
  (display (len (width r)))
  (display ",")
  (display "height:")
  (display (len (height r)))
  (display ",")
  (display "area:")
  (display (area r))
  (display ",")
  (display "perimeter:")
  (display (perimeter r))
  (display "}"))
  
(define (len segment)
  (sqrt 
    (+ (square 
         (- (x-point (end-segment segment)) (x-point (start-segment segment))))
       (square 
         (- (y-point (end-segment segment)) (y-point (start-segment segment)))))))

(define test-len 
  (len (make-segment (make-point 0 2) (make-point 0 0))))

(print-rectangle test-rectangle)

