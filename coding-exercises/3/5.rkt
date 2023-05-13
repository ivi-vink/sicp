#lang racket
;; 5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (random-in-range low high)
  (let ((r (- high low)))
    (+ low (* r (random)))))

(define (estimate-integral P x1 x2 y1 y2 n)
  (* (monte-carlo n (lambda () (P (random-in-range x1 x2) (random-in-range y1 y2))))
     (* (- x2 x1)
        (- y2 y1))))

(estimate-integral
  (lambda (x y) (<= (+ (sqr (- x 5)) (sqr (- y 7))) 1))
  4 6
  6 8
  100)

;; 6
(define (make-rand seed)
  (define (rand-update)
    (random))
  (define (rand-reset reset)
    (random-seed reset))
  (define (dispatch m . args)
    (cond ((eq? m 'generate) (rand-update))
          ((eq? m 'reset) (rand-reset (car args)))))
  dispatch)
(define rand (make-rand 43))
(rand 'generate)
(rand 'reset 42)
