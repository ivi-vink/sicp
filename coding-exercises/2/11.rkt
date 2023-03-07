#lang racket
(require "../../shared/intervals.rkt")

;; Could be written to use less comparisons with nested cond, but this is more readable.
(define (pos? x)
  (> x 0))
(define (neg? x)
  (< x 0))
(define (mul-interval x y)
  (cond
    ;; 1. all positive
    ((and
       (pos? (lower-bound x))
       (pos? (lower-bound y))
       (pos? (upper-bound x))
       (pos? (upper-bound y)))
     (make-interval (* (lower-bound x) (lower-bound y))
                    (* (upper-bound x) (upper-bound y))))
    ;; 2. one lower-bound neg
    ((and
       (neg? (lower-bound x))
       (pos? (lower-bound y))
       (pos? (upper-bound x))
       (pos? (upper-bound y)))
     (make-interval (* (lower-bound x) (upper-bound y))
                    (* (upper-bound x) (upper-bound y))))
    ;; 3. one lower-bound neg
    ((and
       (pos? (lower-bound x))
       (neg? (lower-bound y))
       (pos? (upper-bound x))
       (pos? (upper-bound y)))
     (make-interval (* (upper-bound x) (lower-bound y))
                    (* (upper-bound x) (upper-bound y))))
    ;; 4. one interval neg
    ((and
       (neg? (lower-bound x))
       (pos? (lower-bound y))
       (neg? (upper-bound x))
       (pos? (upper-bound y)))
     (make-interval (* (upper-bound x) (upper-bound y))
                    (* (lower-bound x) (lower-bound y))))
    ;; 5. one interval neg
    ((and
       (pos? (lower-bound x))
       (neg? (lower-bound y))
       (pos? (upper-bound x))
       (neg? (upper-bound y)))
     (make-interval (* (upper-bound x) (upper-bound y))
                    (* (lower-bound x) (lower-bound y))))
    ;; 6. one interval neg, one interval crossing zero
    ((and
       (neg? (lower-bound x))
       (neg? (lower-bound y))
       (neg? (upper-bound x))
       (pos? (upper-bound y)))
     (make-interval (* (upper-bound x) (upper-bound y))
                    (* (upper-bound x) (lower-bound y))))
    ;; 7. one interval neg, one interval crossing zero
    ((and
       (neg? (lower-bound x))
       (neg? (lower-bound y))
       (pos? (upper-bound x))
       (neg? (upper-bound y)))
     (make-interval (* (upper-bound x) (upper-bound y))
                    (* (upper-bound x) (lower-bound y))))
    ;; 8. all neg
    ((and
       (neg? (lower-bound x))
       (neg? (lower-bound y))
       (neg? (upper-bound x))
       (neg? (upper-bound y)))
     (make-interval (* (lower-bound x) (lower-bound y))
                    (* (upper-bound x) (upper-bound y))))
    ;; 9. both crossing zero
    ((and
       (neg? (lower-bound x))
       (neg? (lower-bound y))
       (pos? (upper-bound x))
       (pos? (upper-bound y)))
     (make-interval ((lambda (a b) (if (< a b) a b))
                     (* (lower-bound x) (upper-bound y))
                     (* (upper-bound x) (lower-bound y)))
                    ((lambda (a b) (if (> a b) a b))
                     (* (upper-bound x) (upper-bound y))
                     (* (lower-bound x) (lower-bound y)))))))

(mul-interval
  (make-interval -3 3)
  (make-interval -4 4))
