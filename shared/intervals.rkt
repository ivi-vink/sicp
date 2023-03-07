#lang racket
(provide
  width
  make-center-width
  center
  make-center-percent
  percent
  pos?
  neg?
  mul-interval
  div-interval
  sub-interval
  add-interval
  make-interval
  upper-bound
  lower-bound
  print-interval)
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (make-center-percent c p)
  (let ((toler (* c (/ p 100.0)))) 
    ((lambda (a b) 
       (if (> a b) 
         (make-interval b a)
         (make-interval a b)))
     (- c toler) 
     (+ c toler))))
(define (percent i)
  (let ((w (width i))
        (c (center i)))
    ;; (newline) (print c)(println w)
    ;; not defined: percent of interval centered at zero
    (if
      (<= c 0)
      0
      (abs (* 100 (/ w c))))))
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
                     (* (lower-bound x) (lower-bound y)))))
    ;; 10. The case where one of the endpoints is neither negative or positive, 0
    (else (mul-interval-min-max x y))))
(define (mul-interval-min-max x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (and (< 0 (lower-bound y)) (> 0 (upper-bound y)))
    (error "Division by interval spanning zero")
    (mul-interval 
      x
      (make-interval 
        (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y))))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))
(define (print-interval x)
  (newline)
  (display "interval{")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "}")
  (newline))

