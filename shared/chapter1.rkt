#lang racket
(provide 
  iterative-improve
  close-enough?
  repeated
  fixed-point
  average
  average-damp
  smoother
  square
  cube
  power
  compose
  double
  deriver
  golden-ratio
  miller-raban-test
  all-miller-raban
  coprimer
  filtered-accumulate
  product
  sum
  simpson
  fermat?
  all-fermat
  expmoder
  divides?
  find-divisor
  smallest-divisor)
(require sicp)

;;abstract procedures
(define (iterative-improve good-enuf? improve)
  (lambda (guess)
    (define (try g)
      (let ((next (improve g)))
         (if (good-enuf? next g)
           next
           (try next))))
    (try guess)))

(define (filtered-accumulate pred combiner null-value term a next b)
  (define (iter a result)
    (cond
      ((> a b) result)
      ((pred a)
       (iter (next a) (combiner (term a) result)))
      (else (iter (next a) result))))
  (iter a null-value))

;; basic
(define (cube x) (* x x x))
(define (square x) (* x x))
(define (power x n)
  (cond
    ((= n 0) 1)
    ((= n 1) x)
    (else (* x (power x (dec n))))))
(define (close-enough? tolerance)
  (lambda (x y)
    (< (abs (- x y)) tolerance)))
(define (average a b)
  (/ (+ a b) 2))
(define (divides? a b)
  (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond 
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor 
            n 
            ((lambda (td) (if (= td 2) 3 (+ td 2))) 
             test-divisor)))))
(define (smallest-divisor n)
  (find-divisor n 2))

;; functions
(define (double f)
  (lambda (x)
    (f (f x))))
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
    (lambda (x) x)
    (lambda (x) 
      (define (iter result i)
          (if (> i n)
            result
            (iter (f result) (inc i))))
      (iter x 1))))

(define (average-damp f n)
  (lambda (guess)
   (let ((next (f guess)))
      ((repeated
        (lambda (g) (average guess g))
        n) next))))

;; math
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (product term a next b)
  (define (iter result a)
    (if (> a b)
      result
      (iter (* (term a) result) (next a))))
  (iter 1 a))

(define (smoother dx)
  (lambda (f)
    (lambda (x)
      (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))

(define (deriver dx)
  (lambda (f)
    (lambda (x)
        (/ (- (f (+ x dx)) (f x)) 
           dx))))

(define (coprimer n)
   (lambda (a)
     (if (= 1 (gcd a n))
       ((lambda ()
          (println a)
          true))
       false)))

(define (fixed-point f first-guess)
  ((iterative-improve
     (close-enough? 0.0001)
     f)
   first-guess))

(define (golden-ratio)
  (fixed-point
    (lambda (x) (+ 1 (/ 1 x)))
    1.0))

(define (simpson f lower upper n)
  (define h (/ (- upper lower) n))

  (define (nth-term k)
    (f (+ lower (* k h))))

  (define (term k)
    (cond
      ((= k 0) (f lower))
      ((= k upper) (f upper))
      ((even? k) (* 2 (nth-term k)))
      (else (* 4 (nth-term k)))))

  (* (/ h 3.0)
     (sum term lower inc n)))

(define (expmoder signal)
  (define (expmod base e m)
    (cond
      ((= e 0) 1)
      ((even? e)
       (signal base e m (remainder (square (expmod base (/ e 2) m)) m)))
      (else
        (remainder (* base (expmod base (- e 1) m)) m))))
  expmod)

(define (fermat? a n)
  (= ((expmoder (lambda (b e m x) x)) a n n) a))

(define (all-fermat n)
  (define (f a n)
      (cond
        ((= a 0) true)
        ((fermat? (- a 1) n) (f (- a 1) n))
        (else false)))
  (f n n))

(define (miller-raban-test a n)
  (define (signal-mr b e m x)
      (cond
        ((= e (- m 1)) x) ;; end result
        ((= b (- m 1)) x) ;; base squared wil result in trivial root
        ((= x 1) 0) ;; non-trivial root
        (else x))) ;; no root found
  (= ((expmoder signal-mr) a (- n 1) n) 1))

(define (all-miller-raban n)
  (define (iter a n)
      (cond
        ((<= a 2) true)
        ((miller-raban-test (- a 1) n) (iter (- a 1) n))
        (else false)))
  (iter n n))
