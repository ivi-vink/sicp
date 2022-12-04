#lang sicp
(define (myfun n)
  (cond ((< n 3) n)
        (else (+ (myfun (- n 1))
                 (* 2 (myfun (- n 2)))
                 (* 3 (myfun (- n 3)))))))
(myfun 11)

(define (myfun2 n)
  (define (mf i a b c)
    (cond ((< n 3) n)
          ((> i n) a)
          (else (mf (+ i 1) 
                    (+ a (* 2 b) (* 3 c))
                    a
                    b))))
  (mf 3 2 1 0))

(myfun 11)
