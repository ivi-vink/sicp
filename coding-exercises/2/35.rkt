#lang racket
(require "../../shared/lists.rkt")

(define (count-leaves t)
  (accumulate 
    (lambda (x y) 
      (+ x y)) 
    0 
    (map
     (lambda (x) 
       (if (pair? x)
         (count-leaves x)
         1)) t)))
                      
(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
((lambda ()
   (display "testing")
   (newline)
   (display (count-leaves test-tree))))
