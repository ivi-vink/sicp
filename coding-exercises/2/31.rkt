#lang racket
(define (tree-map fn tree)
  (map (lambda (subtree)
         (if (pair? subtree)
           (tree-map fn subtree)
           (fn subtree)))
       tree))
  
(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
((lambda ()
   (display (tree-map (lambda (x) (* x x)) test-tree))))
