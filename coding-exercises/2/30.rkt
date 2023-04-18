#lang racket
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree subtree factor)
           (* sub-tree factor)))
       tree))

;; analogous to, but we now also have to test when to recurse in the middle of the tree
;; (define (square-list items)
;;   (if (null? items)
;;     (list)
;;     (cons (square (car items)) (square-list (cdr items)))))
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
((lambda ()
   (display (square-tree test-tree))))

;; Can I also pass some state combining sequence and recursive operations?
(define (square-tree-map tree)
  (map (lambda (subtree)
         (if (pair? subtree)
           (square-tree-map subtree)
           (* subtree subtree)))
       tree))
((lambda ()
   (display (square-tree-map test-tree))))
