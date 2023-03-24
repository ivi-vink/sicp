#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-entry entry left right)
  (list entry left right))

(define (element-of-set? x mset)
  (cond ((null? mset) false)
        ((= x (entry mset)) true)
        ((> x (entry mset))
         (element-of-set? (right-branch mset)))
        ((< x (entry mset))
         (element-of-set? (left-branch mset)))))
  
(define (adjoin-set x mset)
  (cond ((null? mset) (make-tree x '() '()))
        ((= x (entry mset)) mset)
        ((< x (entry mset))
         (make-tree
           (entry mset)
           (adjoin-set x (left-branch mset))
           (right-branch mset)))
        ((> x (entry))
         (make-tree
           (entry mset)
           (left-branch mset)
           (adjoin-set x (right-branch mset))))))

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))
  

;; depth of a balanced tree with n elements is log n.. right? Because every level you need 2*(level-1) elements, so the total levels is how many time we can do that step until 2*levels > n.
;; So it is the log a where a is the smallest number that only has factor 2 and > n.
;; Both procedure 1 and 2 are levelwise reducing the problem.
;; Only the overhead spent at each level is greater for 1, because append is used in the linear recursive process.
;; Since at every level we do a linear scan of all left elements, it is n*logn.
;; 1 is a linear iterative procedure and has almost no overhead at every level except a cons operation so it is logn.
((lambda ()
  (define test216a (make-entry 
                     7
                     (make-entry 
                       3
                       (make-entry
                         1 '() '())
                       (make-entry 5 '() '()))
                      
                     (make-entry
                       9
                       '()
                       (make-entry
                         11
                         '()
                         '()))))
  (println (tree->list-1 test216a))
  (println (tree->list-2 test216a))
  (newline)
  (define test216b (make-entry 
                     3
                     (make-entry 1 '() '())
                     (make-entry 
                       7
                       (make-entry 5 '() '())
                       (make-entry
                         9
                         '()
                         (make-entry
                           11
                           '()
                           '())))))
  (println (tree->list-1 test216b))
  (println (tree->list-2 test216b))
  (newline)
  (define test216c (make-entry 
                     5
                     (make-entry 3 (make-entry 1 '() '()) '())
                     (make-entry 
                       9
                       (make-entry 7 '() '())
                       (make-entry
                         11
                         '()
                         '()))))
  (println (tree->list-1 test216c))
  (println (tree->list-2 test216c))
  (newline)))
