#lang racket
;; implements convential interfaces on sets represented as binary trees
(provide
  entry
  left-branch
  right-branch
  make-entry
  element-of-set?
  adjoin-set
  tree->list
  list->tree
  union-set
  intersection-set)

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
  (cond ((null? mset) (make-entry x '() '()))
        ((= x (entry mset)) mset)
        ((< x (entry mset))
         (make-entry
           (entry mset)
           (adjoin-set x (left-branch mset))
           (right-branch mset)))
        ((> x (entry mset))
         (make-entry
           (entry mset)
           (left-branch mset)
           (adjoin-set x (right-branch mset))))))

(define (tree->list tree)
  (define (copy-to-list t result-list)
    (if (null? t)
      result-list
      (copy-to-list (left-branch t)
                    (cons (entry t)
                          (copy-to-list (right-branch t)
                                        result-list)))))
  (copy-to-list tree '()))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-entry this-entry left-tree right-tree)
                    remaining-elts))))))))

;; 2*O(n) + O(n) + O(n)
(define (union-set s1 s2)
  (define (ordered-list-union-set set1 set2)
      (cond
        ((and (null? set1) (null? set2)) '())
        ((null? set1) (cons (car set2) (ordered-list-union-set set1 (cdr set2))))
        ((null? set2) (cons (car set1) (ordered-list-union-set (cdr set1) set2)))
        ((= (car set1) (car set2)) (cons (car set2) (ordered-list-union-set (cdr set1) (cdr set2))))
        ((> (car set1) (car set2)) (cons (car set2) (ordered-list-union-set set1 (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (ordered-list-union-set (cdr set1) set2)))))
  (list->tree
    (ordered-list-union-set (tree->list s1) (tree->list s2))))

;; 2*O(n) + O(n) + O(n)
(define (intersection-set set1 set2)
  (define (ordered-list-intersection-set s1 s2)
    (if (or (null? s1) (null? s2))
      '()
      (let ((x1 (car s1)) (x2 (car s2)))
        (cond ((= x1 x2)
               (cons x1
                     (ordered-list-intersection-set
                       (cdr s1)
                       (cdr s2))))
              ((< x1 x2)
               (ordered-list-intersection-set
                 (cdr s1)
                 s2))
              ((> x1 x2)
               (ordered-list-intersection-set
                 s1
                 (cdr s2)))))))
  (list->tree
    (ordered-list-intersection-set
        (tree->list set1) (tree->list set2))))

