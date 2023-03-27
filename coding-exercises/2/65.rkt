#lang racket
(require "../../shared/sets.rkt")

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

((lambda ()
  (println "tree -- UNION")
  (define test216a (make-entry
                     7
                     '()
                     '()))
  (define test216b (make-entry
                     4
                     (make-entry 1 '() '())
                     (make-entry
                       8
                       (make-entry 6 '() '())
                       (make-entry
                         10
                         '()
                         (make-entry
                           12
                           '()
                           '())))))
  (println (union-set
             (list->tree (tree->list test216a))
             (list->tree (tree->list test216b))))
  (println (intersection-set
             (list->tree (tree->list test216a))
             (list->tree (tree->list test216b))))
  (newline)))
