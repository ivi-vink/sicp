#lang racket
(require "../../shared/sets.rkt")

;; just use id as key
(define (key x)
  (if (number? x)
    x
    (error "Only supports numericals that we can compare with >, <, = -- KEY")))

(define (list-lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (list-lookup given-key (cdr set-of-records)))))

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
    false
    (let ((v (key (car set-of-records))))
      (cond ((= given-key v) v)
            ((< given-key v)
             (lookup given-key (left-branch set-of-records)))
            ((> given-key v)
             (lookup given-key (right-branch set-of-records)))))))

((lambda ()
  (println "list -- LOOKUP")
  (println (list-lookup 5 (list 1 2 3 4 5)))
  (newline)
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
  (println (lookup 10
                   (union-set
                     (list->tree (tree->list test216a))
                     (list->tree (tree->list test216b)))))
  (newline)))
