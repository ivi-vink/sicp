#lang racket
(require sicp)

(define (for-each action items)
  (define (iter things)
    (action (car things))
    (if (not (null? (cdr things)))
      (iter (cdr things))))
  (iter items))

(for-each (lambda (x) (newline) (display x))
          (list 1 2 3 4 5))
      
  
