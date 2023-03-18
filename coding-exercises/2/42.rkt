#lang racket
(require "../../shared/lists.rkt")

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (safe? k positions)
  (let ((maybe-new-queen (car positions))
        (rest-of-queens (cdr positions)))
     (fold-right (lambda (queen safe)
                   (let ((dx (abs (- (car queen) (car maybe-new-queen)))))
                     (and
                       safe
                       (not (or (= dx 0) (= dx (- k (cadr queen))))))))
                true
                rest-of-queens)))

((lambda ()
   (newline)
   (display "safe?-test")
   (newline)
   (display (safe? 2 (list (list 1 2) (list 1 1) (list 0 0))))
   (display (safe? 2 (list (list 1 2) (list 1 1) (list 1 0))))
   (display (safe? 2 (list (list 1 2) (list 1 1) (list 2 0))))
   (display (safe? 2 (list (list 5 2) (list 1 1) (list 3 0))))
   (display (safe? 6 (list (list 6 6) (list 5 5) (list 4 4))))))

(define empty-board '())

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 8))
