#lang racket
(require "../../shared/lists.rkt")

(define (ordered-triples-sum n s)
  (if (< n 3)
    '()
    (filter (lambda (triplet)
              (= (fold-right + 0 triplet) s))
            (flatmap (lambda (i)
                       (flatmap (lambda (pair)
                                 (permutations (cons i pair)))
                        (unique-pairs (- i 1))))
              (enumerate-interval 3 n)))))

((lambda ()
   (newline)
   (display (enumerate-interval 3 3))
   (newline)
   (display (ordered-triples-sum 4 8))))
