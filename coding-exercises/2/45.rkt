#lang racket
(require sicp-pict)

(define (split adjoiner splitter)
  (lambda (painter)
    (let ((splitted (splitter painter painter)))
      (adjoiner painter splitted))))

(define right-split (split beside below))
(define up-split (split below beside))
