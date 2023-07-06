#lang racket

(require "../../../shared/sicp-stream.rkt")

(newline) (print "50") (newline)
(stream-map (lambda (a b) (+ 1 a b))
            (stream-enumerate-interval 0 10)
            (stream-enumerate-interval 0 10))

(newline) (print "51") (newline)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(newline)
(println x)
(stream-ref x 5)
(stream-ref x 7)

(newline) (print "52") (newline)
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(newline) (print "first even is 6 ") (display sum) (newline)
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(newline) (print "first divisable by 5 is 10, this would be 15 if memoization was not used.") (display sum) (newline)
(newline) (print "display-ref 7, this just prints the 7th of the filtered seq. Without memoization it would be hard to say what this value is, because it depends on the assignments to sum in the time before this call.") (newline)
(stream-ref y 7)
(newline) (print "display-stream")
(display-stream z)
