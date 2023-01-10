(define (halve a) (/ a 2))
(define (double a) (* a 2))

(define (miter s a b)
  (cond ((= b 0) s)
        ((even? b) (miter s (double a) (halve b)))
        (else (miter (+ s a) (double a) (halve (- b 1))))))
  

(define (m a b)
  (miter 0 a b))

(m 3 5)
