(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fie a b n)
  (cond ((= n 0) a)
        ((even? n) (fie a (square b) (/ n 2)))
        (else  (fie (* a b) b (- n 1)))))

(define (fast-expt b n)
  (fie 1 b n))

(fast-expt 5 5)
