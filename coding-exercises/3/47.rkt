#lang racket
(require ffi/unsafe/atomic)
(require compatibility/mlist)
(require "../../shared/lists.rkt")

(define (parallel-execute ps)
    (for-each
     thread-wait
     (map (lambda (thnk)
              (thread thnk))
         ps)))

(define (make-sleeper name)
    (define (random-sleeper)
     (let ((n 10))
         (map
          (lambda (i)
            (display name)
            (display " ")
            (displayln i)
            (sleep (random 0 2)))
          (enumerate-interval 0 n))))
    random-sleeper)

(define (clear! cell)
    (set-mcar! cell false))

(define (test-and-set! cell)
    (call-as-atomic
     (lambda ()
         (if (mcar cell)
           true
           (begin (set-mcar! cell true)
                  false)))))


(define (make-mutex-busy)
    (let ((cell (mlist false)))
        (define (the-mutex m)
            (cond ((eq? m 'acquire)
                   (when (test-and-set! cell)
                         (the-mutex 'acquire)))
                  ((eq? m 'release) (clear! cell))))
        the-mutex))

(define (make-mutex)
    (let ((the-mutex (make-semaphore 1)))
        (define (dispatch m)
            (cond ((eq? m 'acquire)
                   (semaphore-wait the-mutex))
                  ((eq? m 'release)
                   (semaphore-post the-mutex))))
        dispatch))

(define (make-serializer)
    (let ((mutex (make-mutex)))
        (lambda (p)
            (define (serialized-p . args)
                (mutex 'acquire)
                (let ((val (apply p args)))
                    (mutex 'release)
                    val))
            serialized-p)))

(when false
    (let ((m (make-mutex)))
       (parallel-execute
        (lambda ()
            (m 'acquire)
            (println "acquired")
            (sleep 5)
            (m 'release)
            (println "released"))
        (lambda ()
            (sleep 1)
            (m 'acquire)
            (println "acquired after other process")
            (m 'release)))))

;; In the book there are only examples that mention that you can use atomic operations on a single processor and for multiple processors there are special instructions that are atomic.
;; But in the racket manual it seems like there is no mention of two processes sharing the same memory space (there is places which run as separate os processes, potentially on a different cpu native processor).
;; So we could get away with atomic mode instead of the n-mutex.
(define (make-semaphore-retry-mutex n)
    (let ((n-mutex (make-mutex))
          (retry-mutex (make-mutex)))
        (define (the-semaphore m)
            (cond ((eq? m 'acquire)
                   (n-mutex 'acquire)
                   (if (<= n 0)
                       (begin
                         (n-mutex 'release)
                         (retry-mutex 'acquire)
                         (the-semaphore 'acquire))
                       (begin
                        (set! n (- n 1))
                        (n-mutex 'release))))
                  ((eq? m 'release)
                   (n-mutex 'acquire)
                   (set! n (+ n 1))
                   (n-mutex 'release)
                   (retry-mutex 'release))))
        the-semaphore))

(when false
    (define s (make-semaphore-retry-mutex 3))
    (parallel-execute (map (lambda (f) (lambda () (s 'acquire) (f) (s 'release)))
                           (map (lambda (n) (lambda () (for-each (lambda (i) (sleep 1) (print "thread: ") (print n) (print ", ") (println i)) (enumerate-interval 1 3))))
                                (enumerate-interval 1 10)))))

(define (make-semaphore-test-and-set n))
