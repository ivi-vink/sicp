#lang racket
(define (get-signal wire)
    (print "setsig"))
(define (set-signal! wire))
(define (add-action! wire cb))

(define (after-delay n f) (print "delay") (println n) (f))

(define (logical-and a1 a2)
    (and (= 1 a1) (= 1 a2)))

(define (logical-or a1 a2)
    (or (= 1 a1) (= 1 a2)))

(define (logical-not s)
    (cond ((= 0 s) 1)
          ((= 1 s) 0)
          (else (error "Invalid signal" s))))

(define (inverter input output)
    (define (invert-input)
        (let ((new-value (logical-not input)))
            (after-delay inverter-delay (lambda ()
                                            (set-signal! output new-value)))))
    (add-action! input invert-input)
    'ok)

(define (and-gate i1 i2 output)
    (define (and-action-procedure)
        (let ((value (logical-and (get-signal a1) (get-signal a2))))
            (after-delay and-gate-delay
                         (lambda ()
                             (set-signal! output value)))))
    (add-action! i1 and-action-procedure)
    (add-action! i2 and-action-procedure)
    'ok)

(define (or-gate i1 i2 output)
    (define (or-action)
        (let ((value (logical-or (get-signal i1) (get-signal i2))))
            (after-delay or-gate-delay (lambda () (set-signal! output value)))))
    (add-action! i1 or-action)
    (add-action! i2 or-action)
    'ok)

(define (half-adder a b s c)
    (let ((d (make-wire))
          (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
        'ok))

(define (full-adder a b c-in sum c-out)
    (let ((s (make-wire))
          (c1 (make-wire))
          (c2 (make-wire)))
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)
        'ok))

;; n * full-adder <=>
;; n * ( 2 * half-adder + or-gate) <=>
;; n * ( 2 * and-gate + or-gate)
(define (ripple-carry-adder A B S C)
    (cond
     ((or (null? A) (null? B)) S)
     (else (let ((carry-out (make-wire)))
             (full-adder (car A) (car B) C (car S) carry-out)
             (ripple-carry-adder (cdr A) (cdr B) (cdr S) carry-out)))))

(define (call-each procedures)
    (if (null? procedures)
        'done
        (begin
         ((car procedures))
         (call-each (cdr procedures)))))

(define (make-wire)
    (let ((signal-value 0) (action-procedures '()))
        (define (set-my-signal! new-value)
            (if (not (= signal-value new-value))
                (begin (set! signal-value new-value)
                       (call-each action-procedures))
                'done))
        (define (accept-action-procedure! proc)
            (set! action-procedures (cons proc action-procedures))
            (proc))
        (define (dispatch m)
            (cond ((eq? m 'get-signal) signal-value)
                  ((eq? m 'set-signal!) set-my-signal!)
                  ((eq? m 'add-action!) accept-action-procedure!)
                  (else (error "Unknown operation -- WIRE" m))))
        dispatch))

(define (get-signal wire)
    (wire 'get-signal))
(define (set-signal! wire)
    ((wire 'set-signal) new-value))
(define (add-action! wire action-procedure)
    ((wire 'add-action!) action-procedure))

(define (after-delay d action)
    (add-to-agenda! (+ d (current-time the-agenda))
                    action
                    the-agenda))

(define (propagate)
    (if (empty-agenda? the-agenda)
        'done
        (let ((first-item (first-agenda-item the-agenda)))
            (first-item)
            (remove-first-agenda-item! the-agenda)
            (propagate))))

(define (probe name wire)
    (add-action! wire
                 (lambda ()
                     (newline)
                     (display name)
                     (display " ")
                     (display (current-time the-agenda))
                     (display " New-value = ")
                     (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)

;; e needs to be 1 in the system to make it work correct.
;; You can try to draw how the agenda would execute the assignments to a b c d and e.
