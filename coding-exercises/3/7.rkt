#lang racket
;; 7
;; import from 1.rkt
(define (make-accumulator amount)
  (lambda (add-this)
    (set! amount (+ amount add-this))
    amount))

(define (make-monitored fn)
  (define calls (make-accumulator 0))
  (lambda (arg-or-message)
    (cond ((equal? arg-or-message 'how-many-calls?) (calls 0))
          ((equal? arg-or-message 'reset-count) (set! calls (make-accumulator 0)))
          (else (begin
                  (calls 1)
                  (fn arg-or-message))))))
(define s (make-monitored sqrt))

(define (make-account balance secret)
  (define secrets (list secret))

  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount)) balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (new-password password)
    (set! secrets (cons password secrets)))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'new-password) new-password)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))

  (define (args->dispatch fn)
    (lambda (arg-list)
      (let ((password (car arg-list))
            (m (cadr arg-list)))
        (if (memq password secrets)
          (fn m)
          (lambda (a . n)
            "Wrong password")))))

  (define monitored-dispatch (make-monitored (args->dispatch dispatch)))
  (define (call-the-cops)
    (lambda (a . n)
      "Calling the cops"))

  (define (safe-dispatch . args)
    (if (> 7 (monitored-dispatch 'how-many-calls?))
      (monitored-dispatch args)
      (call-the-cops)))
  safe-dispatch)

(define (make-joint account password new-password)
  ((account password 'new-password) new-password))

(define paul-acc (make-account 100 'open-sesame))
((paul-acc 'open-sesame 'withdraw) 10)
(make-joint paul-acc 'open-sesame 'rosebud)
((paul-acc 'rosebud 'withdraw) 10)

;; 8
(define hidden '())
(define (f v)
  (if (null? hidden)
    (begin
      (set! hidden v)
      hidden)
    0))
(+ (f 0) (f 0))
