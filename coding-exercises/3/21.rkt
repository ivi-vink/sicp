#lang racket
(require compatibility/mlist)
;; queue methods
(define (front-ptr q) (mcar q))
(define (rear-ptr q) (mcdr q))
(define (set-front-ptr! q v) (set-mcar! q v))
(define (set-rear-ptr! q v) (set-mcdr! q v))
(define (empty-queue? q) (null? (front-ptr q)))
(define (make-queue) (mcons '() '()))
(define (front-queue q)
  (if (empty-queue? q)
    (error "FRONT called on empty queue" q)
    (mcar (front-ptr q))))
(define (insert-queue! q item)
  (let ((new-item (mlist item)))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-item)
           (set-rear-ptr! q new-item)
           q)
          (else
            (set-mcdr! (rear-ptr q) new-item)
            (set-rear-ptr! q new-item)
            q))))
(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "DELETE! called with an empty queue" q))
        (else
          (set-front-ptr! q (mcdr (front-ptr q)))
          q)))
;; 21
(define (print-queue q)
  (front-ptr q))
(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))
;; 22
(define (make-queue-obj)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! v)
      (set! front-ptr v))
    (define (set-rear-ptr! v)
      (set! rear-ptr v))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (mcar front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (mlist item)))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else (set-mcdr! rear-ptr new-pair)
                    (set-rear-ptr! new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" front-ptr))
            (else
              (set-front-ptr! (mcdr front-ptr)))))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) front-ptr)
            (else (error "Message note defined on queue" m))))
    dispatch))
(define qobj (make-queue-obj))
((qobj 'insert-queue!) 'a)
(qobj 'print-queue)
((qobj 'insert-queue!) 'b)
(qobj 'print-queue)
((qobj 'insert-queue!) 'c)
(qobj 'print-queue)
((qobj 'delete-queue!))
(qobj 'print-queue)
;; 23