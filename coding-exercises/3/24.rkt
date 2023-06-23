#lang racket
(require compatibility/mlist)

(define (make-2d-table same-key?)
  (let ((local-table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (mcdr subtable))))
            (if record
              (mcdr record)
              false))
          false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (mcdr subtable))))
            (if record
              (set-mcdr! record value)
              (set-mcdr! subtable
                         (mcons (mcons key-2 value)
                                (mcdr subtable)))))
          (set-mcdr! local-table
            (mcons (mlist key-1 (mcons key-2 value)) (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define tbl (make-2d-table (lambda (a b) (equal? a b))))
((tbl 'insert-proc) 'test 'message "hi")
((tbl 'lookup-proc) 'test 'message)

(define (make-general-table same-key?)
  (let ((local-table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup keys) (mcdr (foldl (lambda (key tbl) (assoc key (mcdr tbl))) local-table keys)))
    (define (insert! keys value)
      (set-mcdr!
       (foldl (lambda (key tbl)
                (let ((subtable (assoc key (mcdr tbl))))
                  (if subtable
                    subtable
                    (let ((empty-record (mlist key)))
                      (set-mcdr! tbl (mcons empty-record (mcdr tbl)))
                      empty-record))))
             local-table
             keys)
       value)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define gentbl (make-general-table (lambda (a b) (equal? a b))))
((gentbl 'insert-proc) '(test message) "hi")
((gentbl 'lookup-proc) '(test message))

;; Example table could be represented as a binary tree on the keys
;;  '((B value) (((A value) () ()))
;;              (((C value) () ())))
;; The insert and lookup procedure can then just be implemented using the element-of-set? and adjoin-set methods if the entry selector is adjusted for the (key value) format
