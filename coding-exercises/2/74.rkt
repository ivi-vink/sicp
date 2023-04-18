#lang racket
(require "../../shared/data-directed-programming.rkt")
(require "../../shared/lists.rkt")

(define test-dispatch (make-dispatch-table))
(define get (getter test-dispatch))
(define put (putter test-dispatch))

;; Example implementations
(define test-division (attach-tag 
                          'division-a
                          (list (attach-tag 
                                  'henk 
                                  (list (attach-tag 'salary 100))))))
(put 'record 'division-a (lambda (file-set employee) 
                           (let ((record (find-first 
                                           (make-eq-type? employee) 
                                           file-set)))
                             (if record
                               record
                               (error "Employee record not found -- GET-RECORD DIVISION-A" employee)))))
(put 'salary 'division-a (lambda (record)
                           (let ((salary (find-first
                                           (make-eq-type? 'salary)
                                           record)))
                             (if salary
                               salary
                               (error "Salary not found -- GET-SALARY DIVISION-A" record)))))

;;a Each divisions file must be a datum tagged with the divisions name.
;;  Together with the division type tag and an operation type tag
;;  we can get a procedure that knows how to do that operation for the given employee.
(define (get-record file employee)
  ((get 'record (type-tag file)) (contents file) employee))

(define test-record (get-record test-division 'henk))

;; b The record can have any structure that is handled by the salary procedure of the
;; division we dispatch the procedure from
(define (get-salary file record)
  ((get 'salary (type-tag file)) (contents record)))


(get-salary test-division test-record)

;;c
(define (find-employee-record files employee)
   (define (search fi)
     (if (null? fi)
       false
       (let ((result (with-handlers
                       ([exn:fail? (lambda (exn)
                                     false)])
                       (get-record (car fi) employee))))
         (if result
           result
           (search (cdr fi))))))
   (search files))
(find-employee-record (list test-division test-division) 'henk)

;;d
;; new implementations for the division representation for the existing operations on a type
