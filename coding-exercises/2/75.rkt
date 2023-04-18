#lang racket

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cosine ang)))
          ((eq? op 'imag-part) (* mag (sine ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; Explicit dispatch
;; The logic for dispatching has to be repeated for every operation and every type.
;; If there are a number of representations for a type or multiple operations, this is not what you want.
;;
;; data dispatch
;; If new operations need to be added then we have to extend all existing type packages to support dispatching the operation to those types.
;; When adding new types we need to install a dispatchable procedure for every operation only in the new type package. 
;;
;; message passing
;; If new operations need to be added then we have to add a message handling case to all data representations.
;; When adding new types we need to make a procedure that handles all operation messages.
;;
;; In general I think if the operation is less smart then it is easier to add to a system, that's why message passing is more appropriate if you often need to add operations.
;; The same goes for data dispatching, if the operations are smart, the data doesn't have to be. This makes adding more data representations more convenient. 
