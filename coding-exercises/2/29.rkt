#lang racket
(require sicp)

;;(define (make-mobile left right)
;; (list left right))
(define (make-mobile left right)
  (cons left right))

(define (make-branch len structure)
  (cons len structure))

(define (left-branch m)
  (car m))
(define (right-branch m)
  (cdr m))

(define m (make-mobile (make-branch 2 3) (make-branch 4 5)))

(define (branch-length b)
  (car b))
(define (branch-structure b)
  (cdr b))

(define (total-weight mobile)
  (define (maybe-recurse s)
    (if (pair? s)
      (rec s)
      s))
  (define (rec m)
    (let ((l (branch-structure (left-branch m)))
          (r (branch-structure (right-branch m))))
      (+ (maybe-recurse l) (maybe-recurse r))))
  (rec mobile))

;; We don't want to call total-weight multiple times, because it has a typical binary tree recursion time
;; complexity of 2^n.
;; Instead we modify total weight to return a pair (weight balance).
;; Weight is calculated as before, but we now also calculate the balance of a tree and its subtrees.
;; So instead of n*2^n we have still 2^n.
(define (balanced? mobile)
  (define (equal-torque? len1 w1 len2 w2)
    (= (* len1 w1) (* len2 w2)))
  (define (rec m)
    (let
      ((ls (branch-structure (left-branch m)))
       (ll (branch-length (left-branch m)))
       (rs (branch-structure (right-branch m)))
       (rl (branch-length (right-branch m))))
     (cond
       ((not (or (pair? ls) (pair? rs)))
        (cons (+ ls rs)
              (equal-torque? ll ls rl rs)))
       ((and (pair? ls) (not (pair? rs)))
        (let ((result-l (rec ls)))
          (cons (+ (car result-l) rs)
                (and (cdr result-l)
                     (equal-torque? ll (car result-l)
                                    rl rs)))))
       ((and (not (pair? ls)) (pair? rs))
        (let ((result-r (rec rs)))
          (cons (+ ls (car result-r))
                (and (cdr result-r)
                     (equal-torque? ll ls
                                    rl (car result-r))))))
       (else
         (let ((result-l (rec ls))
               (result-r (rec rs)))
           (cons (+ (car result-l) (car result-r))
                 (and
                   (cdr result-l)
                   (cdr result-r)
                   (equal-torque? ll (car result-l)
                                  rl (car result-r)))))))))
  (rec mobile))

(define (print)
  (define balanced (make-mobile (make-branch 2 3) (make-branch 3 2)))
  (define unbalanced (make-mobile (make-branch 2 3) (make-branch 3 3)))

  (newline)
  (display (balanced? balanced))
  (newline)
  (display (balanced? unbalanced))

  (define balanced-nested
    (make-mobile
      (make-branch 2 3)
      (make-branch
        3
        (make-mobile
          (make-branch 1 1)
          (make-branch 1 1)))))

  (define unbalanced-nested
    (make-mobile
      (make-branch 2 3)
      (make-branch
        4
        (make-mobile
            (make-branch 1 1)
            (make-branch 1 1)))))
  (newline)
  (display (balanced? balanced-nested))
  (newline)
  (display (balanced? unbalanced-nested)))

(print)
