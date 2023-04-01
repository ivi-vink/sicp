#lang racket
(require "../../shared/lists.rkt")
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list
    left
    right
    (append (symbol-set left)
            (symbol-set right))
    (+ (weight left)
       (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbol-set tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x mset)
  (cond ((null? mset) (list x))
        ((< (weight x) (weight (car mset))) (cons x mset))
        (else (cons (car mset)
                    (adjoin-set x (cdr mset))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
               (make-leaf-set (cdr pairs))))))

;; 67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)
;; '(A D A B B C A)

;; 68
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))


;; just use id as key
(define (key x)
  x)

;; scan for symbol
(define (list-lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (list-lookup given-key (cdr set-of-records)))))

;; without using symbol list, for some reason
;; This is probably not the best way to do it because we cannot short circuit the search using the symbol list that is available at each node.
;; (define (encode-symbol s tree)
;;   (define (bits result subtree)
;;     (cond ((and (leaf? subtree) 
;;                 (equal? s (car (symbol-set subtree))))
;;            result)
;;       ((leaf? subtree) '())
;;       (else
;;         (let ((left (bits 
;;                       (cons 0 result) 
;;                       (left-branch subtree))))
;;           (if (null? left)
;;             (bits (cons 1 result) (right-branch subtree))
;;             left)))))
;;   (reverse (bits '() tree)))

(define (encode-symbol s tree)
  (cond ((leaf? tree) '())
        ((list-lookup s (symbol-set (left-branch tree)))
         (cons 0 (encode-symbol s (left-branch tree))))
        ((list-lookup s (symbol-set (right-branch tree)))
         (cons 1 (encode-symbol s (right-branch tree))))
        (else (error "Symbol not in tree -- ENCODE-SYMBOL" s tree))))
(encode (decode sample-message sample-tree) sample-tree)
(encode '(E) sample-tree)

;; 69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge weighted-leafs)
  (define (merge lightest leafs)
    (if (null? leafs)
      lightest
      (let ((leafs-with-node
              (adjoin-set (make-code-tree
                            lightest
                            (car leafs))
                          (cdr leafs))))
        (merge (car leafs-with-node)
               (cdr leafs-with-node)))))
  (merge (car weighted-leafs)
         (cdr weighted-leafs)))
(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))

;; 70
(define rock-songs-leafs '((A 2) (BOOM 1)
                           (GET 2) (JOB 2)
                           (NA 16) (SHA 3)
                           (YIP 9) (WAH 1)))
(define rock-songs-tree
  (generate-huffman-tree rock-songs-leafs))

(define rock-songs-bits
  (encode '(GET A JOB
            SHA NA NA NA NA NA NA NA NA
            GET A JOB
            SHA NA NA NA NA NA NA NA NA
            WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
            SHA BOOM)
          rock-songs-tree))
;; variable length codes with huffman tree
(length rock-songs-bits)
;; fixed length codes with 8 symbols
(expt 2 (length rock-songs-leafs))

;; 71
;; Most frequent is always a leaf from the root, so only one bit
;; Least frequent requires d bits where d is the depth of the tree,
;; in this case the tree is unbalanced so the depth is linear or n-1.
;; (= (+ 2**(i-2) 2**(i-1)) (- 2**i 1))

;; 72
;; You can do best worst case scenario's for searches and positions in the tree
;; and you can argue average time complexities for different known weight distributions that result in balanced or unbalanced trees.
;; I did them on paper and I'm a messy louise reasoner.
;; Maybe I'll come back someday and write it up.
