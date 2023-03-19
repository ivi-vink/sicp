#lang racket

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
