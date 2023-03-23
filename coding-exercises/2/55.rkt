#lang racket

;; This is a quoted list containing the symbols quote and abracadabra:
;; '(quote abracadabra)
;; Taking the car of the list returns the quote symbol
(car ''abracadabra)
