#|
Basic example of using regular expressions in Racket

Gilberto Echeverria
2021-05-12
|#

#lang racket

(require racket/trace)

; Declare publicly available functions
(provide test)

(define (test input-string)
  " Identify strings containing a fixed sequence "
  (regexp-match #px"hello" input-string))
