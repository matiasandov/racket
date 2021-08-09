#|
Functions to sum all the prime numbers smaller than a limit

Gilberto Echeverria
2021-05-26
|#

#lang racket

(provide prime? sum-primes)

(define (prime? n)
  (if (zero? (remainder n 2))
      #f
      #t))


(define (sum-primes limit)
  ; Loop from 0 to limit
  ; check each number for primality
  ; add to total if it is prime
  )
