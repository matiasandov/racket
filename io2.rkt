#|
Simpler file IO
Using functions in racket to read a file into a list of strings
and vice versa

Gilberto Echeverria
2021-0-22
|#

#lang racket

(provide main)

(define (main in-file-path out-file-path)
  (display-lines-to-file
    (sort (map string->number (file->lines in-file-path)) <)
    out-file-path
    #:exists 'truncate))
