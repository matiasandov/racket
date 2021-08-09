#|
Reading and writing files
This example will read a full file, switch to upper/lowercase and write a new file

Gilberto Echeverria
2021-04-21
|#

#lang racket

(require racket/trace)

; Indicate the functions available in this script
(provide main)

(define (read-file in-file-path)
  (call-with-input-file in-file-path
    (lambda (in)
      (let loop
        ([line (read-line in)]
         [result empty])
        (if (eof-object? line)
            result
            (loop (read-line in) (append result (list line))))))))


(define (write-file out-file-path data)
  (call-with-output-file out-file-path
    #:exists 'truncate
    (lambda (out)
      (let loop
        ([lst data])
        (cond
          [(not (empty? lst))
             (displayln (car lst) out)
             (loop (cdr lst))])))))


(define (main in-file-path out-file-path)
  ;(write-file out-file-path (map string-upcase (read-file in-file-path))))
  (define data (read-file in-file-path))
  (define result (sort (map string->number data) <))
  ;(define result (map string-upcase data))
  (write-file out-file-path result))

