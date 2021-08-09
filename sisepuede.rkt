#lang racket
#|
Token highlighter

Matías Mendez
Fernanda Nava
Eduardo Galindo
13/05/2021

car: saca el primero de la lista

1. funcion escribir en html
2. funcion para obtener el nombre archivo y agregar extension de html
3. funcion para leer archivo
4. funcion para clasificar tokens y devuelve tag de html

|#


(require racket/trace)
(require racket/match)
(provide main)

(define (split-file-extension input-string)
  " Extract only the name part of a file name "
  (let
    ; Define two groups: one for the name and one for the extension
    ; Store the results of the regular expression (a list) in variable 'matches'
    ([matches (regexp-match #px"([\\w-]+)(\\.\\w{1,4})" input-string)])
    ; Take only the first group
    (string-append (cadr matches) ".html")
    )
  ;
  )

(define (read-file in-file-path)
  (call-with-input-file in-file-path
    (lambda (in)
      (let loop
        ([line (read-line in)]
         [result empty])
        (if (eof-object? line)
            result
            (loop (read-line in) (append result (list line))))
        ))))

(define (write-file out-file-path data)
  (call-with-output-file out-file-path
    #:exists 'truncate
    (lambda (out)
      (displayln "<!DOCTYPE html>" out)
      (displayln "<html>" out)
      (displayln "<head>" out)
      (displayln "<title>" out)
      (displayln "Json Codee" out)
      (displayln "<LINK REL=StyleSheet HREF='estilo.css' >" out)
      (displayln "</title>" out)
      (displayln "</head>" out)
      (displayln "<body>" out)
      (let loop
        ;en data se guarda lo que se lee del archivo
        ([lst data])
        (cond
          [(not (empty? lst))
           ;saca el primero y lo escribe en out
           ;(dfa (car lst out)
             (dfa (car lst) out)
             (loop (cdr lst))])
        )
      (displayln "</body>" out)
      (displayln "</html>" out))))
  

(define (main in-file-path)
  (define out-file-path (split-file-extension in-file-path))
  ;(write-file out-file-path (map string-upcase (read-file in-file-path))))
  (define data (read-file in-file-path))
  (write-file out-file-path data))



;object key
(define (dfa element out)

  (if(regexp-match #rx"{|}|," element)
  (displayln (string-append "<span class='punctuacion'>" element "</span>" ) out)
  #f)
  )



