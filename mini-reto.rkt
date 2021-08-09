#|
Token highlighter

Matías Mendez
Fernanda Nava
Eduardo Galindo
13/05/2021

1. funcion escribir en html
2. funcion para obtener el nombre archivo y agregar extension de html
3. funcion para leer archivo
4. funcion para clasificar tokens y devuelve tag de html
|#

#lang racket

(require racket/trace)
#|
escribir en archivo
|#

(provide main)

(define (split-file-extension input-string)
  (let
      ;guardar el resultado de la evaluacion de regexp-match
      )
  )

(define (main in-file-path out-file-path)
  "recibe nombre de archivo y escribe en ese achivo, en display el primer argumento
es lo que escribe y el segundo es DONDE los escribe "
  (display-lines-to-file
   ( funcion de clasificar y texto que iria en html )
   (funcion para separar extension de archivo y crear extension html )
   #:exists 'truncate)
  )
