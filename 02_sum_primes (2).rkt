#lang racket
#|
Actividad 5.2 Programación paralela y concurrente

Matías Mendez
Fernanda Nava
Eduardo Galindo
21/05/2021
|#


;función para obtener la suma secuencial de primos
(define (main_1 limit)
  ;mide el tiempo de ejecución
  (time
   ;loop para recorrer desde 0 hasta el límite
    (let loop
      ([n 0][sum 0])
      (cond
        [(< n limit)
         ;si es primo se suma
         (if (prime? n)(loop (+ n 1) (+ sum n))
             (loop (+ n 1) sum))
         ]
         [else
          ;Cuando se llega al límite se imprima el resultado
          (printf "Result: ~a\n" sum)])))
  )

;;función para obtener la suma paralela de números primos
(define (sum-primes core_number limit)
  (future (lambda ()
            ;loop para 
            (let loop
              ; se asigna el límite de cada future
              ; y se lleva el conteo de las sumas
              ([n (* limit core_number)][sum 0])
              (cond
                ; mientras n sea menor al límite:
                [(< n (* core_number (add1 limit)))
                 ; si n es primo se suma
                 (if (prime? n)(loop (+ n 1)(+ sum n))
                    (loop (+ n 1)sum))
                ]
                ; cuando se llega al límite se
                ; bimprime el resultado
                [else (printf "Result: ~a\n" sum)])))))

;Función para verificar si un número es primo
(define (prime? n)
  ; si es menor 2, devuelve falso
  (if (< n 2)
      #f    
  (let loop
    ; define el límite como la raíz cuadrada de n
    ([ div (round(sqrt n))])
    (cond
      ;mientras el número a dividir sea mayor a 0:
      [(> div 1)
       ;si encuentra que el residuo al dividir n entre div
       ;es 0, devuelve flaso
       (if (zero? (remainder n div))
           #f
           ;continúa el ciclo restándole 1 a el divisor
           (loop (- div 1)))]
      ; si el divisor llega a 1 y no encontró ninguno, es primo
      [else #t]))))


;Función para crear los futures posibles y combinar sus resultados
(define (main limit num-cores)
  (time
   ; crea los futures necesarios y les asigna un rango dependiendo de la cantidad
   ; de cores y el límite
  (define futures (map sum-primes (make-list num-cores (quotient limit num-cores))
                       (range num-cores)))
  ; Combina los resultados de los múltiples futures
  (define result (foldl + 0 (map touch futures)))
  ;Imprime el resultado
  (printf "Result: ~a\n" result)
  ))



