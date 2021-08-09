#lang racket

(provide prime? sum-primes)

(define (prime? n)
  (if (< n 2)
      #f    
  (let loop
    ([ div (- n 1)])
    (cond
      [(> div 1)
       (if (zero? (remainder n div))
           #f
           (loop (- div 1)))]
      [else #t]))))
    

(define (make-thread name limit)
  (thread (lambda ()
    (let loop
      ([n 0])
      (cond
        [(< n  limit)
         ;(printf "Thread ~a | Number ~a\n" name n)
         ; Force the thread to wait, to let others run too
         (sleep (random))
         (loop (add1 n))]
        [else (printf "Thread ~a Finished!\n" name)])))))

(define (sum-primes name limit)
  (thread (lambda ()
            (let loop
              ([n 0][sum 0])
              (cond
                [(< n  limit)
                 (cond
                   [(prime? n)
                    (sleep (random))
                    (loop (+ n 1)(+ sum n))]
                   [else
                    (sleep (random))
                    (loop (+ n 1)(+ sum 0))])]
                 [else
                  (sleep (random))
                  (printf "Thread ~a Finished! : ~a sum \n"name sum)])))))

(define (main num-threads)
  (time
  " Creates a number of threads and waits for them to finish"
  (define threads (map sum-primes (range num-threads)
                       (make-list num-threads 10)))
  (for-each thread-wait threads)
  (sleep (random))
  ;(kill-thread threads)
  (printf "MAIN THREAD FINISHED\n"))
  (void))


(define (main2)
  ;tienes que decir que funcion llamara dentro dle thread y no puede tener argumentos
  ; asi pasas argumentos, capsulando una funcion sin argumentos dentro de otra con argumentos
  ; y devolver solo el thread
  ;aqui estas definiendo un nombre donde se guardara el thread que regresa make-thread
  (define my-thread (make-thread "name" 20))
  (for ([n 20])
    ;se duerme un momento para que otros thread tome uso del procesador
    ;no se debe usar siempre
    (sleep (random))
    (printf "in main thread : ~a\n" n))
  ; lo matas cuando acabe 
  (kill-thread my-thread)
  ;cuando ya avabe
  (printf "main finishes"))

; map podría sirve para usar todos los elementos de una lista en una funcion
;ejemplo: map (sqrt 2 4 5) a todos se les aplica la funcion sqrt
