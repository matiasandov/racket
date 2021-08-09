#|
Examples of using threads

Gilberto Echeverria
2021-05-26
|#

#lang racket

(define (make-thread name limit)
  " Return a new thread that loops 'limit' times "
  (thread (lambda ()
    (let loop
      ([n 0])
      (cond
        [(< n  limit)
         (printf "Thread ~a | Number ~a\n" name n)
         ; Force the thread to wait, to let others run too
         (sleep (random))
         (loop (add1 n))]
        [else (printf "Thread ~a Finished!\n" name)])))))


(define (main num-threads)
  " Creates a number of threads and waits for them to finish"
  (define threads (map make-thread (range num-threads)
                       (make-list num-threads 10)))
  ;(define my-thread-1 (make-thread "One" 10))
  ;(define my-thread-2 (make-thread "Two" 10))
  ;(define my-thread-3 (make-thread "Three" 10))
  (for ([n 5])
       (sleep (random))
       (printf "In main thread: ~a\n" n))
  ; Make sure all threads are finished before continuing
  (for-each thread-wait threads)
  (printf "MAIN THREAD FINISHED\n"))
