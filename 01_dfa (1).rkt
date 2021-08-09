#|
Implement a Deterministic Finite Automata

Gilberto Echeverria
10/03/2021
|#

#lang racket

(require racket/trace)

(define (validate-string input-string dfa)
  " Determine if the input string is accepted by the dfa
  Ex: (validate-string 'abababa' (list accept-start-ba 'q0 '(q2)))
  Arguments:
  input-string - string
  dfa - list with these elements
            * transition function
            * start state
            * list of accept states
  Return: boolean "
  (let loop
    ([lst (string->list input-string)]
     [state (cadr dfa)]     ; The second element in the list
     [transition (car dfa)]) ; The first element in the list
    (if (empty? lst)
        ; Check if the final state is in the list of acceptables
        (member state (caddr dfa))
        ; Recursive call
        (loop
          (cdr lst)
          ; Get the next state
          (transition state (car lst))
          ; Pass the same function again
          transition))))

(define (accept-start-ba state symbol)
  (cond
    [(eq? state 'q0) (cond
        [(eq? symbol #\a) 'q3]
        [(eq? symbol #\b) 'q1])]
    [(eq? state 'q1) (cond
        [(eq? symbol #\a) 'q2]
        [(eq? symbol #\b) 'q3])]
    [(eq? state 'q2) (cond
        [(eq? symbol #\a) 'q2]
        [(eq? symbol #\b) 'q2])]
    [(eq? state 'q3) (cond
        [(eq? symbol #\a) 'q3]
        [(eq? symbol #\b) 'q3])]))


(define (accept-start-a-end-b state symbol)
  " Transition function that accepts strings that:
  start with an a and finish with a b"
  (cond
    [(eq? state 'q0) (cond
        [(eq? symbol #\a) 'q1]
        [(eq? symbol #\b) 'q3])]
    [(eq? state 'q1) (cond
        [(eq? symbol #\a) 'q1]
        [(eq? symbol #\b) 'q2])]
    [(eq? state 'q2) (cond
        [(eq? symbol #\a) 'q1]
        [(eq? symbol #\b) 'q2])]
    [(eq? state 'q3) (cond
        [(eq? symbol #\a) 'q3]
        [(eq? symbol #\b) 'q3])]))
