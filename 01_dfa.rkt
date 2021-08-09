#|
Implement a Deterministic Finite Automata

Matías Mendez
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
     [token-list empty]
     [transition (car dfa)]) ; The first element in the list
    (if (empty? lst)
        ; Check if the final state is in the list of acceptables
        (if (member state (caddr dfa))
            ; Return the list of tokens and the last accept state
            (append token-list (list state))
            #f)
        (let-values
            ;queremos guardar el symbol en un symbol
            ;car te da primero de la lista
          ([(state token-type) (transition state (car lst))])
          ; Recursive call
          (loop
            (cdr lst)
            state
            ; Add valid tokens to the list
            (if token-type
              (append token-list (list token-type))
              token-list)
            ; Pass the same function again
            transition)))))

(define (accept-simple-arithmetic state symbol)
  " Accepts arithmetic expressions with positive integers"
  (let
    ([ops (list #\= #\+ #\- #\* #\/ #\^)])
    (cond
      [(eq? state 'q0) (cond
        [(char-numeric? symbol) (values 'int #f )] 
        [(char-alphabetic? symbol) (values 'var #f)] ;rechaza cualquier cosa que no sea un numero
        [(eq? symbol #\space  ) (values 'q0 #f)]
        [else (values 'invalid #f )]
        )]
     ;-------------checar que onda con parentesis---------
      
      [(eq? state 'int) (cond
        [(char-numeric? symbol) (values 'int #f)] ;int
        [(eq? symbol #\. ) (values 'dot #f)] ;dot
        [(member symbol ops) (values 'op 'int)]
        [(eq? symbol #\space  ) (values 'int_sp 'int)]
        ;si no funcionan e, poner ^
        [else (values 'invalid #f ) ]
        )] ; An int had been found
      ;n_sp
      [(eq? state 'int_sp) (cond
        [(member symbol ops) (values 'op #f)] 
        [(eq? symbol #\space  ) (values 'int_sp #f)]
        [else (values 'invalid #f ) ]
        )]
      
      [(eq? state 'var) (cond
         [(char-numeric? symbol) (values 'var #f)] ;int
         [(char-alphabetic? symbol) (values 'var #f)] ;rechaza cualquier cosa que no sea un numero
         [(eq? symbol #\_ ) (values 'var #f)]
         [(eq? symbol #\( ) (values 'paren 'var)]
         [(eq? symbol #\) ) (values 'paren 'var)]
         [(eq? symbol #\space  ) (values 'var_sp 'var)]
         [(member symbol ops) (values 'op 'var)]
         [else (values 'invalid #f ) ]
                          )]
      ;v_sp
      [(eq? state 'var_sp) (cond
        [(member symbol ops) (values 'op #f)] 
        [(eq? symbol #\space  ) (values 'var_sp #f)]
        [(eq? symbol #\( ) (values 'paren #f)]
         [(eq? symbol #\) ) (values 'paren #f)]
        [else (values 'invalid #f ) ]
        )]

      
      [(eq? state 'paren) (cond
         [(char-numeric? symbol) (values 'int #f)] ;int
         [(char-alphabetic? symbol) (values 'var #f)] ;te regresa a variable
         [(eq? symbol #\( ) (values 'paren #f)]
         [(eq? symbol #\) ) (values 'paren #f)]
         [(member symbol ops) (values 'op 'int)]
         [(eq? symbol #\space ) (values 'paren #f)]
         [else (values 'invalid #f ) ]
                          )]
      
       [(eq? state 'dot) (cond
         [(char-numeric? symbol) (values 'float #f)] ;int
         [else (values 'invalid #f ) ]
                          )]
       
       [(eq? state 'float) (cond
         [(char-numeric? symbol) (values 'float #f)] ;int
         [(eq? symbol #\e ) (values 'exp #f)] ;e
         [(eq? symbol #\E ) (values 'exp #f)] ;e
         [(eq? symbol #\space  ) (values 'int_sp 'float)]
         [(member symbol ops) (values 'op 'float)] ; An int had been found
         [else (values 'invalid #f ) ]
         
                          )]
       
       [(eq? state 'exp) (cond
         [(char-numeric? symbol) (values 'exp #f)] ;int
         [(eq? symbol #\- ) (values 'exp #f)] ;dot
         [(eq? symbol #\space   ) (values 'int_sp #f)]
         [(member symbol ops) (values 'op 'int)] ; An int had been found
         [else (values 'invalid #f ) ]
                          )]

      [(eq? state 'op) (cond
        [(char-numeric? symbol) (values 'int 'op)] ; Found an operator
        [(eq? symbol #\space  ) (values 'op #f)]
        [(eq? symbol #\/  ) (values 'com #f)];comentario
         [(char-alphabetic? symbol) (values 'var #f)]
         [else (values 'invalid #f ) ]
        )]
      
      [(eq? state 'com) (cond
         [(char-numeric? symbol) (values 'com #f)] ;int
         [(char-alphabetic? symbol) (values 'com #f)] ;rechaza cualquier cosa que no sea un numero
         [(member symbol ops) (values 'com #f)]
         [(eq? symbol #\space  ) (values 'com #f)]
         [else (values 'com #f ) ]
                          )]
      
      [(eq? state 'invalid) (values 'invalid #f)]
      )
    )
  )