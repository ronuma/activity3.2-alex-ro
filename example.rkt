#|
Implementation of a Deterministic Finite Automaton (DFA)

A function will receive the definition of a DFA and a string,
and return whether the string belongs in the language

A DFA is defined as a state machine, with 3 elements:
- Transition function
- Initial state
- List of acceptable states

The DFA in this file is used to identify valid arithmetic expressions

Examples:
> (evaluate-dfa (dfa delta-arithmetic 'start '(int float exp)) "-234.56")
'(float exp)

> (arithmetic-lexer "45.3 - +34 / none")
'(float op int op var)

Gilberto Echeverria
2023-04-13
|#

#lang racket

(require racket/trace)

(provide (all-defined-out))

; Declare the structure that describes a DFA
(struct dfa (func initial accept))

(define (arithmetic-lexer strng)
  " Call the function to validate using a specific DFA "
  (evaluate-dfa (dfa delta-arithmetic 'start '(int float exp var spa)) strng))

(define (evaluate-dfa dfa-to-evaluate strng)
  " This function will verify if a string is acceptable by a DFA "
  (let loop
    ; Convert the string into a list of characters
    ([chars (string->list strng)]
     ; Get the initial state of the DFA
     [state (dfa-initial dfa-to-evaluate)]
     ; The return list with all the tokens found
     [tokens '()])
    (cond
      ; When the list of chars if over, check if the final state is acceptable
      [(empty? chars)
       (if (member state (dfa-accept dfa-to-evaluate))
         ; Add the last pending state to the list, and reverse it
         (reverse (cons state tokens))
         'invalid)]
      [else
        (let-values
          ; Call the transition function and get the new state and whether or not a token was found
          ([(new-state found) ((dfa-func dfa-to-evaluate) state (car chars))])
          (loop (cdr chars)
                new-state
                ; The new list of tokens
                (if found (cons found tokens) tokens)))])))

(define (char-operator? char)
  " Identify caracters that represent arithmetic operators "
  (member char '(#\+ #\- #\* #\/ #\= #\^)))

(define (delta-arithmetic state char)
  " Transition function to validate numbers
  This function now returns two values:
   - The new state in the automaton
   - The token that has been found. Generally false, until we are sure to have found a token
  Initial state: start
  Accept states: int float exp "
  (case state
    ['start (cond
       [(char-numeric? char) (values 'int #f)]
       [(or (eq? char #\+) (eq? char #\-)) (values 'sign #f)]
       [(char-alphabetic? char) (values 'var #f)]
       [(eq? char #\_) (values 'var #f)]
       [else (values 'inv #f)])]
    ['sign (cond
       [(char-numeric? char) (values 'int #f)]
       [else (values 'inv #f)])]
    ['int (cond
       [(char-numeric? char) (values 'int #f)]
       [(eq? char #\.) (values 'dot #f)]
       [(or (eq? char #\e) (eq? char #\E)) (values 'e #f)]
       [(char-operator? char) (values 'op 'int)]
       [(eq? char #\space) (values 'spa 'int)]
       [else (values 'inv #f)])]
    ['dot (cond
       [(char-numeric? char) (values 'float #f)]
       [else (values 'inv #f)])]
    ['float (cond
       [(char-numeric? char) (values 'float #f)]
       [(or (eq? char #\e) (eq? char #\E)) (values 'e #f)]
       [(char-operator? char) (values 'op 'float)]
       [(eq? char #\space) (values 'spa 'float)]
       [else (values 'inv #f)])]
    ['e (cond
       [(char-numeric? char) (values 'exp #f)]
       [(or (eq? char #\+) (eq? char #\-)) (values 'e_sign #f)]
       [else (values 'inv #f)])]
    ['e_sign (cond
       [(char-numeric? char) (values 'exp #f)]
       [else (values 'inv #f)])]
    ['exp (cond
       [(char-numeric? char) (values 'exp #f)]
       [(char-operator? char) (values 'op 'exp)]
       [(eq? char #\space) (values 'spa 'exp)]
       [else (values 'inv #f)])]
    ['var (cond
       [(char-alphabetic? char) (values 'var #f)]
       [(char-numeric? char) (values 'var #f)]
       [(eq? char #\_) (values 'var #f)]
       [(char-operator? char) (values 'op 'var)]
       [(eq? char #\space) (values 'spa 'var)]
       [else (values 'inv #f)])]
    ['op (cond
       [(char-numeric? char) (values 'int 'op)]
       [(or (eq? char #\+) (eq? char #\-)) (values 'sign 'op)]
       [(char-alphabetic? char) (values 'var 'op)]
       [(eq? char #\_) (values 'var 'op)]
       [(eq? char #\space) (values 'op_spa 'op)]
       [else (values 'inv #f)])]
     ['spa (cond
       [(char-operator? char) (values 'op #f)]
       [(eq? char #\space) (values 'spa #f)]
       [else (values 'inv #f)])]
    ['op_spa (cond
       [(char-numeric? char) (values 'int #f)]
       [(or (eq? char #\+) (eq? char #\-)) (values 'sign #f)]
       [(char-alphabetic? char) (values 'var #f)]
       [(eq? char #\_) (values 'var #f)]
       [(eq? char #\space) (values 'op_spa #f)]
       [else (values 'inv #f)])]
    [else (values 'inv #f)]))
