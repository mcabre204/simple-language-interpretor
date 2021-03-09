#lang racket
(require "simpleParser.rkt")
(require "lex.rkt")
;;===============================================================
;; The Interpreter
;; Jake Prusky, Marco Cabre, Morris Lee
;; CSDS 345
;=======================================

(define interpret
	(lambda (file)
		 (evalfile (parser file) '((return_value) (error)))))

(define evalfile
    (lambda (lis state)
      (cond
        ((not(eq? (getValue 'return_value state) 'error)) (getValue 'return_value state))
        ((null? lis)                   (error 'nothing_here_bro))
        ;((eq? (car (car lis)) 'return) (getValue 'return_value (M-return (cadar lis) state)))
        (else                          (evalfile (cdr lis) (M-state-statement (car lis) state))))))

;argument helpers
(define arg1 (lambda (lis) (cadr lis)))
(define arg2 (lambda (lis) (caddr lis)))
(define arg3 (lambda (lis) (cadddr lis)))

;get left and right operands and operator
(define leftoperand (lambda (exp) (cadr exp)))
(define operator (lambda (exp) (car exp)))
(define rightoperand (lambda (exp) (caddr exp)))

;returns if the variable is declared in a given state
(define declared
    (lambda (var state)
        (cond
            ((null? state)          #f)
            ((null? (car state))    #f)
            ((eq? var 'return_value) #t)
            ((eq? var (caar state)) #t)
            (else                   (declared var (list (cdar state) (cdadr state)))))))

;gets the value of a declared variable in a state
(define getValue
    (lambda (var state)
        (if (eq? var (caar state))
            (caadr state)
            (getValue var (list (cdar state) (cdadr state))))))
    
;helper function for setValue
(define setValue-helper
    (lambda (var val state return)
      (cond
        ((null? (car state))    (return (list var) (list val)))
        ((eq? (caar state) var) (return (car state) (cons val (cdadr state))))
        (else                   (setValue-helper var val (list (cdar state) (cdadr state)) (lambda (v w) (return (cons (caar state) v) (cons (caadr state) w))))))))

;sets the value of a declared variable, if the variable is not declared, declare with input value 
(define setValue
    (lambda (var val state)
        (setValue-helper var val state (lambda (v w) (list v w)))))

(define M-state-statement
    (lambda (statement state)
        (cond
            ((eq? (car statement) 'var)   (if (eq? (length statement) 2)
                                            (M-declaration (arg1 statement) state)
                                            (M-declaration-val (arg1 statement) (M-value (arg2 statement) state) state)))
            ((eq? (car statement) '=)     (M-assignment (arg1 statement)  (M-value (arg2 statement) state) state))
            ((eq? (car statement) 'if)    (if (eq? (length statement) 3)
                                            (M-if (arg1 statement) (arg2 statement) state)
                                            (M-if-else (arg1 statement) (arg2 statement) (arg3 statement) state)))
            ((eq? (car statement) 'while) (M-while (arg1 statement) (arg2 statement) state))
            ((eq? (car statement) 'return) (M-return (arg1 statement) state))
            (else                         (error 'bad_operator)))))

;helper functions for and and or because racket is stupid
(define myor (lambda (v w) (or v w)))
(define myand (lambda (v w) (and v w)))

(define M-value
    (lambda (exp state)
        (if (list? exp) 
            (cond
               ((eq? (operator exp) '!)     (M-boolean exp not state))
               ((eq? (operator exp) '==)    (M-boolean exp eq? state))
               ((eq? (operator exp) '!=)    (M-boolean exp (lambda (v w) (not (eq? v w))) state))
               ((eq? (operator exp) '&&)    (M-boolean exp myand state))
               ((eq? (operator exp) '||)    (M-boolean exp myor state))  
               ((eq? (operator exp) '>=)    (M-boolean exp >= state))
               ((eq? (operator exp) '<=)    (M-boolean exp <= state))
               ((eq? (operator exp) '<)     (M-boolean exp < state))
               ((eq? (operator exp) '>)     (M-boolean exp > state))    
               ((eq? (operator exp) '+)     (M-integer exp + state))
               ((eq? (operator exp) '-)     (M-integer exp - state))
               ((eq? (operator exp) '*)     (M-integer exp * state))
               ((eq? (operator exp) '%)     (M-integer exp modulo state))
               ((eq? (operator exp) '/)     (M-integer exp / state))
               (else                        (error 'bad_operator)))
            (cond
                ((or (eq? exp 'true) (eq? exp #t))          'true)
                ((or (eq? exp 'false) (eq? exp #f))          'false)
                ((number? exp)              exp)
                ((not (declared exp state)) (error 'undeclared_variable))
                ((eq? (getValue exp state) 'error) (error 'unassigned_variable))
                (else (getValue exp state))))))

(define M-boolean
    (lambda (exp op state)
        (if (eq? op not) 
            (not(M-value (leftoperand exp) state))
            (op (M-value (leftoperand exp) state) (M-value (rightoperand exp) state)))))

(define M-integer
  (lambda (exp op state)
    (if (null? (cddr exp))
        (M-value (- (leftoperand exp)) state)
        (op (M-value (leftoperand exp) state) (M-value (rightoperand exp) state)))))

(define M-if
    (lambda (condition statement1 state) 
        (if (M-value condition state)
            (M-state-statement statement1 (M-state-statement condition state))
            (M-state-statement condition state))))

(define M-if-else
    (lambda (condition statement1 statement2 state)
        (if (M-value condition state)
            (M-state-statement statement1 state)
            (M-state-statement statement2 state))))

(define M-while
    (lambda (condition body state)
        (if (M-value condition state)
            (M-while condition body (M-state-statement body state))
            state)))

(define M-return
    (lambda (exp state)
        (setValue 'return_value (M-value exp state) state)))

(define M-assignment
    (lambda (var exp state)
        (if (declared var state)
            (setValue var (M-value exp state) state)
            (error 'variable-not-declared))))

(define M-declaration
    (lambda (var state)
        (if (declared var state)
            (error 'variable_already_declared)
            (setValue var 'error state))))

(define M-declaration-val
    (lambda (var val state)
        (if (declared var state)
            (error 'variable_already_declared)
            (setValue var val state))))

(interpret "testcock.txt")