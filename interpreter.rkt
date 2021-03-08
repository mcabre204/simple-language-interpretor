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
		 (parser file)))


(define evalfile
    (lambda (lis state)
        ((null? lis) (error 'nothing_here_bro))
        ((eq? (car (car lis)) 'return) (M-return arg1 state))
        (else (evalfile (cdr lis) (M-state-statement (car lis) state)))))

(define arg1 (lambda lis) (cadr lis))
(define arg2 (lambda lis) (caddr lis))
(define arg3 (lambda lis) (cadddr) lis)

(define M-state-statement
    (lambda (statement state)
    (cond
        ((eq? (car statement) 'var) #| mstate for var assignment |# )
        ((eq? (car statement) '=) #|mstate for =|#)
        ((eq? (car statement) 'if) (if (null? (cdddr))
                                     (M-if (arg1 statement) (arg2 statement) state)
                                     (M-if-else (arg1 statement) (arg2 statement) (arg3 statement) state)))
        ((eq? (car statement) 'while) (M-while (arg1 statement) (arg2 statement) state))
        (else (error 'bad_operator)))))

;returns if the variable is declared in a given state
(define declared
    (lambda (var state)
        (cond
        ((null? (car state)) #f)
        ((eq? var (caar state)) #t)
        (else (declared var (cdar state)))
        )
    ))

;gets the value of a declared variable in a state
(define getValue
    (lambda (var state)
        (if (eq? var (caar state))
            (cadr state)
            (getValue (list (cdar state) (cddr state)))
        )
    ))
    
;sets a value to an already declared value
(define setValue
    (lambda (var val state return) ;'(()())
        (if (eq? var (caar state)
            (list (car return) (cons (cdr return) val))
            (setValue var val (list (cdar state) (cddr state)) (list (cons ((car return) (caar state))) (cons ((cdr return) (cddr state))))


            ;(cons (car state)(cons val (cddr state)))
            ;(cons(list (caar state) (cadr state))(setValue var val (list (cdar state) (cddr state))))
        ))
    )))

(define leftoperand (lambda (exp) (cadr exp)))
(define operator (lambda (exp) (car exp)))
(define rightoperand (lambda (exp) (caddr exp)))

(define checkType
    (lambda (exp)
        (if (list? (leftoperand exp))
            (checkType (leftoperand exp))
            (if (or ((eq? (leftoperand exp) #t) ((eq? leftoperand exp) #f)))
            'bool
            'int
            ))))

;((x y) (3 4))
;(x = x + 3)
; (= x (+ x 3))

(define M-value
    (lambda (exp state)
        (if(list? exp) 
            (if (eq? (checkType exp) 'bool)
                ; returns bool
                (cond
                    ((eq? (operator exp) '!) (M-boolean exp not))
                    ((eq? (operator exp) '==) (M-boolean exp eq?))
                    ((eq? (operator exp) '!=) (M-boolean exp (lambda (v w) (not (eq? v w)))))
                    ((eq? (operator exp) '&&) (M-boolean exp and))
                    ((eq? (operator exp) '||) (M-boolean exp or)))
                    (else (error 'bad_operator))
            
                ; returns bool
                (cond
                    ((eq? (operator exp) '==) (M-compare exp eq?))
                    ((eq? (operator exp) '!=) (M-compare exp (lambda (v w) (not (eq? v w)))))
                    ((eq? (operator exp) '>=) (M-compare exp >=))
                    ((eq? (operator exp) '<=) (M-compare exp <=))
                    ((eq? (operator exp) '<) (M-compare exp <))
                    ((eq? (operator exp) '>) (M-compare exp >))
                    ; return int
                    (else (M-integer exp)))
                )
            (cond
                ((eq? exp 'true) #t)
                ((eq? exp 'false) #f)
                ((number? exp) exp)
                ((if (declared exp state))
                    (getValue exp state)
                    (error 'bad_operator) ))
                
        )))

; (== (== x x) y)
(define M-boolean
    (lambda (exp op)
        (if (eq? op not) 
            (M-value(leftoperand exp))
            (op (M-value(leftoperand exp)) (M-value(rightoperand exp)))

(define M-compare
    (lambda (exp op)
        (op (M-value(leftoperand exp)) (M-value(rightoperand exp))))
    ))

;(M-integer '(* (+ 4 3) (- 2 1))) => 7
; make '- negative
(define M-integer
  (lambda (exp)
    (cond
      ((number? (leftoperand) exp))
      ((eq? (operator exp) '+) (+ (M-integer(leftoperand exp) (M-integer (rightoperand exp)))))
      ((eq? (operator exp) '-) (- (M-integer (leftoperand exp) (M-integer (rightoperand exp)))))
      ((eq? (operator exp) '*) (floor(* (M-integer (leftoperand exp) (M-integer (rightoperand exp))))))
      ((eq? (operator exp) '%) (modulo (M-integer (leftoperand exp) (M-integer (rightoperand exp)))))
      ((eq? (operator exp) '/) (quotient (M-integer (leftoperand exp) (M-integer (rightoperand exp)))))
      (else (error 'bad_operator))))

(define M-if
    (lambda (condition statement1 state) 
        (if (M-value condition state)
            (M-state-statement statement1 (M-state-statement condition state)
            (M-state-statement condition state))
        )))

(define M-if-else
    (lambda (condition statement1 statement2 state)
        (if (M-value condition state)
            (M-state-statement statement1 (M-state-statement condition state))
            (M-state-statement statement2 (M-state-statement condition state)
            ))))

(define M-while
    (lambda (condition body state)
        (if (M-value condition)
            (M-while condition body (M-state-statement body (M-state-statement condition state)))
            (M-state-statement condition state)
            )))

(define M-return
    (lambda (exp state)
        (M-value exp state))
    )

(define M-assignment
    (lambda (var exp state)
        (setValue var (M-value exp state) state )
    ))

(define M-declaration
    (lambda (var state)
        (cons (cons (var (car state))) (cons ('error (cdr state))))
    ))

(define M-declaration-val
    (lambda (var value state)
        (cons (cons (var (car state))) (cons ((M-value value state) (cdr state))))
    ))