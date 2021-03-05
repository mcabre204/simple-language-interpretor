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
        ((null? lis) (error))
        ((eq? (car (car lis)) 'return) #|Mstate return|##)
        (else (evalfile (cdr lis) (evaluate (car lis) state)))))

(define evaluate
    (lambda (statement state)
    (cond
    ((eq? (car statement) 'var) #| mstate for var assignment |# )
    ((eq? (car statement) '=) #|mstate for =|#)
    ((eq? (car statement) 'return) #|mstate for return|#)
    ((eq? (car statement) 'if) #|mstate for if|#)
    ((eq? (car statement) 'while) #|mstate for while|#)
    (else (error)))))

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
            )

(define M-statement
    (lambda (exp state)
        (if (eq? (checkType exp) 'bool)
            ; returns bool
            (cond
                ((eq? (operator exp) '!) (M-boolean exp not))
                ((eq? (operator exp) '==) (M-boolean exp eq?))
                ((eq? (operator exp) '!=) (M-boolean exp (lambda (v w) (not (eq? v w)))))
                ((eq? (operator exp) '&&) (M-boolean exp and))
                ((eq? (operator exp) '||) (M-boolean exp or)))
            )
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

; (== (== x x) y)
(define M-boolean
    (lambda (exp op)
        (if (eq? op not) 
            (M-statement(leftoperand exp))
            (op (M-statement(leftoperand exp)) (M-statement(rightoperand exp)))

(define M-compare
    (lambda (exp op)
        (op (M-statement(leftoperand exp)) (M-statement(rightoperand exp))))
    ))

;(M-integer '(* (+ 4 3) (- 2 1))) => 7
(define M-integer
  (lambda (exp)
    (cond
      ((number? (leftoperand) exp))
      ((eq? (operator exp) '+) (+ (M-integer(leftoperand exp) (M-integer (rightoperand exp)))))
      ((eq? (operator exp) '-) (- (M-integer (leftoperand exp) (M-integer (rightoperand exp)))))
      ((eq? (operator exp) '*) (floor(* (M-integer (leftoperand exp) (M-integer (rightoperand exp))))))
      ((eq? (operator exp) '%) (modulo (M-integer (leftoperand exp) (M-integer (rightoperand exp)))))
      ((eq? (operator exp) '/) (quotient (M-integer (leftoperand exp) (M-integer (rightoperand exp)))))
      (else (error 'bad operator))))

(define M-if
    (lambda (exp statement1 statement2 state) 
        (if (M-boolean exp)
        
        (else)))

(define M-while
    (lambda (exp body state)
        (if (M-boolean exp)
            (M-while exp body (M-statement body (M-boolean exp state)))
            state
    )))

(define M-return
    (lambda (return exp state)
    ))

(define M-assignment
    (lambda (var exp state)
    ))

(define M-declaration
    (lambda (var value state)
        (cond
            ((null? value) error)

    
    ))