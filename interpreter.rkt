#lang racket
(require "simpleParser.rkt")
(require "lex.rkt")
;; =====================================
;; The Interpreter
;; Jake Prusky, Marco Cabre, Morris Lee
;; CSDS 345
;; =====================================

(define interpret
	(lambda (file)
		 (evalfile (parser file) init)))
         
(define init '(()()))

(define evalfile
    (lambda (lis state)
        (call/cc
            (lambda (break)
                (M-state-statement-cc lis state break (lambda (v1) (error 'BreakError)) (lambda (v2) (error 'ContinueError)) (lambda (v3) (error 'ThrowError))))

;; =====================================
;; HELPER FUNCTIONS
;; =====================================

(define arg1 (lambda (lis) (cadr lis)))
(define arg2 (lambda (lis) (caddr lis)))
(define arg3 (lambda (lis) (cadddr lis)))
(define leftoperand (lambda (exp) (cadr exp)))
(define operator (lambda (exp) (car exp)))
(define rightoperand (lambda (exp) (caddr exp)))
(define myor (lambda (v w) (or v w)))
(define myand (lambda (v w) (and v w)))

;; =====================================
;; STATE LAYER FUNCTIONS
;; =====================================

; returns if the variable is declared in a given state, no matter what layer it's in
(define declared
    (lambda (var state)
        (cond
            ((null? state)           #f)
            ((null? (car state))     #f)
            ((eq? var 'return_value) #t)
            ((eq? var (caar state))  #t)
            (else                    (declared var (list (cdar state) (cdadr state)))))))

; gets the value of a declared variable in a state
; will return the most local variable
(define getValue
    (lambda (var state)
        (cond
        (member*? var (caar state) )
        ((eq? var (caar state))    (caadr state))
        (else                      (getValue var (list (cdar state) (cdadr state)))))))

(define getValueDepth
    (lambda (var state)
        (cond
        ((null? (cdr state)) getValue state)
        ((member*? var (caar state)) getValue (car state))
        (else (getValueDepth (cdr state))))))
    
; helper function for setValue
; will always set the most local variable
(define setValue-helper
    (lambda (var val state return)
      (cond
        ((null? (car state))        (return (list var) (list val)))
        ((eq? (caar state) var)     (return (car state) (cons val (cdadr state))))
        (else                       (setValue-helper var val (list (cdar state) (cdadr state)) (lambda (v w) (return (cons (caar state) v) (cons (caadr state) w)))))))))

; sets the value of a declared variable, if the variable is not declared, declare with input value 
(define setValue
    (lambda (var val state)
        (setValue-helper var val state (lambda (v w) (list v w)))))

(define addLayer
    (lambda (state)
        (cons init state)))


(define removeLayer
    (lambda (state)
        (if (null? (cdr state)
        (error 'NOSTATE)
        (cdr state)))))

(define member*?
  (lambda (a lis)
    (cond
      ((null? lis)       #f)
      ((pair? (car lis)) (or (member*? a (car lis)) (member*? a (cdr lis))))
      ((eq? a (car lis)) #t)
      (else              (member*? a (cdr lis))))))


;; =====================================
;; M VALUE FUNCTIONS
;; =====================================

; Mvalue takes an expression exp and a state and returns the evaluation of exp in state
(define M-value
    (lambda (exp state)
        (if (list? exp) 
            (cond
               ((eq? (operator exp) '!)             (M-boolean exp not state))
               ((eq? (operator exp) '==)            (M-boolean exp eq? state))
               ((eq? (operator exp) '!=)            (M-boolean exp (lambda (v w) (not (eq? v w))) state))
               ((eq? (operator exp) '&&)            (M-boolean exp myand state))
               ((eq? (operator exp) '||)            (M-boolean exp myor state))  
               ((eq? (operator exp) '>=)            (M-boolean exp >= state))
               ((eq? (operator exp) '<=)            (M-boolean exp <= state))
               ((eq? (operator exp) '<)             (M-boolean exp < state))
               ((eq? (operator exp) '>)             (M-boolean exp > state))    
               ((eq? (operator exp) '+)             (M-integer exp + state))
               ((eq? (operator exp) '-)             (M-integer exp - state))
               ((eq? (operator exp) '*)             (M-integer exp * state))
               ((eq? (operator exp) '%)             (M-integer exp modulo state))
               ((eq? (operator exp) '/)             (M-integer exp (lambda (v w) (floor (/ v w))) state))
               (else                                (error 'bad_operator)))
            (cond
                ((or (eq? exp 'true) (eq? exp #t))  #t)
                ((or (eq? exp 'false) (eq? exp #f)) #f)
                ((number? exp)                      exp)
                ((if (declared exp state)
                    (getValue exp state)
                    (error 'bad_operator)))
                (else                               (error 'bad_operator))))))

; handles expressions that return booleans for Mvalue
(define M-boolean
    (lambda (exp op state)
        (if (eq? op not) 
            (not(M-value (leftoperand exp) state))
            (op (M-value (leftoperand exp) state) (M-value (rightoperand exp) state)))))

; handles expressions that return integers for Mvalue
(define M-integer
  (lambda (exp op state)
    (if (null? (cddr exp))
        (M-value (- (leftoperand exp)) state)
        (op (M-value (leftoperand exp) state) (M-value (rightoperand exp) state)))))

;; ====================================
;; M STATE FUNCTIONS
;; ====================================

(define M-state-statement-cc
    (lambda (lis state return break continue throw)
        (cond
            ((null? lis) state)
            ((eq? (car lis) 'return)  (break (Mboolean (cadar lis) state))))
            ((eq? (car lis) 'var)     (if (eq? (length statement) 2)
                                          (M-state-statement-cc (cdr lis) (M-declaration (arg1 lis) state) return break continue throw))
                                          (M-state-statement-cc (cdr lis) (M-declaration-val (arg1 lis) (M-value (arg2 lis) state)) return break continue throw)))
            ((eq? (car lis) '=)       (M-state-statement-cc (cdr lis) ((M-assignment (arg1 lis)  (M-value (arg2 lis) state) state)) return break continue throw))
            ((eq? (car lis) 'if)      (if (eq? (length lis) 3)
                                          (M-state-statement-cc (cdr lis) (M-if (arg1 lis) (arg2 lis) state return break continue throw) return break continue throw)
                                          (M-if-else (arg1 lis) (arg2 lis) (arg3 lis) state return break continue throw) return break continue throw))
            ((eq? (car lis) 'while)   (M-state-statement-cc (cdr lis) 
                                                (call/cc
                                                    (lambda (newBreak)
                                                        (statement-cc (cdr lis)
                                                            (M-while (arg1 lis) (arg2 lis) state return newBreak continue throw)
                                                            return newBreak continue throw))) return break continue throw))
            ((eq? (car lis) 'begin)   (M-state-statement-cc (cdr lis) (removeLayer (M-state-statement-cc (cdr lis) (addLayer state) return break continue throw)) return break continue throw)) return break continue throw))
            ((eq? (car lis) 'break)   (whileBreak (removeLayer state)))
            ((eq? (car lis) 'try)     (M-state-statement-cc (cdr lis) (M-try (arg1 lis) return break continue throw) return break continue throw))
            ((eq? (car lis) 'throw)   (throw IDFK
            ((eq? (car lis) 'finally) (M-state-statement-cc (cdr lis) (M-try (arg1 lis) return break continue throw))
            (else                     (error 'BAD_STATEMENT)))))

; State function for if statements
(define M-if
    (lambda (condition statement1 state return break continue throw)
        (if (M-value condition state)
            (M-state-statement-cc statement1 state return break continue throw)
            (M-state-statement-cc condition state return break continue throw))))

; State function for if-else statements
(define M-if-else
    (lambda (condition statement1 statement2 state return break continue throw)
        (if (M-value condition state)
            (M-state-statement-cc statement1 state return break continue throw)
            (M-state-statement-cc statement2 state return break continue throw))))

; CC helper function for M-while
(define M-while-cc
    (lambda (condition body state return break continue throw)
        (if (M-value (car condition) state) 
            (M-while-cc condition (M-state-statement-cc (cons (cadr condition) '()) state return break continue throw) return break continue throw))
            (else                             state))))
(define M-while
    (lambda (condition body state return break continue throw)
        (if (M-value (car condition) state) 
            (M-while condition 
                (call/cc
                    (lambda (newContinue)
                        (M-while-cc condition state return break newContinue throw)))
                return break continue throw))
            (else                             state))))

; State function for while loops
(define M-while
    (lambda (condition body state return break continue throw)
        (if (M-value condition state)
            (M-while condition body (M-state-statement body state return break continue throw))
            state))

; State function for return keyword
(define M-return
    (lambda (exp state return break continue throw)
        (setValue 'return_value (M-value exp state) state)))

; State function for assignment statements
(define M-assignment
    (lambda (var exp state return break continue throw)
        (if (declared var state)
            (setValue var (M-value exp state) state)
            (error 'variable-not-declared))))

; State function for declaration statements
(define M-declaration
    (lambda (var state return break continue throw)
        (if (declared var state)
            (error 'variable_already_declared)
            (setValue var 'error state))))

; State function for declaration assignment statements
(define M-declaration-val
    (lambda (var val state return break continue throw)
        (if (declared var state)
            (error 'variable_already_declared)
            (setValue var val state))))

(define M-try
    (lambda (lis state return break continue throw)
        (cond
            ((null? lis) state)
            ((null? (cadr lis)) (M-state-statement-cc (cddr lis)
                (call/cc
                    (lambda (newThrow1)
                        (M-state-statement-cc (car lis) state return break continue newThrow1)))
                return break continue throw))
            (hasEnd lis) (M-state-statement-cc (cddr lis) (M-catch (catchExp lis)
                (call/cc
                    (lambda (newThrow2)
                        (M-state-statement-cc (car lis) state return break continue newThrow2)))
                return break continue throw))
            (else (M-catch (catchExp lis)
                (call/cc
                    (lambda (newThrow3)
                        (M-state-statement-cc (car lis) state return break continue newThrow3)))
                return break continue throw)))))

; Helpers For M-Try
; defines the Catch Statement Expression
(define catchExp cdadr)
; checks the end of lis
(define hasEnd
  (lambda (lis)
    (if (null? (caddr lis))
        #f
        #t)))

(define M-catch
    (lambda (statement state break whileContinue whileBreak throw)
        (cond 
            (null? statement)        'NO_CATCH_STATEMENT)
            ((member*? 'throw state) (M-state-statement-cc (cadr statement) (M-declare (list (caar statement) (getValue 'throw state)) state) return break continue throw))
            (else                    state))))