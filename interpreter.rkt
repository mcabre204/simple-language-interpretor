#lang racket

(require "simpleParser.rkt")
(require "lex.rkt")

;;==eq=============================================================
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

(define evaluate{
    (lambda (statement state)
    (cond
    ((eq? (car statement) 'var) #| mstate for var assignment |# )
    ((eq? (car statement) '=) #|mstate for =|#)
    ;((eq? (car statement) 'return) #|mstate for return|#)
    ((eq? (car statement) 'if) #|mstate for =|#)
    ((eq? (car statement) 'while) #|mstate for =|#)
    (else (error))))
}