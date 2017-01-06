#lang racket
;(open-input-string "")
;In this assignment, you will implement an interpreter for fully parenthesized expressions.
;For example: ((2+3)*5) should output 25. 


; defines a token struct, which contains a type (to indicate the type of token)
; and a representation, which is the string representation of the token
(define-struct token (type repr) #:transparent)


; creates a lexer (a struct, perhaps?) that prepares to lex a particular file
(define (lexer filename)
  (struct(
          (let ([x (read-char(open-input-string filename))])
          (cond
               [(eq? x eof) (token ENDOFFILE x)]
               [(eq? x #\() (token LEFTPAREN x)]
               [(eq? x #\)) (token RIGHTPAREN x)]
               [(eq? x #\+) (token ADD x)]
               [(eq? x #\*) (token MULT x)]
               [(eq? x char-numeric) (token DIGIT x)]
               )
            )
          )
    )
  )

; gets the next token on the input stream associated with the lexer
(define (get-next-token lexer)
  (read-char lexer)
 )
  

; the parser should take a lexer and should return a struct or function
; that allows you to parse the rest of its contents
(define (parser lex) ... )

  
(define (parse-operator ... ) ... )
(define (parse-expression ... ))
