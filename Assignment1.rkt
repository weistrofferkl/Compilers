#lang racket
;(open-input-string "")
;In this assignment, you will implement an interpreter for fully parenthesized expressions.
;For example: ((2+3)*5) should output 25. 


; defines a token struct, which contains a type (to indicate the type of token)
; and a representation, which is the string representation of the token
(define-struct token (type repr) #:transparent
  (struct (token LEFTPAREN #\()
    (token RIGHTPAREN #\))
    (token ADD "+")
    (token MULT "*")
    (token DIGIT (or "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
    )
  )

; creates a lexer (a struct, perhaps?) that prepares to lex a particular file
(define (lexer filename)
  (struct(
          (open-input-string filename)
          (cond ([eof] "END OF FILE")
                (else get-next-token lexer)))
  )


; gets the next token on the input stream associated with the lexer
(define (get-next-token lexer)
  (read-char lexer)
 )
  

; the parser should take a lexer and should return a struct or function
; that allows you to parse the rest of its contents
(define (parser lex) ... )

  
(define (parse-operator ... ) ... )
(define (parse-expression ... ) ... )
