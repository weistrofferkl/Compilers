#lang racket
;(open-input-string "")
;In this assignment, you will implement an interpreter for fully parenthesized expressions.
;For example: ((2+3)*5) should output 25. 


; defines a token struct, which contains a type (to indicate the type of token)
; and a representation, which is the string representation of the token

; a structure to hold a digit, this is used by the lexer
; a token type will be 'op, 'lparen, 'rparen, or 'digit or 'eof
; repr is a character representation

(define-struct token (type repr) #:transparent

  
   ; a guard is a function that 'tests' the values you put into the structure
   ; remember: racket is dynamically typed so you kinda have to check things to
   ; save yourself a ton of grief later (trust me)
  
   #:guard (λ (type repr struct-name)
     (if (not (is-token-type? type))
         (error "expected a proper token-type which is-token-type? returns true from, got" type)
         (if (and (not (eq? eof repr)) (not (char? repr)))
             (error "expected a string? or eof? for token-repr, got" repr)
             (values type repr)))))

(define (is-token-type? type)
  (cond
    [(eq? type 'eof) #t]
    [(eq? type 'lparen) #t]
    [(eq? type 'rparen) #t]
    [(eq? type 'op) #t]
    [(eq? type 'digit) #t]
    [else #f]))


; input-port -> token
; returns the next input token from the input port
(define (get-next-token input-port)
  (let ([x (read-char input-port)])
    (cond
               [(eq? x eof) (token 'eof x)]
               [(eq? x #\() (token 'lparen x)]
               [(eq? x #\)) (token 'rparen x)]
               [(eq? x #\+) (token 'op x)]
               [(eq? x #\*) (token 'op x)]
               [(char-numeric? x) (token 'digit x)]
               [else (token 'invalid x)]
               )
    )
    
 )

; creates a lexer (a struct, perhaps?) that prepares to lex a particular file
; string -> 0 argument function that returns the next token on the string
; this function creates a function that uses get-next-token on the string that was passed in,
; notice how we pass create the input by using open-input-string
(define (lexstr str)
          (let ([x (open-input-string str)])
             (λ () (get-next-token x)))
 )
    
 

 
; the parser should take a lexer and should return a struct or function
; that allows you to parse the rest of its contents

; (() -> token) -> (ast-node | ast-expr-node)
; the parser takes a function (probably produced by lexstr) that
; lexes the contents of the input stream

(define (parser lex) ; function return a token
  (let ([x lex])
  (cond
    [(eq? x 'lparen) ast-expr-node ]
    [else (ast-node x)]
    )
   
  )
)

; value node for numbers
(struct ast-node (val) #:transparent)

; expression nodes for operators 
(struct ast-expr-node (operator left-child right-child) #:transparent)


; ast -> val
; this function takes an AST and returns the calculated value
; note that we assume the tree was built correctly!
(define (eval ast)
   (match ast
     ([ast-node v] v) ...))

; str -> val
; takes a string, creates the lexer and parser and then evaluates it
(define (evalstr str)
  (let ([lexer (lexstr str)])
    (eval (parser lexer))))
  
;(define (parse-operator ... ) ... )
;(define (parse-expression ... )...)
