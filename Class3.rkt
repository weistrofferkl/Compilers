#lang racket
; Goal of Lexer is to generate tokens
;lexeme - grabs whatever was matched and returns it

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))
(require test-engine/racket-tests)

(define-tokens value-tokens (NUMBER ID))
(define-empty-tokens operators (ADD MULT LPAREN RPAREN))
(define-empty-tokens keywords (NI NEEWOM))
(define-empty-tokens end (EOF))


(define calclexer
  (lexer
   ["ni" (token-NI)]
   ["neewom" (token-NEEWOM)]
   [(:+ numeric) (token-NUMBER lexeme)]; ":+" means 1 or more
   [(:: alphabetic (:*(:or alphabetic numeric))) (token-ID lexeme)]; "::" means concatenate
   [whitespace (calclexer input-port)]
   [(eof) (token-EOF)]
   
   ))

(define (lexstr str)
  (letrec ([in (open-input-string str)]
        [lexfun (λ (i) (let ([tok (calclexer i)])
                         (cond [(eq? tok (token-EOF)) '()]
                               [else (cons tok (lexfun i))])))])
    (lexfun in)))

;(check-expect (lexstr "ni") (token-NI))
;(check-expect (lexstr "5") (token-NUMBER "5"))
;(check-expect (lexstr "5") (token-NUMBER "56"))

(test)
(command-line
 #:args (filename)filename)