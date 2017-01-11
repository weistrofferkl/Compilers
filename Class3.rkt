#lang racket
; Goal of Lexer is to generate tokens

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUMBER ID))
(define-empty-tokens operators (ADD MULT LPAREN RPAREN))
define-empty-tokens keywords (NI NEEWOM))


(define calclexer
  (lexer
   ["ni" (token-NI)]))