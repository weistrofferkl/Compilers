#lang racket
(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM ID STRING))
(define-empty-tokens paren-types (LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE))
(define-empty-tokens operators (ADD MULT DIV SUB DOT))
(define-empty-tokens punctuation (COMMA COLON SEMI))
(define-empty-tokens comparators (EQ NE LT GT LE GE))
(define-empty-tokens boolops (BOOLOR BOOLAND))

(define-empty-tokens keywords (AND ARRAY AS BREAK DO ELSE END IF IN IS
 JUNIPER KIND LET NEEWOM NI NOW OF PENG THEN
 TO WHILE WITH))

(define-empty-tokens endoffile (EOF))

