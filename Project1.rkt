#lang racket
(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM ID STRING))
(define-empty-tokens paren-types (LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE)) ;done
(define-empty-tokens operators (ADD MULT DIV SUB DOT)) ;done
(define-empty-tokens punct (COMMA COLON SEMI)) ; done
(define-empty-tokens comparators (EQ NE LT GT LE GE))
(define-empty-tokens boolops (BOOLOR BOOLAND))

(define-empty-tokens keywords (AND ARRAY AS BREAK DO ELSE END IF IN IS ;done
 JUNIPER KIND LET NEEWOM NI NOW OF PENG THEN
 TO WHILE WITH))

(define-empty-tokens endoffile (EOF)) ;done

(define nilexer
  (lexer
   ;Keywords
   ["neewom" (token-NEEWOM)]
   ["ni" (token-NI)]
   ["and" (token-AND)]
   ["array" (token-ARRAY)]
   ["as" (token-AS)]
   ["break" (token-BREAK)]
   ["do" (token-DO)]
   ["else" (token-ELSE)]
   ["end" (token-END)]
   ["if" (token-IF)]
   ["in" (token-IN)]
   ["is" (token-IS)]
   ["juniper" (token-JUNIPER)]
   ["kind" (token-KIND)]
   ["let" (token-LET)]
   ["now" (token-NOW)]
   ["of" (token-OF)]
   ["peng" (token-PENG)]
   ["then" (token-THEN)]
   ["to" (token-TO)]
   ["while" (token-WHILE)]
   ["with" (token-WITH)]
   ;Paren Types
   ["#/(" (token-LPAREN)]
   ["#/)" (token-RPAREN)]
   ["#/[" (token-LBRACKET)]
   ["#/]" (token-RBRACKET)]
   ["#/{" (token-LBRACE)]
   ["#/}" (token-RBRACE)]
   ;Operators ADD MULT DIV SUB DOT
   ["#/+" (token-ADD)]
   ["#/*" (token-MULT)]
   ["#//" (token-DIV)]
   ["#/-" (token-SUB)]
   ["#/." (token-DOT)]
   ;Punctuation COMMA COLON SEMI
   ["#/," (token-COMMA)]
   ["#/:" (token-COLON)]
   ["#/;" (token-SEMI)]

   [(:+ numeric) (token-NUM lexeme)]; ":+" means 1 or more
   [whitespace (nilexer input-port)]
   [(eof) (token-EOF)]
   ))
   