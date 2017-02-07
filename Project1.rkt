#lang racket
(provide (all-defined-out))
(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))
(require racket/cmdline)
(require test-engine/racket-tests)


(define-tokens value-tokens (NUM ID STRING))
(define-empty-tokens paren-types (LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE)) ;done
(define-empty-tokens operators (ADD MULT DIV SUB DOT)) ;done
(define-empty-tokens punct (COMMA COLON SEMI)) ; done
(define-empty-tokens comparators (EQ NE LT GT LE GE)) ;donne
(define-empty-tokens boolops (BOOLOR BOOLAND)) ;done

(define-empty-tokens keywords (AND ARRAY AS BREAK DO ELSE END IF IN IS ;done
 JUNIPER KIND LET NEEWOM NI NOW OF PENG THEN
 TO WHILE WITH DEFINE))

(define-empty-tokens endoffile (EOF));done

(define nilexer
  (lexer-src-pos ;Tells where syntax error does, also lets you define error functions
   ;Keywords
   ["define" (token-DEFINE)]
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
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\[ (token-LBRACKET)]
   [#\] (token-RBRACKET)]
   [#\{ (token-LBRACE)]
   [#\} (token-RBRACE)]
   ;Operators ADD MULT DIV SUB DOT
   [#\+ (token-ADD)]
   [#\* (token-MULT)]
   [#\/ (token-DIV)]
   [#\-  (token-SUB)]
   [#\. (token-DOT)]
   ;Comparators EQ NE LT GT LE GE
   ["=" (token-EQ)]
   ["<>" (token-NE)]
   ["<" (token-LT)]
   [">" (token-GT)]
   ["<=" (token-LE)]
   [">=" (token-GE)]
   ;boolops:BOOLOR BOOLAND
   [#\| (token-BOOLOR)]
   [#\& (token-BOOLAND)]
   ;Punctuation COMMA COLON SEMI
   [#\, (token-COMMA)]
   [#\: (token-COLON)]
   [#\; (token-SEMI)]
   ;Number
   [(:+ numeric) (token-NUM lexeme)]; ":+" means 1 or more
   ;ID
   [(:: alphabetic (:seq (:* (:or alphabetic numeric #\_ #\-)) (:* #\'))) (token-ID lexeme)];
   ;String - Match first quote, everything that's not a \" and then the final end quote
   ;[(:: #\" (complement (:: #\\ #\")) #\")(token-STRING lexeme)]
   ;[(:: #\" (repetition 0 +inf.0(:or (:: #\\ any-char) (:~ #\" #\\))) #\")(token-STRING lexeme)]
   ;[(:: "//" (:+ (char-complement #\newline))) (return-without-pos (nilexer input-port))]
   [(:: #\" (:* (:or (:: #\\ any-char) (char-complement (char-set "\"\\"))))  #\") (token-STRING lexeme)]
   
   ;Whitespace
   [whitespace (return-without-pos (nilexer input-port))]
   ;comment:
   [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") (return-without-pos (nilexer input-port))]
   [(:: #\/ #\/ (repetition 0 +inf.0 (char-complement  #\newline)) #\newline) (return-without-pos (nilexer input-port))]
   ;EOF
   [(eof) (token-EOF)]
  
   ))

(define (lexstr str)
  (letrec ([in (open-input-string str)]
        [lexfun (λ (i) (let ([tok (nilexer i)])
                         (cond [(eq? (position-token-token tok) (token-EOF)) '()]
                               [else (cons (position-token-token tok) (lexfun i))])))])
    (lexfun in)))

(define (lexfile filename)
    (letrec ([in (open-input-file filename)]
             
             [lexfun (λ (i) (let ([tok (nilexer i)])
                              (cond [(eq? (position-token-token tok) (token-EOF)) '()]
                                    [else (cons (position-token-token tok) (lexfun i))])))])
      (port-count-lines! in)
      (lexfun in)
      ))

;(check-expect (lexstr "and") (list (token-AND)))
;(check-expect (lexstr "array") (list (token-ARRAY)))
;(check-expect (lexstr "as") (list (token-AS)))
;(check-expect (lexstr "break") (list (token-BREAK)))
;(check-expect (lexstr "do") (list (token-DO)))
;(check-expect (lexstr "else") (list (token-ELSE)))
;(check-expect (lexstr "end") (list (token-END)))
;(check-expect (lexstr "if") (list (token-IF)))
;(check-expect (lexstr "in") (list (token-IN)))
;(check-expect (lexstr "is") (list (token-IS)))
;(check-expect (lexstr "juniper") (list (token-JUNIPER)))
;(check-expect (lexstr "kind") (list (token-KIND)))
;(check-expect (lexstr "let") (list (token-LET)))
;(check-expect (lexstr "neewom") (list (token-NEEWOM)))
;(check-expect (lexstr "ni") (list (token-NI)))
;(check-expect (lexstr "now") (list (token-NOW)))
;(check-expect (lexstr "of") (list (token-OF)))
;(check-expect (lexstr "peng") (list (token-PENG)))
;(check-expect (lexstr "]") (list (token-RBRACKET)))
;(check-expect (lexstr "[") (list (token-LBRACKET)))
;(check-expect (lexstr "}") (list (token-RBRACE)))
;(check-expect (lexstr "{") (list (token-LBRACE)))
;(check-expect (lexstr ")") (list (token-RPAREN)))
;(check-expect (lexstr "(") (list (token-LPAREN)))
;(check-expect (lexstr "+") (list (token-ADD)))
;(check-expect (lexstr "-") (list (token-SUB)))
;(check-expect (lexstr "/") (list (token-DIV)))
;(check-expect (lexstr "*") (list (token-MULT)))
;(check-expect (lexstr ";") (list (token-SEMI)))
;(check-expect (lexstr ",") (list (token-COMMA)))
;(check-expect (lexstr ":") (list (token-COLON)))
;(check-expect (lexstr ">") (list (token-GT)))
;(check-expect (lexstr "<") (list (token-LT)))
;(check-expect (lexstr "=") (list (token-EQ)))
;(check-expect (lexstr ">=") (list (token-GE)))
;(check-expect (lexstr "<=") (list (token-LE)))
;(check-expect (lexstr "&") (list (token-BOOLAND)))
;(check-expect (lexstr "|") (list (token-BOOLOR)))
;(check-expect (lexstr "1") (list (token-NUM "1")))
;(check-expect (lexstr "ax") (list (token-ID "ax")))
;(check-expect (lexstr (string-append (string #\") (string #\\) (string #\") "Hello world" (string #\\) (string #\") (string #\")))
; (list (token-STRING (string-append (string #\") (string #\\) (string #\") "Hello world" (string #\\) (string #\") (string #\") ))))
;(check-expect (lexstr "") '())
;
;
;(check-expect (lexstr "ni") (list (token-NI)))
;(check-expect (lexstr "5") (list (token-NUM "5")))
;(check-expect (lexstr "56") (list (token-NUM "56")))
;(check-expect (lexstr "/* Hello
;there */") '())
;
;(check-expect (lexstr "/* comment */ 5 /* comment */") (list (token-NUM "5")))
;
;(check-expect (lexstr "// this is a comment \n5+3") (list (token-NUM "5") (token-ADD) (token-NUM "3")))
;(check-expect (lexstr "\"\\\\\"a\"\"") (list (token-STRING "\"\\\\\"") (token-ID "a") (token-STRING "\"\"")))
;(check-expect (lexstr "\"you had me at \\\"hello\\\"\"") (list (token-STRING "\"you had me at \\\"hello\\\"\"")))
;(test)
;(command-line
; #:args (filename)(begin (printf "compiling ~a\n" filename) (lexfile filename)))