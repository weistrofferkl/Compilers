#lang racket

; import in the test engine stuff, here I assume your assignment file is called a1.rkt
(require test-engine/racket-tests
  "Assignment1.rkt")

; checks on the is-token-type? function
(check-expect (is-token-type? 'op) #t)
(check-expect (is-token-type? 'lparen) #t)
(check-expect (is-token-type? 'rparen) #t)
(check-expect (is-token-type? 'digit) #t)
(check-expect (is-token-type? 'foo) #f)
(check-expect (is-token-type? 'eof) #t)

; error cases on creating tokens
(check-error (token 'op 1))
(check-error (token 'foo "hello"))

; test cases for get-next-token, note this implies get-next-token requires an input-port
; as its sole argument...(that's a hint)
(check-expect (get-next-token (open-input-string "0")) (token 'digit #\0))
(check-expect (get-next-token (open-input-string "1")) (token 'digit #\1))
(check-expect (get-next-token (open-input-string "2")) (token 'digit #\2))
(check-expect (get-next-token (open-input-string "3")) (token 'digit #\3))
(check-expect (get-next-token (open-input-string "4")) (token 'digit #\4))
(check-expect (get-next-token (open-input-string "5")) (token 'digit #\5))
(check-expect (get-next-token (open-input-string "6")) (token 'digit #\6))
(check-expect (get-next-token (open-input-string "7")) (token 'digit #\7))
(check-expect (get-next-token (open-input-string "8")) (token 'digit #\8))
(check-expect (get-next-token (open-input-string "9")) (token 'digit #\9))
(check-expect (get-next-token (open-input-string "+")) (token 'op #\+))
(check-expect (get-next-token (open-input-string "*")) (token 'op #\*))
(check-expect (get-next-token (open-input-string ")")) (token 'rparen #\)))
(check-expect (get-next-token (open-input-string "(")) (token 'lparen #\())
(check-expect (get-next-token (open-input-string "")) (token 'eof eof))
(check-error (get-next-token (open-input-string "a")))

; this should produce an error, it's not correct according to the grammar
(check-error (parser (lexstr "(5)")))
; but the rest of these are fine
(check-expect (parser (lexstr "5")) (ast-node 5))
(check-expect (parser (lexstr "(5+2)")) (ast-expr-node #\+ (ast-node 5) (ast-node 2)))
(check-expect (parser (lexstr "(5+(3+2))")) (ast-expr-node #\+ (ast-node 5) (ast-expr-node #\+ (ast-node 3) (ast-node 2))))
(check-expect (parser (lexstr "((5+3)+2)")) (ast-expr-node #\+ (ast-expr-node #\+ (ast-node 5) (ast-node 3)) (ast-node 2)))
(check-expect (parser (lexstr "((5+3)+(2*3))")) (ast-expr-node #\+ (ast-expr-node #\+ (ast-node 5) (ast-node 3)) (ast-expr-node #\* (ast-node 2) (ast-node 3))))

(check-expect (evalstr "5") 5)
(check-expect (evalstr "(5+2)") 7)
(check-expect (evalstr "(5+(3+2))") 10) 
(check-expect (evalstr "((5+3)+2)") 10) 
(check-expect (evalstr "((5+3)+(2*3))") 14) 

(test)