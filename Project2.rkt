#lang racket
(require racket/cmdline)
(require test-engine/racket-tests)
(require parser-tools/cfg-parser)
(require (prefix-in lex: parser-tools/lex))

(require "Project1.rkt")
(provide (all-defined-out))


(struct Peng () #:transparent)

; var declarations
;DONE
(struct VarDecl (type id expr) #:transparent)

; type declarations--note they can be mutually recursive (using AND)
; so our struct has a link to the next one that belongs here, otherwise
; it's simply '()
;DONE
(struct NameType (name kind next) #:transparent)
(struct RecordType (name fields next) #:transparent)
(struct ArrayType (name kind next) #:transparent)
(struct TypeField (name kind) #:transparent)

; defines a function in ni
; these consist of the name of the function, the arguments to it,
; the return type (which may be #f if it doesn't have one) and the body
; finally, next points to the next, related definition (for mutual recursion)
;DONE, DO RECURSION? -DONE?
(struct FunDecl (name args rettype body next) #:transparent)

; things associated with expressions and lvalues
;DONE
(struct NumExpr (val) #:transparent)

; variable expressions
;DONE
(struct VarExpr (name) #:transparent)

; record expressions (name and a list of fields are required)
(struct RecordExpr (name field) #:transparent)

; array expressions (name and expression for the index)
(struct ArrayExpr (name expr) #:transparent)

; function call which is name and a list of arguments
;DONE
(struct FuncallExpr (name args) #:transparent)

; a string
;DONE
(struct StringExpr (str) #:transparent)

; a noval
;DONE
(struct NoVal () #:transparent)

; a list of declarations for the let and a list of expressions following it
;DONE
(struct LetExpr (decs exprs) #:transparent)

; arithmetic expression
;DONE
(struct MathExpr (expr1 op expr2) #:transparent)

; bool op, i.e., comparision
;DONE
(struct BoolExpr (expr1 op expr2) #:transparent)

; logic op, and or or
;DONE
(struct LogicExpr (expr1 op expr2) #:transparent)

; assignment in a field for creating a record
(struct FieldAssign (name expr) #:transparent)

; creating a new record
(struct NewRecordExpr (name assignments) #:transparent)

; creating a new array
;DONE
(struct NewArrayExpr (name expr kind) #:transparent)

; an if expression (hint, you may temporarily use an IfElseExpr if you
; would like to make it easy to see when you're matching or not
(struct IfExpr (test true-branch false-branch) #:transparent)

; a while expression, which is a test and the body
(struct WhileExpr (test body) #:transparent)

; an assignment expression
(struct AssignmentExpr (name expr) #:transparent)

; break expression--this has no arguments
;DONE
(struct BreakExpr () #:transparent)

; with expression (think: for expression)
(struct WithExpr (idname initexpr fromexpr toexpr) #:transparent)


;WithExpr
;AssignmentExpr
;WhileExpr
;IfExpr

(define niparser
  (cfg-parser
   (src-pos)
   (start program)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (if (and (eq? tok-ok? #t) (eq? tok-name 'EOF)) '()
                (printf "Parsing error at line ~a, col ~a: token: ~a, value: ~a, tok-ok? ~a\n"
                        (lex:position-line start-pos) (lex:position-col start-pos) tok-name tok-value tok-ok?))))
   (tokens value-tokens paren-types operators punct comparators boolops keywords endoffile)
   
   (grammar

    (program ;list of expressions or list of declarations
     [(expression program) (cons $1 $2)]
     [(decl program) (cons $1 $2)]
     [(expression) (list $1)]
     [(decl) (list $1)]
     )

    (decl 

   ;  [(NI ID IS expression) (VarDecl #f $2 $4)]
   ;  [(NI type ID IS expression) (VarDecl $2 $3 $5)]
     [(vDecl) (list $1)]
     [(TypeDecls) (list $1)]
     [(vDecl decl) (cons $1 $2)]
     [(TypeDecls decl) (cons $1 $2)]
     [() '()]
     )
    ; [(functDecls) $1])
     

    (vDecl
     [(NI ID IS expression) (VarDecl #f $2 $4)]
     [(NI type ID IS expression) (VarDecl $2 $3 $5)]
     )
    

     
     (TypeDecl
      [(DEFINE ID KIND AS type) (NameType $2 $5 '() )]
      [(DEFINE ID KIND AS ARRAY OF type) (ArrayType $2 $7 '() )]
      [(DEFINE ID KIND AS LBRACE RBRACE)(RecordType $2 '() '())]
      [(DEFINE ID KIND AS LBRACE recordRecurse RBRACE)(RecordType $2 $6 '())]
      [(NEEWOM ID LPAREN recordRecurse RPAREN AS type IS expression) (FunDecl $2 $4 $7 $9 '())]
      
     )
     (TypeDecls
      [(TypeDecl) $1]
      [(TypeDecl AND TypeDecls)
       (match $1
         [(NameType name kind _) (NameType name kind $3)]
         [(ArrayType name kind _) (ArrayType name kind $3)]
         [(RecordType name fields _)(RecordType name fields $3)]
         [(FunDecl name args rettype body _) (FunDecl name args rettype body $3)])])
      
    

     ;FunDecl
     ;REMEBER TO DO RECURSIVE CALL TO NEXT: "finally, next points to the next, related definition (for mutual recursion)"
   ;  (functDecl
   ;   [(NEEWOM ID LPAREN recordRecurse RPAREN AS type IS expression) (FunDecl $2 $4 $7 $9 '())]
   ;  )
   ;  (functDecls
   ;   [(functDecl) $1]
   ;   [(functDecl AND functDecls)
    ;   (match $1
     ;    [(FunDecl name args rettype body _) (FunDecl name args rettype body $3)])]
     ; )
     
    
   (recordRecurse
    [(type ID COMMA recordRecurse) (cons (TypeField $1 $2) $4)]
    [(type ID recordRecurse) (cons (TypeField $1 $2) $3)]
    [(type ID) (list (TypeField $1 $2))]
    [() '()]
    )

   (functPars
    [() '()]
    [(expression COMMA functPars) (cons $1 $3)]
    [(expression) (list $1)])

   (letPars
    [() '()]
    [(expression SEMI letPars) (cons $1 $3)]
    [(expression) (list $1)])

   (LValue
    [(ID) (VarExpr $1)]
    [(LValue DOT ID) (RecordExpr $1 $3)]
    [(LValue LBRACKET expression RBRACKET) (ArrayExpr $1 $3)])

   
    (type
     [(ID) $1])

    (expression
     [(boolExpression) $1])

    ;Math Expressions: With Prescidence 
    (mathExpression
      [(mathExpression ADD term) (MathExpr $1 '+ $3)]
      [(mathExpression SUB term) (MathExpr $1 '- $3)]
      [(SUB term) (MathExpr (NumExpr "0")'- $2)]
      [(term) $1]
      )
    (term
      [(term MULT factor) (MathExpr $1 '* $3)]
      [(term DIV factor) (MathExpr $1 '/ $3)]
      [(factor) $1]
      )
    (factor
     [(simple-expression) $1]
     )



   ;Boolean Expressions
  (boolExpression
      [(logicExpression EQ logicExpression) (BoolExpr $1 'eq $3)]
      [(logicExpression NE logicExpression) (BoolExpr $1 'ne $3)]
      [(logicExpression LT logicExpression) (BoolExpr $1 'lt $3)]
      [(logicExpression GT logicExpression) (BoolExpr $1 'gt $3)]
      [(logicExpression LE logicExpression) (BoolExpr $1 'le $3)]
      [(logicExpression GE logicExpression) (BoolExpr $1 'ge $3)]
      [(logicExpression) $1]
      )

  (logicExpression
   [(logicExpression BOOLOR logTerm) (LogicExpr $1 'or $3)]
   [(logTerm) $1]
   )
  
  (logTerm
   [(logTerm BOOLAND mathExpression) (LogicExpr $1 'and $3)]
   [(mathExpression) $1])
    

     (simple-expression
      [(NUM) (NumExpr $1)]
      [(BREAK) (BreakExpr)]
      [(LPAREN RPAREN) (NoVal)]
      [(ID) (VarExpr $1)]
      ;NewArrayExpression
      [(ID LBRACKET expression RBRACKET OF expression) (NewArrayExpr $1 $3 $6)]
      [(PENG) (Peng)]
      [(STRING) (StringExpr $1)]
      [(LPAREN expression RPAREN) $2]
      [(LValue) $1]
      
      ;FunCallExpr
      [(ID LPAREN functPars RPAREN) (FuncallExpr $1 $3)]

      ;Let
      [(LET decl IN letPars END) (LetExpr $2 $4)]

      ;FieldAssign
      [(ID IS expression)(FieldAssign $1 $3)]

      ;NewRecordExpression
      ([ID LBRACE functPars RBRACE] (NewRecordExpr $1 $3))
      
      ;RecordExpression
     ; ([LValue DOT ID] (RecordExpr $1 $3))
      
      ;ArrayExpression
     ; ([LValue LBRACKET expression RBRACKET] (ArrayExpr $1 $3))
      

      
      )


     
     )))

(define (parse-str str)
     (let ([in (open-input-string str)])
       (port-count-lines! in)
       (niparser (get-tokenizer in))))

(define (get-tokenizer in)
  (lambda () (nilexer in)))


;Test cases:
;(parse-str "let ni x is 5 in 5; 6; end")


