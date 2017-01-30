#lang racket
(require racket/cmdline)
(require test-engine/racket-tests)
(require parser-tools/cfg-parser)
(require (prefix-in lex: parser-tools/lex))

(require "Project1.rkt")
(provide (all-defined-out))


(struct Peng () #:transparent)

; var declarations
(struct VarDecl (type id expr) #:transparent)

; type declarations--note they can be mutually recursive (using AND)
; so our struct has a link to the next one that belongs here, otherwise
; it's simply '()
(struct NameType (name kind next) #:transparent)
(struct RecordType (name fields next) #:transparent)
(struct ArrayType (name kind next) #:transparent)
(struct TypeField (name kind) #:transparent)

; defines a function in ni
; these consist of the name of the function, the arguments to it,
; the return type (which may be #f if it doesn't have one) and the body
; finally, next points to the next, related definition (for mutual recursion)
(struct FunDecl (name args rettype body next) #:transparent)

; things associated with expressions and lvalues
(struct NumExpr (val) #:transparent)
; variable expressions
(struct VarExpr (name) #:transparent)
; record expressions (name and a list of fields are required)
(struct RecordExpr (name field) #:transparent)
; array expressions (name and expression for the index)
(struct ArrayExpr (name expr) #:transparent)
; function call which is name and a list of arguments
(struct FuncallExpr (name args) #:transparent)
; a string
(struct StringExpr (str) #:transparent)
; a noval 
(struct NoVal () #:transparent)
; a list of declarations for the let and a list of expressions following it
(struct LetExpr (decs exprs) #:transparent)
; arithmetic expression
(struct MathExpr (expr1 op expr2) #:transparent)
; bool op, i.e., comparision
(struct BoolExpr (expr1 op expr2) #:transparent)
; logic op, and or or
(struct LogicExpr (expr1 op expr2) #:transparent)
; assignment in a field for creating a record
(struct FieldAssign (name expr) #:transparent)
; creating a new record
(struct NewRecordExpr (name assignments) #:transparent)
; creating a new array
(struct NewArrayExpr (name expr kind) #:transparent)
; an if expression (hint, you may temporarily use an IfElseExpr if you
; would like to make it easy to see when you're matching or not
(struct IfExpr (test true-branch false-branch) #:transparent)
; a while expression, which is a test and the body
(struct WhileExpr (test body) #:transparent)
; an assignment expression
(struct AssignmentExpr (name expr) #:transparent)
; break expression--this has no arguments
(struct BreakExpr () #:transparent)
; with expression (think: for expression)
(struct WithExpr (idname initexpr fromexpr toexpr) #:transparent)

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

    (decl ; REMEMBER TO DO AND
    
     [(NI ID IS expression) (VarDecl #f $2 $4)]
     [(NI type ID IS expression) (VarDecl $2 $3 $5)]
     [(DEFINE ID KIND AS type) (NameType $2 $5 '() )]
     [(DEFINE ID KIND AS ARRAY OF type) (ArrayType $2 $7 '() )]
     [(DEFINE ID KIND AS LBRACE RBRACE)(RecordType $2 '() '())]
     [(DEFINE ID KIND AS LBRACE recordRecurse RBRACE)(RecordType $2 $6 '())])
    
   (recordRecurse
    [(type ID COMMA recordRecurse) (cons (TypeField $1 $2) $4)]
    [(type ID) (list (TypeField $1 $2))]
    )
   
    (type
     [(ID) $1])

   ; (type-decl
   ;  [(DEFINE ID KIND AS type) (NameType $2 $5 '() )]
   ; [(DEFINE ID KIND AS type OF type) (ArrayType $2 $7 '() )])
    

     (expression
      [(NUM) (NumExpr $1)]
      [(BREAK) (BreakExpr)]
      [(LPAREN RPAREN) (NoVal)]
      [() $1]
      [(PENG) (Peng)]
      [(STRING) (StringExpr $1)]
      )


     
     )))

(define (parse-str str)
     (let ([in (open-input-string str)])
       (port-count-lines! in)
       (niparser (get-tokenizer in))))

(define (get-tokenizer in)
  (lambda () (nilexer in)))


   


