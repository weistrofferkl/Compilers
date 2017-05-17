#lang racket
(require "Project2.rkt"
         "Project3TypeCheck.rkt")

(provide (all-defined-out))

; walk the ast with a given function, which should take two arguments,
; the first being the node, the second being a symbol which will be either
; 'pre or 'post. Generally, we call your function on each node twice if
; it has children, 'pre before visiting any children, and 'post after
; visiting the children, allowing you to insert behavior before and after.
; If a node doesn't have children, we simply use 'pre only
(define (ast-walk fun ast)
  (match ast
    ; these are in essence the base cases, simply because you don't walk
    ; any children, you're done at this point and return control upwards
    ['() (fun '() 'pre)]
    [(NumExpr _) (fun ast 'pre)]
    [(StringExpr _) (fun ast 'pre)]
    [(BoolVal _) (fun ast 'pre)]
    [(PengExpr) (fun ast 'pre)]
    [(NoVal) (fun ast 'pre)]

    ; handling lists of nodes, these are always just sequential
    [(list node) (ast-walk fun node)]
    [(cons node rest) (ast-walk fun node)
                      (ast-walk fun rest)]

    ; simple math expressions
    [(MathExpr e1 _ e2) (fun ast 'pre) (ast-walk fun e1) (ast-walk fun e2) (fun ast 'post)]
                         

    ; greater than, less than, etc
    [(BoolExpr e1 _ e2) (fun ast 'pre) (ast-walk fun e1) (ast-walk fun e2) (fun ast 'post)]
    
    ; logic expression, & or |
    [(LogicExpr e1 _ e2) (fun ast 'pre) (ast-walk fun e1) (ast-walk fun e2) (fun ast 'post)]
    
    ; variable declarations, we'll visit the init
    [(VarDecl _ _ init) (fun ast 'pre) (ast-walk fun init) (fun ast 'post)]

    ; handle name types, which may be recursive (if next is defined),
    ; but this will need to be handled at the fun level (i.e., your fun must deal
    ; with this simply because you usually need prior information related to
    ; the recursive definitions
    [(NameType _ _ _) (fun ast 'pre)]
     
    ; handle records like names
    [(RecordType _ _ _) (fun ast 'pre)]

    ; handle array declarations
    [(ArrayType _ _ _) (fun ast 'pre)]

    ; now for function declarations, which we do the same thing--and note we'll
    ; probably call walk-ast on the body of the function in whatever function
    ; we have, but if we have mutually recursive function definitions, then we
    ; need to handle this case inside fun itself to gather info instead of just
    ; walking the body first
    [(FunDecl _ _ _ _ _) (funForEach
                          (λ (fundecl)
                            (match fundecl
                              [(FunDecl name args rettype body _)
                               (fun fundecl 'pre) (ast-walk fun body) (fun fundecl 'post)]))
                            ast)]

    ; variable expressions, probably means lookups of some sort
    [(VarExpr name) (fun ast 'pre)]

    ; for records, we just walk the record creation, though assignments will have ast
    ; nodes in them, we generally need to match them up with the record creation
    [(NewRecordExpr _ _) (fun ast 'pre)]

    ; for record field access, we just walk the node and handle the details in fun
    [(RecordExpr _ _) (fun ast 'pre)]

    ; we walk the index first, then the initialization values before we walk the array
    [(NewArrayExpr _ num-elements init-val)
     (fun ast 'pre) (ast-walk fun num-elements) (ast-walk fun init-val) (fun ast 'post)]

    ; for subscripts, we walk the index first, then the ast node
    [(ArrayExpr _ index) (fun ast 'pre) (ast-walk fun index) (fun ast)]

    ; let expressions are also complicated, we generally need to push scope as
    ; we walk the decls, then we can walk the exprs, then we can pop scope, so
    ; most likely if you did this, you'd handle both pre and post
    [(LetExpr decls body)
     (fun ast 'pre) (ast-walk fun decls) (ast-walk fun body) (fun ast 'post)]

    ; we walk the test first, then true branch, then false branch
    [(IfExpr test true-branch false-branch)
     (fun ast 'pre) (ast-walk fun test) (ast-walk fun true-branch) (ast-walk fun false-branch) (fun ast 'post)]
    
    [(FuncallExpr _ args) (fun ast 'pre) (ast-walk fun args) (fun ast 'post)]

    ; walk the test, then the body, then the whole thing
    [(WhileExpr test body) (fun ast 'pre) (ast-walk fun test) (ast-walk fun body) (fun ast 'post)]

    ; with expr, walk the from, then to, then the whole thing (we can't walk the body)
    ; because we typically have to bind variables to the environment before doing so
    [(WithExpr _ fromexpr toexpr body)
     (fun ast 'pre) (ast-walk fun fromexpr) (ast-walk fun toexpr) (ast-walk fun body) (fun ast 'post)]

    ; nothing to see here
    [(BreakExpr) (fun ast 'pre)]

    ; walk the expr first, then the assignment
    [(AssignmentExpr name expr) (fun ast 'pre) (ast-walk fun expr) (fun ast 'post)]
    
    [_ (printf "~a Node not implemented in walk-ast!~n" ast)]))