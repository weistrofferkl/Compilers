#lang racket

(require "Emitter.rkt"
         "names.rkt"
         (prefix-in t: "Project3Types.rkt")
         "Project2.rkt" ;parser
         "Project3TypeCheck.rkt" ;Typechecker
         "errors.rkt"
         "log.rkt")

(provide (all-defined-out))

(define (trans str)
  (clear-errors)
  (clear-writers)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse error")
        (let ([ty (typecheck-ast ast)])
          (if (error-generated?)
              (error "cannot translate due to type error")
              (begin
                (ast->llvm (first ast))
                (finish-emission)))))))

  
(define (translate-str str)
  ; clear the errors first
  (clear-errors)
  (clear-writers)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse errors")
        (let ([ty (typecheck-ast ast)])
          (if (error-generated?)
              (error "cannot translate due to type error")
              (begin
                (translate-ast ast)))))))

(define (translate-ast ast)
  ; begin with the prelude
  (emit-header)
  (emit-main-header)
  (ast->llvm (first ast))
  (emit-main-trailer)
  (finish-emission))

(define (ast->llvm ast)
  (match ast
    ;Boolean Expr, Logic Expr, branching
    
    ; deal with lists, like in let expressions
    ['() '()]
    [(cons first rest) (begin (ast->llvm first) (ast->llvm rest))]
    
    ; integer literals
    [(NumExpr val) (numexpr->llvm ast val)]

    ;Booleans
    [(BoolExpr V1 op V2) (boolexpr->llvm ast V1 op V2)]

    ;Logics
    [(LogicExpr V1 op V2) (logicexpr->llvm ast V1 op V2)]

    [(StringExpr val) (stringexpr->llvm ast val)]

    ; variable declarations!
    [(VarDecl _ _ _) (vardecl->llvm ast)]
    
    ; function calls
    [(FuncallExpr _ _) (funcall->llvm ast)]
       
    ; variable expressions
    [(VarExpr _) (var->llvm ast)]

    ;Assignment Expression
    [(AssignmentExpr _ _) (assexpr->llvm ast)]

    ; let expressions--need these for any declarations to work!
    [(LetExpr _ _) (letexpr->llvm ast)]
    

    ;Math Expression
    [(MathExpr V1 op V2) (mathexpr->llvm ast V1 op V2)]

    [(IfExpr testExpr true-branch false-branch) (ifexpr->llvm ast)]

    
    
    [_ (error "Translation node " ast " not implemented yet!")]))        

; emits a numeric literal
(define (numexpr->llvm node val)
  ; literal nums can go in registers
  (let ([result (emit-math 'add val "0")])
    (add-note node 'result result)))

;Let Expressions
(define (letexpr->llvm node)
  (match node
    [(LetExpr decls exprs)
     (begin
       (let ([declsRet (ast->llvm decls)]
             [exprsRet (ast->llvm exprs)])
           
         (add-note node 'result (get-note (last exprs) 'result))
         ))]))

(define (ifexpr->llvm node)
  (match node
    [(IfExpr testExpr true-branch false-branch)
     (begin
     (ast->llvm testExpr)
     (let
         ([branch1 (make-label-result)]
          [branch2 (make-label-result)]
          [elseBranch (make-label-result)]
          [testCaseRes (get-note testExpr 'result)])

       (emit-branch testCaseRes)
       

       ))]))
    ; ]))
  

;VarDecls
(define (vardecl->llvm node)
  (match node
    [(VarDecl type id expr)
     (begin
     ;  (printf "~n HELLO FROM VARDECL ")
       (let* ([nodeRet (ast->llvm expr)]
              [emitRet (emit-varDecl type id (get-note expr 'result))]
              [varvalue (get-note node 'type)])
        ; (printf "~n HELLO FROM VARDECL 2")
         
         (add-note node 'result emitRet)
         (t:set-VarValue-result! varvalue emitRet))
       )]))

;var expression
(define (var->llvm node)
  
  (match node
    [(VarExpr name)
     (printf "~n Var Value from VE~a" (t:VarValue name #f)) 
     (let* ([type (get-note node 'type)]
            [result (t:VarValue-result (get-note node 'varval))]
            [emitRet (emit-varExpr type result)])
       (printf "~n Result from Let : ~a" result)
       (printf "~n EmitRes from Let : ~a" emitRet)

       (add-note node 'result emitRet))]))


                        


;Assignment Expression
(define (assexpr->llvm node)
  
  (match node
    [(AssignmentExpr name expr)
     (ast->llvm name)
     (let* ([nodeRet(ast->llvm expr)]

            
            [varvalue  (get-note name 'varval)]
            [emitRet (emit-assign (t:VarValue-result varvalue) (get-note expr 'result))])
            ;[varRes (t:VarValue-result varvalue)])
      ; (printf "~n HELLO SAYS LE ASSIGNMENT!!!!")
       ;(printf "~n Var Value from AssExpr ~a" emitRet)
       ;(add-note node 'result varvalue) ; was prev. (add-note node 'result emitRet)
       (add-note node 'result emitRet)
       (t:set-VarValue-result! varvalue emitRet)
       )]))

       

;String Expressions
(define (stringexpr->llvm node val)
  (let ([result (emit-literal-string val)])
    (add-note node 'result result)))

;Math Expressions
(define (mathexpr->llvm node v1 op v2)
  (ast->llvm v1)
  (ast->llvm v2)
  
  (let* ([var1 (get-note v1 'result)]
         [var2 (get-note v2 'result)]
         [result (emit-math op var1 var2)])
    (add-note node 'result result)))

;Boolean Expressions
(define (boolexpr->llvm node v1 op v2)
  (ast->llvm v1)
  (ast->llvm v2)

  (let* ([var1 (get-note v1 'result)]
         [var2 (get-note v2 'result)]
         [result (emit-bool op var1 var2)])
    (add-note node 'result result)))

;Logic Expressions
(define (logicexpr->llvm node v1 op v2)
  (ast->llvm v1)
  (ast->llvm v2)

  (let* ([var1 (get-note v1 'result)]
         [var2 (get-note v2 'result)]
         [result (emit-logic op var1 var2)])
    (add-note node 'result result)))

;parameters, function call that uses those parameter names
(define (funcall->llvm node)
  (match node
    [(FuncallExpr name args)
     (let ([results 
            (map (lambda (arg)
                   (ast->llvm arg)
                   
                   (get-note arg 'result)) args)]
           [types
            (map (lambda (arg)
                   ;(ast->llvm args)
                   (get-note arg 'type))args)])
     
       (add-note node 'result (emit-funcall name results types (get-note node 'type))))]))
