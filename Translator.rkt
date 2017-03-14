#lang racket

(require "Emitter.rkt"
         "names.rkt"
         (prefix-in t: "Project3Types.rkt")
         "Project2.rkt" ;parser
         "Project3TypeCheck.rkt" ;Typechecker
         "errors.rkt"
         "log.rkt")

;DO STRING COMPARISON, With-Loop, FunDecl, Arrays

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
    
    ;If Expression
    [(IfExpr testExpr true-branch false-branch) (ifexpr->llvm ast)]

    ;While Loop
    [(WhileExpr test body) (whileExpr->llvm ast)]

    ;With Loop
    [(WithExpr name init from to) (withExpr->llvm ast)]

    ;Bool Val Thing Chris Had Me Add. 
    [(BoolVal _) (boolval->llvm ast)]

    [(FunDecl name args rettype body next) (funDecl->llvm ast)]

    
    
    [_ (error "Translation node " ast " not implemented yet!")]))

(define (boolval->llvm node)
  (add-note node 'result (emit-boolVal (BoolVal-val node))))


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

;If-Expression
(define (ifexpr->llvm node)
  (match node
    [(IfExpr testExpr true-branch false-branch)
     (begin
       (ast->llvm testExpr)
       (let
           (
            [thenBranch (make-label)]
            [elseBranch (make-label)]
            [endBranch (make-label)]
            [phiThing (make-temp-result)]
            [testCaseRes (get-note testExpr 'result)])

         ; (testExpr) ;branch to then ;branch to false)
         (emit-branch testCaseRes thenBranch elseBranch)
        
         (println (Label-name thenBranch) ": ") 
         (ast->llvm true-branch)
         (emit-jump endBranch)

         (println (Label-name elseBranch) ": ") 
         (ast->llvm false-branch)
         (emit-jump endBranch)

         (println (Label-name endBranch) ": ")

         (add-note node 'result (emit-phi phiThing thenBranch elseBranch (get-note true-branch 'result) (get-note false-branch 'result)))

         ))]))

;While Loop
(define (whileExpr->llvm node)
  (match node
    [(WhileExpr test body)
     (begin
       (let* ([testBranch (make-label)]
              [bodyBranch (make-label)]
              [endBranch (make-label)]
              )
         (emit-jump testBranch)
         (println (Label-name testBranch)": ")
         (ast->llvm test)
         
         (let ([testCaseRes (get-note test 'result)])
           
           (emit-branch testCaseRes bodyBranch endBranch)
           (println (Label-name bodyBranch) ": ")
           (ast->llvm body)
           (emit-jump testBranch)
           (println (Label-name endBranch) ": ")
           (add-note node 'result (emit-WhileVoid)))

         ))]))

;With Loop

(define (withExpr->llvm node)
  (match node
    [(WithExpr name init from to) ;init is body
     (begin
       
       
       (ast->llvm from)
       (let ([fromLabel
              (emit-inital (get-note from 'result))])
         (ast->llvm to)
         (let ([conBranch (make-label)]
               [bodyBranch (make-label)]
               [endBranch (make-label)]
               [holderVar (make-temp-result)]
               [holderVar2 (make-temp-result)]
               )

           (emit-jump conBranch)
           (println (Label-name conBranch) ": ")

           (t:set-VarValue-result! (get-note node 'varvalue) fromLabel)
           (let 
               ([condRes (emit-condition holderVar holderVar2 (result->string fromLabel) (get-note to 'result))])
 
             (emit-branch condRes bodyBranch endBranch)
             (println (Label-name bodyBranch)": ")
             (ast->llvm init)
             (emit-inc holderVar fromLabel) 
             (emit-jump conBranch)
             (println (Label-name endBranch)": ")
             (add-note node 'result (emit-WhileVoid))))))]))

;VarDecls
(define (vardecl->llvm node)
  (match node
    [(VarDecl type id expr)
     (begin
       (let* ([nodeRet (ast->llvm expr)]
              [emitRet (emit-varDecl type id (get-note expr 'result))]
              [varvalue (get-note node 'type)])
         
         (add-note node 'result emitRet)
         (t:set-VarValue-result! varvalue emitRet))
       )]))

;var expression
(define (var->llvm node)
  
  (match node
    [(VarExpr name)
     (let* ([type (get-note node 'type)]
            [result (t:VarValue-result (get-note node 'varval))]
            [emitRet (if (or (or (isPointer result) (global? result)) (in-frame? result)) (emit-varExpr type result) result)])
      
       (add-note node 'result emitRet))]))

(define (isPointer type)

  (if (or (or (t:StringType? 'string) (t:RecordType? type)) (t:ArrayType? type)) #t #f)
  )


;Assignment Expression
(define (assexpr->llvm node)
  
  (match node
    [(AssignmentExpr name expr)
     (ast->llvm name)
     (let* ([nodeRet(ast->llvm expr)]

            
            [varvalue  (get-note name 'varval)]
            [emitRet (emit-assign (t:VarValue-result varvalue) (get-note expr 'result))])
      
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
         [v1Ty (get-note v1 'type)]
         [v2Ty (get-note v2 'type)]
         [result (emit-bool op var1 var2 v1Ty v2Ty)])

    (printf "~n V1 ~a " (t:IntType? (get-note v1 'type)))
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
     (let* ([results 
            (map (lambda (arg)
                   (ast->llvm arg)
                   
                   (get-note arg 'result)) args)]
           [types
            (map (lambda (arg)
                  
                   (get-note arg 'type))args)]
          [nameLabel (t:FunValue-result (get-note node 'FunVal))]
          [nameForPrint (if (eq? nameLabel #f) (string-append "@" name) (result->string nameLabel))]

          )
       (printf "~n NameLab ~a" nameLabel)
     
       (add-note node 'result (emit-funcall nameForPrint results types (get-note node 'type))))]))

;funDecls
(define (funDecl->llvm node)
  (match node
    [(FunDecl name args rettype body next)
     (begin
       (begin-fun-defn)
       ;(printf "~nFunVal: ~a~n" (get-note node 'FunVal))
       (let* ([globalFunc (make-global-result)]
             [funVal  (get-note node 'FunVal)]
             [results
              (map (lambda (arg)
                     (let ([argument (make-temp-result)])
                      
                       (printf "~n arg ~a" arg) 
                       (t:set-VarValue-result! (t:NameTypePair-result arg) argument)
                       argument))
                   
                   (t:FunValue-parameters funVal))])
                       
      
         (printf" ~n results ~a" results)
         (emit-func globalFunc results)
         (t:set-FunValue-result! funVal globalFunc)
         (ast->llvm body)
         
         (let
             ([lastTemp (get-note body 'result)])
           (emit-retType rettype lastTemp))
         (emit-closeBr)
         (end-fun-defn)
         ))]))
       
       
