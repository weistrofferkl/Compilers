#lang racket
(require "Project2.rkt"
         "Project3Env.rkt"
         "errors.rkt"
         "log.rkt"
         (prefix-in types: "Project3Types.rkt"))
(require test-engine/racket-tests)
(provide (all-defined-out))

(define (tc-file filename)
  (tc-str (file->string filename)))

(define (init-typeEnv)
  (let ([env (empty-env)])
    (extend-env env 'int (types:make-IntType))
    (extend-env env 'string (types:make-StringType))
    (extend-env env 'bool (types:make-BoolType))
    (extend-env env 'peng (types:make-PengType))
    (extend-env env 'stringCompare (types:FunValue 'stringCompare (list (types:make-StringType) (types:make-StringType)) (types:make-StringType)))
    (extend-env env 'print (types:FunValue 'print (list (types:NameTypePair (types:make-StringType) 's )) (types:make-VoidType)))
    (extend-env env 'printi (types:FunValue 'printi (list (types:NameTypePair (types:make-IntType) 'x))(types:make-VoidType)))
    (extend-env env 'flush (types:FunValue 'flush '() (types:make-VoidType)))
    (extend-env env 'getChar (types:FunValue 'getChar '() (types:make-StringType)))
    (extend-env env 'ord (types:FunValue 'ord (list (types:NameTypePair (types:make-StringType) 's )) (types:make-IntType)))
    (extend-env env 'chr (types:FunValue 'chr (list (types:NameTypePair (types:make-IntType) 's )) (types:make-StringType)))
    (extend-env env 'size (types:FunValue 'size (list (types:NameTypePair (types:make-StringType) 's )) (types:make-IntType)))
    (extend-env env 'substring (types:FunValue 'substring (list (types:NameTypePair (types:make-StringType) 's ) (types:NameTypePair (types:make-IntType) 'first)
                                                    (types:NameTypePair (types:make-IntType) 'n)) (types:make-StringType)))
    (extend-env env 'concat (types:FunValue 'concat (list(types:NameTypePair (types:make-StringType) 's1) (types:NameTypePair (types:make-StringType) 's2))
                                            (types:make-StringType)))
    (extend-env env 'not (types:FunValue 'not (types:NameTypePair (types:make-BoolType) 'i) (types:make-IntType)))
    (extend-env env 'exit (types:FunValue 'exit (types:NameTypePair (types:make-IntType) 'i) (types:make-IntType)))
    env))
    
(define (tc-str str)
  (let ([env (empty-env)])
    (extend-env env 'int (types:make-IntType))
    (extend-env env 'string (types:make-StringType))
    (extend-env env 'bool (types:make-BoolType))
    (extend-env env 'peng (types:make-PengType))
    (extend-env env 'print (types:FunValue 'print (list (types:NameTypePair (types:make-StringType) 's )) (types:make-VoidType)))
    (extend-env env 'printi (types:FunValue 'printi (list (types:NameTypePair (types:make-IntType) 'x))(types:make-VoidType)))
    (extend-env env 'flush (types:FunValue 'flush '() (types:make-VoidType)))
    (extend-env env 'getChar (types:FunValue 'getChar '() (types:make-StringType)))
    (extend-env env 'ord (types:FunValue 'ord (list (types:NameTypePair (types:make-StringType) 's )) (types:make-IntType)))
    (extend-env env 'chr (types:FunValue 'chr (list (types:NameTypePair (types:make-IntType) 's )) (types:make-StringType)))
    (extend-env env 'size (types:FunValue 'size (list (types:NameTypePair (types:make-StringType) 's )) (types:make-IntType)))
    (extend-env env 'substring (types:FunValue 'substring (list (types:NameTypePair (types:make-StringType) 's ) (types:NameTypePair (types:make-IntType) 'first)
                                                    (types:NameTypePair (types:make-IntType) 'n)) (types:make-StringType)))
    (extend-env env 'concat (types:FunValue 'concat (list(types:NameTypePair (types:make-StringType) 's1) (types:NameTypePair (types:make-StringType) 's2))
                                            (types:make-StringType)))
    (extend-env env 'not (types:FunValue 'not (types:NameTypePair (types:make-BoolType) 'i) (types:make-IntType)))
    (extend-env env 'exit (types:FunValue 'exit (types:NameTypePair (types:make-IntType) 'i) (types:make-IntType)))
    
  
    
   
    (typeCheck (first (parse-str str)) env #f 0)))


(define (nameFields fields env level)

  (map (lambda (field)
         (let ([NTPair 
         (types:NameTypePair                
          (apply-env env (string->symbol (TypeField-kind field)))
          (string->symbol (TypeField-name field)))])
         ;  (types:set-NameTypePair-result! NTPair (types:VarValue (apply-env env (string->symbol (TypeField-kind field))) #f level #f #f))
           NTPair)) fields)
       )

(define (findit item lst)
  (cond
    [(null? lst) #f]
    [(equal? item (types:NameTypePair-name (first lst))) (first lst)]
    [else (findit item (rest lst))]))


(define (recordSearch assignments env recTy recTyFields inLoop level)

  (cond
    [(and(null? assignments) (null? recTyFields)) recTy]
    [(not(equal? (length assignments) (length recTyFields))) #f]
    ; want to ask if the first name in the FieldAssign matches the first name in the NameTypePair, AND
    ; if the typechecked expression in the first FieldAssign matches the type of the first NameTypePair
    [(and(equal?(string->symbol(FieldAssign-name(first assignments)))(types:NameTypePair-name(first recTyFields)))
         (equal? (typeCheck (FieldAssign-expr(first assignments)) env inLoop level) (types:NameTypePair-type (first recTyFields))))

     (recordSearch (rest assignments) env recTy (rest recTyFields) inLoop level)]
    [(and(equal?(string->symbol(FieldAssign-name(first assignments)))(types:NameTypePair-name(first recTyFields)))
         (types:PengType?(typeCheck(FieldAssign-expr(first assignments)) env inLoop level)))

     (recordSearch (rest assignments) env recTy (rest recTyFields) inLoop level)]))


(define (funSearch pars env funTy funTyFields inLoop level)

  (cond
    [(and(null? pars) (null? funTyFields)) (types:FunValue-return-type funTy)]
    [(not(equal? (length pars) (length funTyFields))) (error "Lengths not equal")]
    ; want to ask if the first name in the FieldAssign matches the first name in the NameTypePair, AND
    ; if the typechecked expression in the first FieldAssign matches the type of the first NameTypePair
    [(correctComparison (types:actual-type(typeCheck (first pars) env inLoop level)) (types:actual-type(types:NameTypePair-type (first funTyFields))))
     (begin
       (funSearch (rest pars) env funTy (rest funTyFields) inLoop level))]
    [else
     (begin
       
       (error "Funsearch broke"))]))

;Mutual Recursion: Modified For-Each

(define (modForEach funct decl)
  (if (empty? decl) '()
      (begin
        (funct decl)
        (match decl
          [(NameType name kind next) (modForEach funct (NameType-next decl))]
          [(ArrayType name kind next) (modForEach funct (ArrayType-next decl))]
          [(RecordType name fields next) (modForEach funct (RecordType-next decl))])
        )))

;Mutual Recursion: Function For-Each

(define (funForEach funct decl)
  (if (empty? decl) '()
      (begin
        (funct decl)
        (funForEach funct (FunDecl-next decl))
      
        )))

(define (typeCheckFun decl env inLoop level)
  (let ([nameList '()])
    (funForEach (lambda (decl)
                  ; (set! nameList (cons (string->symbol name) nameList))
                  ;create a list of NameType Pairs from the fieldType list of each FunDecl
                  ;make sure the types of those parameters are valid (apply-env)
                  ;make sure rettype is valid
                  ;add FunValue to current Env.
                  (match decl
                    [(FunDecl name args rettype body next)
                   
                     (set! nameList (cons (string->symbol name) nameList))                
                     (let* ([nameList (nameFields args env level)]
                            
                           
                           [rTy  (if (equal? rettype #f) (types:make-VoidType)
                                     (begin (apply-env env (string->symbol rettype))))]
                           [FuncVal (types:FunValue name nameList rTy)])
                     
                         (printf "~n NAMEFIELDS ~a" nameList)
                       (printf "~n rTy ~a" rTy)
                       (printf "~n FuncVal ~a" FuncVal)
                       
                        (add-note decl 'FunVal FuncVal)
                       
                       (extend-env env (string->symbol name) FuncVal)

                       )]))decl)


    (cond
      [(equal? (nameUnique nameList) #t)]
      [else (error "NOT A UNIQUE NAME")]))

  
  (funForEach (lambda (decl)
                (match decl
                  [(FunDecl name args rettype body next)
                   ;Push a new Scope
                   ;When eval a function, look it up in the env and extend that env with the parameters of the funct
                   ;typecheck the body and make sure the body return type matches the declared return type
                   (let* ([newScopeEnv (push-scope env)]
                          ; grab the fun val here
                          [funTy (apply-env env (string->symbol name))]
                          [level (add1 level)]
                          [rTy  (if (eq? #f rettype) (types:make-VoidType) (apply-env env (string->symbol rettype)))])

                     ; extend the environment with the varvalues associated with each parameter
                     (for-each (lambda (arg)
                                (let* ([argName (types:NameTypePair-name arg)]
                                       
                                       [argKind (types:NameTypePair-type arg)])
                                      
                                      
                                  
                                  (extend-env newScopeEnv argName (types:VarValue argKind #f level #f #f))

                                  )) (types:FunValue-parameters funTy))

                     (if (equal? (typeCheck body newScopeEnv inLoop level) rTy) rTy (error "Not equal to return type"))
                  
                     )]))decl))

(define (nameUnique nameList)
  (if (empty? nameList) #t
      (begin
        (if (equal? (member (first nameList) (rest nameList)) #f) (nameUnique (rest nameList)) #f))))
        
             
(define (typeCheckTD decl env name level)
  (let ([ARFlag #f]
        [nameList '()]
        [multiF #f])
    (modForEach (lambda (type)

                  (match type

                    [(NameType name kind next)
                     (begin
                       (cond [(not(empty? next)) (set! multiF #t)])
                       (set! nameList (cons (string->symbol name) nameList))
                       (extend-env env (string->symbol name) (types:make-NameType '() (string->symbol name))))]
                    [(ArrayType name kind next)  (set! nameList (cons (string->symbol name) nameList))(extend-env env (string->symbol name) (types:make-NameType '() (string->symbol name)))]
                    [(RecordType name fields next)  (set! nameList (cons (string->symbol name) nameList))(extend-env env (string->symbol name) (types:make-NameType '()(string->symbol name)))]))
                decl)

    (cond
      [(equal? (nameUnique nameList) #t)]
      [else (error "NOT A UNIQUE NAME")])
  
       
    (modForEach (lambda (type)
                  (match type
                  
                    [(RecordType name fields next)
                     (let ([recTy (apply-env env (string->symbol name))])
                       (types:set-NiType-actual! recTy (makeRecType name fields next env level))
                       (set! ARFlag #t))]

                    [(ArrayType name kind next)
                     (let ([arrTy (apply-env env (string->symbol name))])
                       (types:set-NiType-actual! arrTy (makeArrType name kind next env))
                       (set! ARFlag #t))]

                    [(NameType name kind next)
                     (let ([nameTy (apply-env env (string->symbol name))])
                       (types:set-NiType-actual! nameTy (makeNType name kind next env)))])
                  )decl)
    (if (and (equal? ARFlag #f) (equal? multiF #t)) (error "ARFLAG ERROR") #t)))

              
(define (makeRecType name fields next env level)
  (let ([nameSym (string->symbol name)])
     (printf "nameSym: ~a~n" (nameFields fields env level))
    (types:make-RecordType nameSym (nameFields fields env level))))

(define(makeArrType name kind next env)
  (let ([nameSym (string->symbol name)])
    (types:make-ArrayType name (apply-env env (string->symbol kind)))))

(define(makeNType name kind next env)
  (let ([nameSym (string->symbol name)])
    (types:actual-type(apply-env env (string->symbol kind)))))

(define(correctComparison type1 type2)
  ; (printf "Type1: ~a~n" type1)
  ;(printf "Type2: ~a~n" type2)
  (if (not (and (types:NiType? type1) (types:NiType? type2)))
      (raise-arguments-error 'correctComparison "wrong types for correctComparison, must be NiType"
                             "type1" type1 "type2" type2)
      (cond
        [(and(types:StringType? type1) (types:StringType? type2)) #t]
        [(and(types:IntType? type1) (types:IntType? type2)) #t]
        [(and(types:BoolType? type1) (types:BoolType? type2)) #t]
        [(and(types:PengType? type1) (types:PengType? type2)) #f]
        [(and(types:ArrayType? type1) (types:ArrayType? type2)) (eq? type1 type2)]
        [(and(types:RecordType? type1) (types:RecordType? type2)) (eq? type1 type2)]
        [else #f]
        )))
     
  


;Done: NumExpr, StringExpr, VarExpr, MathExpr, NoVal, VarDecl, LetExpr, NameType, RecordType, ArrayType,
;BoolExpr, LogicExpr, AssignmentExpr, IfExpr, BreakExpr, WhileExpr,WithExpr, RecordExpr (dot notation),
;NewRecordExpr,FieldAssign, FunDecl, FunCallExpr, ArrayExpr (bracket access), NewArrayExpr, Mutual Recursion
;Break only in With/While, With (Read-only var), PengExpr(null) 


;FILES: test02
;Error Test: test40

(define (typecheck-ast ast)
  (let ([typeEnv (init-typeEnv)])
    ; don't clear errors in case we forgot to for some reason...
    (if (error-generated?)
        (cond
          [(eq? (scan-error) #t) (error "cannot continue after scanning errors")]
          [(eq? (parse-error) #t) (error "cannot continue after parsing errors")]
          [else (error "compiler error! help!")])
        (let ([ty (typeCheck ast typeEnv #f 0)])
          (cond
            [(eq? (scan-error) #t) (error "cannot continue after scanning errors")]
            [(eq? (parse-error) #t) (error "cannot continue after parsing errors")]
            [(eq? (type-error) #t) (error "cannot continue after type errors")])
          ty))))
  
;Recursive

(define (typeCheck ast env inLoop level)
  (let ([typeNote
         
         (match ast
           ;Empty List --> VoidType
           ['() (types:make-VoidType)]
           ;Break Expression
           [(BreakExpr) (if (equal? inLoop #t) (types:make-VoidType) (error "Breaking outside of a loop!"))]
           [(PengExpr) (let ([ty (types:make-PengType)])
                         (add-note ast 'type ty)
                         ty)]
           ;List with one element, List with 1+ element
           [(list expr) (typeCheck expr env inLoop level)] 
           [(cons e1 e2)(begin
                          (typeCheck e1 env inLoop level)
                          (typeCheck e2 env inLoop level))]

    
           ;NameType
     
           [(NameType name kind next) (typeCheckTD ast env name level)]

           ;RecordType
           
           [(RecordType name fields next) (typeCheckTD ast env name level)]
                 
           ;ArrayType
          
           [(ArrayType name kind next) (typeCheckTD ast env name level)]

           ;Numbers
           [(NumExpr val) (let ([ty (types:make-IntType)])
                            (add-note ast 'type ty)
                            ty)]

           [(BoolVal v) (types:make-BoolType)]
           ;No Value
           [(NoVal) (let ([ty (types:make-VoidType)])
                      (add-note ast 'type ty)
                      ty)]
           ;Strings
           [(StringExpr str) (let ([ty (types:make-StringType)])
                               (add-note ast 'type ty)
                               ty)]
           ;Array Expression (BracketAccess):
           [(ArrayExpr name expr) (let* ([nameAr (typeCheck name env inLoop level)] 
                                         [arField (typeCheck expr env inLoop level)]
                                         [arType (types:ArrayType-element-type (types:actual-type nameAr))])
                                    
                                    (cond
                                      [(and (types:ArrayType?  (types:actual-type nameAr)) (types:IntType? arField)) arType]
                                      [else (error "Not a valid Array Access")]))]
                           

           ;Record Expressions (Dot Notation):
           ;Check if Record name is a record
           ;Check if field is declared as legit in that record --> return that field's return type
    
           [(RecordExpr name field) (let* ([nameRec (typeCheck name env inLoop level)]
                                 
                                           [recField (findit (string->symbol field) (types:RecordType-fields (types:actual-type nameRec)))])
                                      
                                      (cond
                                        [(and(types:RecordType? (types:actual-type nameRec)) (not(equal? recField #f))) (types:NameTypePair-type recField)]
                                        [else (error "Not a valid Record")]))]
                             
                               
           ;Variable Expression:
           [(VarExpr name) (let ([t1 (apply-env env(string->symbol name))])
                             (if (< (types:VarValue-level t1) level) (types:set-VarValue-escape?! t1 #t) (types:set-VarValue-escape?! t1 #f))
                             (printf "VarExpr lookup is: ~a~n" t1)
                             (printf "NAME~a~n" name)
                             (cond
                               
                               [(equal? name "true") (types:make-BoolType)]
                               [(equal? name "false") (types:make-BoolType)]
                               [(equal? t1 #f) (error "Type Error")]
                               [else
                                (begin
                                  (add-note ast 'varval t1)
                                  (types:VarValue-type t1))
                                ]))]

           ;Math Expressions:
           [(MathExpr e1 op e2) (let ([t1 (typeCheck e1 env inLoop level)]
                                      [t2 (typeCheck e2 env inLoop level)])
                                  (printf "~n~ne1 and e2 ~a~a~n" t1 t2)
                                  (if (and (types:IntType? t1) (types:IntType? t2))
                                      (begin
                                        (add-note ast 'type (types:IntType '()))
                                        (types:make-IntType))
                                      (error "Type Mismatch in MathExpression")))
                         
                                ]

           ;Boolean Expressions:
           [(BoolExpr e1 op e2) (let ([t1 (typeCheck e1 env inLoop level)]
                                      [t2 (typeCheck e2 env inLoop level)])
                                  
                                  (cond
                                    [(and (and  (types:BoolType? t1) (types:BoolType? t2)) (or (eq? op 'ne) (eq? op 'eq))) (types:make-BoolType)]
                                    [(and (types:StringType? t1) (types:StringType? t2))(types:make-BoolType)]
                                    [(and (types:IntType? t1) (types:IntType? t2)) (types:make-BoolType)]
                                    [(and(and (types:RecordType? t1) (types:RecordType? t2)) (or (eq? op 'ne) (eq? op 'eq))) (types:make-BoolType)]
                                    [(and(and (or (types:RecordType? t1) (types:PengType? t1)) (types:RecordType? t2)) (or (eq? op 'ne) (eq? op 'eq))) (types:make-BoolType)]
                                    [(and(and (types:RecordType? t1) (or (types:RecordType? t2) (types:PengType? t2))) (or (eq? op 'ne) (eq? op 'eq))) (types:make-BoolType)]
                             
                                    [(and(and (types:ArrayType? t1) (types:ArrayType t2)) (or (eq? op 'ne) (eq? op 'eq))) (types:make-BoolType)]
                             
                                    [else (error "TYPEEEEEE issue in le Bools")]))]

           ;Logic Expressions:
           [(LogicExpr e1 op e2) (let ([t1 (typeCheck e1 env inLoop level)]
                                       [t2 (typeCheck e2 env inLoop level)])
                                   (cond
                                     [(and (types:BoolType? t1) (types:BoolType? t2)) (types:make-BoolType)] 
                                     [else (error "Logic Type not Compatable " ast)]))]

           ;Assignment Expression:
           [(AssignmentExpr name expr)
           
            (let ([t1 (types:actual-type(typeCheck name env inLoop level))] 
                  [t2 (types:actual-type(typeCheck expr env inLoop level))]
                  )
           
              (match name
                [(VarExpr name)(if (eq?(types:VarValue-read-only? (apply-env env (string->symbol name))) #t) (error "ASSERROR")
                                   (begin
                                    (let ([t3 (types:VarValue (apply-env env (string->symbol name)) #t level #f #f)])
                                       (add-note ast 'varval  t3))
                                       
                                     (cond
                                       [(correctComparison t1 t2) (types:make-VoidType)]
                                       [(types:PengType? t2) (types:make-VoidType)]
                                       [else (error "ASSEXPR SUCKS")])))]
                [_
       
                
                 (cond
         
                   [(correctComparison t1 t2)
                 
                    (types:make-VoidType)
          
                    ]
                   [else (error "AssignmentExpression Suckssssss")])]))]

           ;New Record Expressions
           [(NewRecordExpr name assignments) (let ([recName (apply-env env (string->symbol name))])
                                              
                                               (recordSearch assignments env recName (types:RecordType-fields (types:actual-type recName)) inLoop level))]

           ;New Array Expression:
           ;type must be declared as an array type
           ;if so, extend env. with that array with "expr" elements, and have that kind assigned to each element 
           [(NewArrayExpr name expr kind) (let ([arrName (apply-env env (string->symbol name))]
                                                [arrExp (typeCheck expr env inLoop level)]
                                                [arrKind (apply-env env kind)])
                                     
                                          
                                            (cond
                                              
                                              [(and (and (types:ArrayType? (types:actual-type arrName))(types:IntType? arrExp))
                                                    (equal?(types:ArrayType-element-type (types:actual-type arrName))(typeCheck kind env inLoop level))) arrName]
                                              [else (error "Not arrType")]))]

   
           ;(printf "~n~a"(extend-env env arrName (types:make-VoidType)))
           ;Variable Declarations
           [(VarDecl type id expr) (let ([t1 (types:actual-type(typeCheck expr env inLoop level))])
                                     

                                     
                                     (cond
                                
                                       [(and (eq? type #f) (not (types:PengType? t1))) (let
                                                                                           ([vartype (types:VarValue t1 #f level #f #f)])
                                                                                         (add-note ast 'varvalue vartype)
                                                                                         (extend-env env (string->symbol id) vartype) 
                                                                                         vartype)]
                                
                                       [(and (eq? type #f) (types:PengType? t1)) (error "Assigning Peng Wrong")]
                                      
                                
                                       [(and (not (eq? type #f))(types:PengType? t1))
                                        (let
                                            ([vartypeEx (types:VarValue (types:actual-type (apply-env env (string->symbol type))) #f level #f #f)])
                                          (add-note ast 'varvalue vartypeEx)
                                          
                                          (extend-env env (string->symbol id) vartypeEx)
                                          vartypeEx)]
                                
                          ;(types:VarValue t1 #f level #f #f)
                                       [(correctComparison (types:actual-type (apply-env env (string->symbol type))) t1)
                                        (let ([vv (types:VarValue t1 #f level #f #f)])
                                          (add-note ast 'varvalue vv)
                                          (extend-env env (string->symbol id) vv)
                                          vv)]
                                       [(and (types:RecordType? (apply-env env (string->symbol type)))  (types:PengType? t1)) (types:make-VoidType)]
                                
                                       [else ( (types:actual-type (apply-env env (string->symbol type)))(error "ERMERGRD"))]))]

           ;Function Declaration:  (enterNewScope args rettype body env)
           ;Collect name, parameters, store them in environment (FunValue)
           ;Must enter new scope, add bindings to parameters (extend environment)
           ;typecheck body, return final type of expression, compare this against function definition
           ;(typeCheckFun decl env)
           [(FunDecl name args rettype body next) (typeCheckFun ast env #f level)]

           ;Let Expressions
           [(LetExpr decls exprs) (let ([env1 (push-scope env)])
                                   
                                    (typeCheck decls env1 inLoop level)


                                    (typeCheck exprs env1 inLoop level))

                                  ]

           ;If Expressions
           [(IfExpr testExpr true-branch false-branch) (let ([t1 (typeCheck testExpr env inLoop level)]
                                                             [t2 (typeCheck true-branch env inLoop level)]
                                                             [t3 (typeCheck false-branch env inLoop level)])
                                                         (cond
                                                           [(and (types:BoolType? t1) (equal? t2 t3)) t2]
                                                           [else (error "If Thing Errorssss")]))]

           ;While Expression
           [(WhileExpr test body) (let ([testExpr (typeCheck test env inLoop level)]
                                        [bodyExpr (typeCheck body env #t level)])
                                    (cond
                                      [(and (types:BoolType? testExpr) (types:VoidType? bodyExpr)) (types:make-VoidType)]
                                      [else (error "While Expression error")]))]
           ;With Expression:
           ;expr1 must be < expr2
           ;expr3 must not produce a value
           [(WithExpr name init from to) (let ([expr1 (typeCheck from env inLoop level)]
                                               [expr2 (typeCheck to env inLoop level)])
                                        
                                          
                                           (cond
                                             [(and (types:IntType? expr1) (types:IntType? expr2))(let
                                                                                                     ([vartype (types:VarValue (types:make-IntType) #t level #f #f)])
                                                                                                   
                                                                                                   (extend-env env (string->symbol name) vartype)
                                                                                                   (add-note ast 'varvalue vartype))]

                                                                                                           
                                             [else (error "WithExpression Error from and to must be ints")]))
                                         (let ([expr3 (typeCheck init env #t level)])
                                           (if (types:VoidType? expr3) (types:make-VoidType) (error "Expr3 broken")))]

           ;FunCall Expression:
           ;If ID indicates a function w/o return val then it must not return a val
           ;(recordSearch assignments env recName (types:RecordType-fields recName)))]
           [(FuncallExpr name args);(begin (printf "env in FunCall : ~a~n" env)
            (let ([id (apply-env env (string->symbol name))]
                  [argList (typeCheck args env inLoop level)])
               ;   (printf "ARGLIST ~a~n" argList)
              (let ([funStore 
                     (funSearch args env id (types:FunValue-parameters id) inLoop level)])
                (printf "~n FUNSTORE ~a " funStore)
                (add-note ast 'FunVal id)
                (add-note ast 'type funStore)
                funStore)

       
              )]
                                 
                                        
                             
                             
                     
           [_(begin
               (display ast)
               (error "Node not implemented yet!"))]
    
           )]) (add-note ast 'type typeNote) typeNote))

; noval
(check-expect (tc-str "()") (types:make-VoidType))

; first, simple integer expressions of various sorts
(check-expect (tc-str "5") (types:make-IntType))
(check-expect (tc-str "5+3") (types:make-IntType))
(check-expect (tc-str "5<3") (types:make-BoolType))
(check-expect (tc-str "5>3") (types:make-BoolType))
(check-expect (tc-str "5<=3") (types:make-BoolType))
(check-expect (tc-str "5>=3") (types:make-BoolType))
(check-expect (tc-str "5=3") (types:make-BoolType))
(check-expect (tc-str "5<>3") (types:make-BoolType))

; now test strings
(check-expect (tc-str "\"hello\"") (types:make-StringType))
(check-expect (tc-str "\"hi\"<\"hello\"") (types:make-BoolType))
(check-expect (tc-str "\"hi\">\"hello\"") (types:make-BoolType))
(check-expect (tc-str "\"hi\"<=\"hello\"") (types:make-BoolType))
(check-expect (tc-str "\"hi\">=\"hello\"") (types:make-BoolType))
(check-expect (tc-str "\"hi\"<>\"hello\"") (types:make-BoolType))
(check-expect (tc-str "\"hi\"=\"hello\"") (types:make-BoolType))

(check-error (tc-str "5 = \"5\""))
(check-error (tc-str "5 >= \"5\""))

;BoolTypes
(check-expect (tc-str "true") (types:make-BoolType))
(check-expect (tc-str "false") (types:make-BoolType))
(check-expect (tc-str "true & true") (types:make-BoolType))
(check-expect (tc-str "true | true & false") (types:make-BoolType))

(check-error (tc-str "true & 1"))
(check-expect (tc-str "true & (5+3 < 6)") (types:make-BoolType))

; simple sequence expressions
(check-expect (tc-str "(1; 2; 3; 4)") (types:make-IntType))
; simple let expressions
(check-expect (tc-str "let ni x is 5 in end") (types:make-VoidType))
; more complex, x should be stored and its type retreived 
(check-expect (tc-str "let ni x is 5 in x end") (types:make-IntType))
(check-expect (tc-str "let define itype kind as int
ni itype x is 5 in x end") (types:make-IntType))

;Peng
(check-expect (tc-str "peng") (types:make-PengType))

; simple records
(check-expect (tc-str
               "let
  define empty kind as {}
  ni e is empty {}
in end") (types:make-VoidType))

; record starting off null
(check-expect (tc-str
               "let
  define empty kind as {}
  ni empty e is peng
in
end") (types:make-VoidType))
; record with 1 field
(check-expect (tc-str "
let
  define dot kind as { int x }
  ni dot p is dot { x is 5 }
in end") (types:make-VoidType))
; record with 2 fields
(check-expect (tc-str "
let
  define dot kind as { int x }
  ni dot p is dot { x is 5 }
in p.x end") (types:make-IntType))
; record with 2 fields and a field access through dot notation
(check-expect (tc-str "
let
  define point kind as { int x, int y }
  ni point p is point { x is 5, y is 6 }
in p.x end") (types:make-IntType))
; record with 2 fields, but accessing the 2nd kind
(check-expect (tc-str "
let
  define point kind as { int x, string y }
  ni point p is point { x is 5, y is \"hello\" }
in p.y end") (types:make-StringType))

; record with 2 fields, but accessing the 2nd kind incorrectly
(check-error (tc-str "
let
  define point kind as { int x, string y }
  ni point p is point { x is 5, y is \"hello\" }
in p.y + 5 end"))

; record expression (dot notation) in a math expression
(check-expect (tc-str "
let
  define point kind as { int x, string y }
  ni point p is point { x is 5, y is \"hello\" }
in p.x + 5 end") (types:make-IntType))

; records in records
(check-expect (tc-str "
let
  define color kind as { int r, int g, int b }
  define point kind as { int x, int y, int z }
  define dot kind as { point p, color c }
  ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is color { r is 1, g is 1, b is 1 } }
in d.p.x + 5 end") (types:make-IntType))

;;;; the following are a bit tricky, they should generally fail because you can't return
; a type that was declared in the same scope as the let (aliases can be an exception if
; they're pointing to a type in a higher scope 
;-- you can comment out until the array section to test these later
;;;;

;(check-error (tc-str "
;let
;  define e kind as { int x }
;  ni e x is e { x is 7 }
;in
;  x
;end"))
;(check-error (tc-str "
;let
;  define color kind as { int r, int g, int b }
;  define point kind as { int x, int y, int z }
;  define dot kind as { point p, color c }
;  ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is color { r is 1, g is 1, b is 1 } }
;in d.p end"))
(check-expect (tc-str "
let
  define color kind as { int r, int g, int b }
  ni color col is 
    let
      define point kind as { int x, int y, int z }
    in
      let
         define dot kind as { point p, color c }
         ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is color { r is 1, g is 1, b is 1 } }
      in
        d.c
      end
    end
in
  col.g
end") (types:make-IntType))

;(check-error (tc-str "
;let
;  define color kind as { int r, int g, int b }
;in
;  let
;    define point kind as { int x, int y, int z }
;  in
;    let
;       define dot kind as { point p, color c }
;       ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is color { r is 1, g is 1, b is 1 } }
;    in
;      d.c
;    end
;  end
;end"))

(check-expect (tc-str "
let
  define color kind as { int r, int g, int b }
  ni color col is 
    let
      define point kind as { int x, int y, int z }
    in
      let
         define mycol kind as color
         define dot kind as { point p, mycol c }
         ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is mycol { r is 1, g is 1, b is 1 } }
      in
        d.c
      end
    end
in
  col.g
end") (types:make-IntType))

; arrays -- begin with simple type delcarations, to make sure these are implemented
(check-expect (tc-str "
let
	define arrtype kind as array of int
in
end") (types:make-VoidType))
; now a assignment with an array type
(check-expect (tc-str "
let
  define arrtype kind as array of int
  ni a is arrtype[5] of 0
in
end") (types:make-VoidType))
; and one where we specify the type
(check-expect (tc-str "
let
  define arrtype kind as array of int
  ni arrtype a is arrtype[5] of 0
in
end") (types:make-VoidType))

; and subscript access
(check-expect (tc-str "
let
  define arrtype kind as array of int
  ni arrtype a is arrtype[5] of 0
in
  a[1]
end") (types:make-IntType))

; branches
(check-expect (tc-str "if true then true else true end") (types:make-BoolType))
(check-error (tc-str "if true then true end"))
(check-expect (tc-str "if true then () end") (types:make-VoidType))

; assignments, begin with simple var assign
(check-expect (tc-str "let ni x is 5 in now x is 6 end") (types:make-VoidType))
; bad assignment
(check-error (tc-str "let ni x is 5 in now x is \"hi\" end"))
; record expression assign
(check-expect (tc-str "let define p kind as { int v } ni p x is p { v is 5 } in now x.v is 6 end") (types:make-VoidType))
; array assign
(check-expect (tc-str "let define i kind as array of int ni iarr is i[10] of 0 in now iarr[2] is 12 end") (types:make-VoidType))
; assign peng
(check-expect (tc-str "let define no kind as { } ni x is no {} in now x is peng end") (types:make-VoidType))
; record assign
(check-expect (tc-str "let define p kind as { int v } ni p x is p { v is 5 } in now x is p { v is 7 } end") (types:make-VoidType))

; while loops
(check-expect (tc-str "
let
  ni i is 0
in
  while (i < 10) do
    now i is i + 1
  end
end") (types:make-VoidType))
; while bodies must return no value
(check-error (tc-str "
let
  ni i is 0
in
  while (i < 10) do
    (now i is i + 1; 5)
  end
end"))

; with loops, simple first
(check-expect (tc-str "with i as 0 to 10 do () end") (types:make-VoidType))
; with loop bodies can't return values
(check-error (tc-str "with i as 0 to 10 do 5 end"))
; with loop cannot assign to declared variable from loop
(check-error (tc-str "with i as 0 to 10 do now i is 0 end"))



; functions
(check-expect (tc-str "
let
  neewom nothing() is ()
in
end") (types:make-VoidType))

; simple function call
(check-expect (tc-str "
let
  neewom nothing() is ()
in
  nothing()
end") (types:make-VoidType))


; sort of mutually recursive, at least the logic should execute
(check-expect (tc-str "
let
  neewom no() as bool is false and
  neewom yes() as bool is true
 in
end") (types:make-VoidType))

; mutually recursive
(check-expect (tc-str "
let
  neewom odd(int x) as bool is
    if (2 * (x / 2)) = x then false else true end 
  and neewom even(int x) as bool is
    if (odd(x)) then false else true end
in
  odd(5)
end") (types:make-BoolType))

; mutually recursive, for real this time
(check-expect (tc-str "
let
   neewom foo() is bar() and 
   neewom bar() is foo()
in
  foo()
end") (types:make-VoidType))

; mutually recursive, for real this time with args
(check-expect (tc-str "
let
   neewom foo(int x) as int is bar(x) and 
   neewom bar(int x) as int is foo(x)
in
  foo(42)
end") (types:make-IntType))

; multiple args
(check-expect (tc-str "
let
  define point kind as { int x, int y }
  neewom make-point(int x, int y) as int is
    let
      ni x is point { x is x, y is y }
    in
      x.y
    end
in
  make-point(1, 2)
end") (types:make-IntType))


;DO MONSTER CHECK-EXPECT AT END HERE!!!!!!

(check-expect (tc-str "
let
  ni N is 9

  define intArray kind as array of int

  ni row is intArray [ N ] of 0
  ni col is intArray [ N ] of 0
  ni diag1 is intArray [ N + N - 1] of 0
  ni diag2 is intArray [ N + N - 1] of 0

  neewom printboard () is 
    (with i as 0 to N - 1 do
      (with j as 0 to N - 1 do
        print(if col[i] = j then \" 0\" else \" .\" end)
       end;
       print(\"\n\"))
     end;
     print(\"\n\"))

  neewom try (int c) is
    if c = N - 1
    then printboard()
    else with r as 0 to N - 1 do
              if row[r] = 0 & diag1[r + c] = 0 & diag2[r + 7 - c] = 0 
              then (now row[r] is 1;
                    now diag1[r + c] is 1;
                    now diag2[r + 7 - c] is 1;
                    now col[c] is r;
                    try(c + 1);
                    now row[r] is 0;
                    now diag1[r + c] is 0;
                    now diag2[r + 7 - c] is 0)
              end
         end
    end
  in
    try(0)
end
") (types:make-VoidType))

(test)