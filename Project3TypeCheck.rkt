#lang racket
(require "Project2.rkt"
         "Project3Env.rkt"
         (prefix-in types: "Project3Types.rkt"))

(define (tc-str str)
  (let ([env (empty-env)])
    (extend-env env 'int (types:make-IntType))
    (extend-env env 'string (types:make-StringType))
    (extend-env env 'bool (types:make-BoolType))
    (extend-env env 'peng (types:make-PengType))
    (printf "env: ~a~n" env)
    (typeCheck (first (parse-str str)) env)
(printf "env: ~a~n" env)))

(define (nameFields fields env)
  (map (lambda (field)
       (types:NameTypePair
        (apply-env env (string->symbol (TypeField-kind field)))
        (string->symbol (TypeField-name field))))
         fields))

(define (findit item lst)
  (cond
    [(null? lst) #f]
    [(equal? item (first lst)) (first lst)]
    [else (findit item (rest lst))]))

(define (recordSearch assignments env recTy recTyFields)
  (cond
    [(and(null? assignments) (null? recTyFields)) recTy]
    [(not(equal? (length assignments) (length recTyFields))) #f]
    ; want to ask if the first name in the FieldAssign matches the first name in the NameTypePair, AND
    ; if the typechecked expression in the first FieldAssign matches the type of the first NameTypePair
    [(and(equal?(string->symbol(FieldAssign-name(first assignments)))(types:NameTypePair-name(first recTyFields)))
                 (equal? (typeCheck (FieldAssign-expr(first assignments)) env) (types:NiType-actual (first recTyFields))))

             (recordSearch (rest assignments) env recTy (rest recTyFields))]))
;Done: NumExpr, StringExpr, VarExpr, MathExpr, NoVal, VarDecl, LetExpr, NameType, RecordType, ArrayType, BoolExpr, LogicExpr, AssignmentExpr, IfExpr, BreakExpr, WhileExpr,WithExpr, RecordExpr (dot notation),FieldAssign, NewRecordExpr
;In Progress/NotSure: ArrayExpr (bracket access), FunDecl, FunCallExpr, NewArrayExpr, PengExpr(null)
;To Do: 


;Recursive
(define (typeCheck ast env)
  (match ast
    ;Empty List --> VoidType
    ['() (types:make-VoidType)]
    ;Break Expression
    [(BreakExpr) (types:make-VoidType)]
    [(PengExpr) (types:make-PengType)]
    ;List with one element, List with 1+ element
    [(list expr) (typeCheck expr env)] 
    [(cons e1 e2)(begin
                   (typeCheck e1 env)
                   (typeCheck e2 env))]

    
    ;NameType
    [(NameType name kind next) (let ([nameSym (string->symbol name)])
                                 (extend-env env nameSym (types:actual-type(apply-env env (string->symbol kind)))))]
    ;RecordType
    [(RecordType name fields next) (let ([nameSym (string->symbol name)])
                                        
                                     (extend-env env nameSym (types:make-RecordType (nameFields fields env))))] 
                 
    ;ArrayType
    [(ArrayType name kind next) (let ([nameSym (string->symbol name)])
                                  (extend-env env nameSym (types:make-ArrayType (apply-env env (string->symbol kind)))))]

    ;Numbers
    [(NumExpr val) (types:make-IntType)]
    ;No Value
    [(NoVal) (types:make-VoidType)]
    ;Strings
    [(StringExpr str) (types:make-StringType)]
    ;Array Expression (Creation):
    [(ArrayExpr name expr) (types:make-ArrayType expr)]
    ;Record Expression (Creation):
    [(RecordExpr name field) (types:make-RecordType field)]
    ;Variable Expression:
    [(VarExpr name) (let ([t1 (apply-env env(string->symbol name))])
                      t1)]

    ;Math Expressions:
    [(MathExpr e1 op e2) (let ([t1 (typeCheck e1 env)]
                               [t2 (typeCheck e2 env)])
                           (printf "~n~nt1 and t2 ~a~a~n" t1 t2)
                           (if (and (types:IntType? t1) (types:IntType? t2))
                               (types:make-IntType)
                               (error "Type Mismatch in MathExpression")))]

    ;Boolean Expressions:
    [(BoolExpr e1 op e2) (let ([t1 (typeCheck e1 env)]
                               [t2 (typeCheck e2 env)])
                           (cond
                             [(and (types:IntType? t1) (types:IntType? t2)) (types:make-BoolType)]
                             [(and(and (types:RecordType? t1) (types:RecordType t2)) (or (eq? op '<>) (eq? op '=))) (types:make-BoolType)]
                             [(and(and (types:ArrayType? t1) (types:ArrayType t2)) (or (eq? op '<>) (eq? op '=))) (types:make-BoolType)]
                             [else (error "TYPEEEEEE issue in le Bools")]))]

    ;Logic Expressions:
    [(LogicExpr e1 op e2) (let ([t1 (typeCheck e1 env)]
                                [t2 (typeCheck e2 env)])
                            (cond
                              [(and (types:BoolType? t1) (types:BoolType? t2)) (types:make-BoolType)] 
                              [else (error "Logic Type not Compatable")]))]

    ;Assignment Expression:
    [(AssignmentExpr name expr) (let ([t1 (apply-env env (string->symbol name))]
                                      [t2 (typeCheck expr env)])
                                  (cond
                                    [(equal? t1 t2) (types:make-VoidType)]
                                    [else (error "AssignmentExpression Suckssssss")]))]

    ;FieldAssign:
    ;check if name already exists in env
  ;  [(FieldAssign name expr) (let ([fieldName (apply-env env (string->symbol name))]
   ;                                [exp1 (typeCheck expr env)])
    ;                           (cond
     ;                            [(and(not(check-duplicates fieldName env)) )]
      ;                           [else (error "Fields not assigned correctly")]))]

    [(NewRecordExpr name assignments) (let ([recName (apply-env env (string->symbol name))])
                                      (recordSearch assignments env recName (types:RecordType-fields recName)))]
                                       ;(recordSearch assignments env recTy)
                                       ; [fieldName (FieldAssign-name)]
                                        ;[fieldVal (typeCheck FieldAssign-expr env)]
                                        ;)]
                                        
                                  
                                      

    ;Record Expressions (Dot Notation):
    ;Check if Record name is a record
    ;Check if field is declared as legit in that record --> return that field's return type
    
    [(RecordExpr name field) (let* ([nameRec (typeCheck name env)]
                                 
                                   [recField (findit field (types:RecordType-fields nameRec))])
                               (printf "namerec: ~a~n recField: ~a~n" nameRec recField)
                             (cond
                               [(and(types:RecordType? nameRec) (not(equal? recField #f))) (types:NiType-actual recField)]
                               [else (error "Not a valid Record")]))]
                             
                               
    ;Variable Declaration: 
;    [(VarDecl type id expr) (let ([t1 (typeCheck expr env)])
;                              (cond ;Chris said this was wrong even though we did it in class types:VarValue
;                                [(eq? type #f)(extend-env env (string->symbol id) t1)]
;                                [(equal?(apply-env env (string->symbol type)) t1)
;                                 (extend-env env (string->symbol id) t1)]
;                                
;                                [else (error "Type Mismatch in VarDecl!!!")]))]
    [(VarDecl type id expr) (let ([t1 (typeCheck expr env)])
                           ;   (printf "env: ~a~n" env)
                           ;   (printf "t1 ~a~n" t1)
                              (cond
                                [(and (eq? type #f) (not (types:PengType? t1))) (extend-env env (string->symbol id) (types:VarValue t1))]
                                [(equal? (apply-env env (string->symbol type)) t1)
                                 (begin
                                   (printf "BeforeExt: ~a~n" (apply-env env (string->symbol type)))
                                 (extend-env env (string->symbol id) (types:VarValue t1)))]
                                [(and (types:RecordType? (apply-env env (string->symbol type)))  (types:PengType? t1)) (types:make-VoidType)]
                                
                                [else (error "ERMERGRD")]))]

    ;Function Declaration:
    [(FunDecl name args rettype body next) (let ([bodyType (typeCheck body env)]
                                                 [returnTy (typeCheck rettype env)])
                                             (cond ;Chris said there was something else to do here in regards to the body
                                               [(equal? bodyType returnTy) (extend-env env (types:FunValue (nameFields args env) returnTy))]
                                               [else (error "Body and return type not equal")]))]
    ;can't deal with the function body until 
                                               

    ;Let Expressions
    [(LetExpr decls exprs) (let ([env1 (push-scope env)])
                             (typeCheck decls env1)
                             (printf "env1: ~a~n" env1)
                             (typeCheck exprs env1))]

    ;If Expressions
    [(IfExpr testExpr true-branch false-branch) (let ([t1 (typeCheck testExpr env)]
                                                      [t2 (typeCheck true-branch env)]
                                                      [t3 (typeCheck false-branch env)])
                                                  (cond
                                                    [(and (types:BoolType? t1) (equal? t2 t3)) t2]
                                                    [else (error "If Thing Errorssss")]))]

    ;While Expression
    [(WhileExpr test body) (let ([testExpr (typeCheck test env)]
                                 [bodyExpr (typeCheck body env)])
                             (cond
                               [(and (types:BoolType? testExpr) (types:VoidType? bodyExpr)) (types:make-VoidType)]
                               [else (error "While Expression error")]))]
    ;With Expression:
    ;expr1 must be < expr2
    ;expr3 must not produce a value
    [(WithExpr name init from to) (let ([expr1 (typeCheck from env)]
                                        [expr2 (typeCheck to env)]
                                        [expr3 (typeCheck init env)])
                                    (cond
                                      [(and (and (types:IntType? expr1) (types:IntType? expr2))(types:VoidType expr3)) (types:make-VoidType)]
                                      [else (error "WithExpression Error")]))]

    ;FunCall Expression:
    ;If ID indicates a function w/o return val then it must not return a val
    [(FuncallExpr name args) (let ([id (typeCheck name env)]
                                   [argList (typeCheck args env)])
                               (cond
                                 [(types:VoidType? id) (types:make-VoidType)]
                                 [else (error "Function Call Error")]))]
                                 
                                        
                             
                             
                     
    [_(begin
        (display ast)
     (error "Node not implemented yet!"))]
    
    ))