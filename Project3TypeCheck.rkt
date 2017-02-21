#lang racket
(require "Project2.rkt"
         "Project3Env.rkt"
         (prefix-in types: "Project3Types.rkt"))

(define (tc-str str)
  (let ([env (empty-env)])
    (extend-env env 'int (types:make-IntType))
    (extend-env env 'string (types:make-StringType))
    (extend-env env 'bool (types:make-BoolType))
    (printf "env: ~a~n" env)
    (typeCheck (first (parse-str str)) env)))
    ;(printf "env: ~a~n" env)))

(define (nameFields fields env)
  (map (lambda (field)
       (types:NameTypePair
        (apply-env env (string->symbol (TypeField-kind field)))
        (string->symbol (TypeField-name field))))
         fields))



;Recursive
;Done: NumExpr, StringExpr, VarExpr, MathExpr, NoVal, VarDecl, LetExpr, NameType, RecordType, ArrayType
;In Progress/NotSure: BoolExpr, LogicExpr,RecordExpr (dot notation)???, ArrayExpr (bracket access)???, BreakExpr?????, FunDecl, AssignmentExpr????, IfExpr?
;To Do: FunCallExpr, FieldAssign, NewRecordExpr, NewArrayExpr, WhileExpr, PengExpr, WithExpr
(define (typeCheck ast env)
  (match ast
    ['() (types:make-VoidType)]
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
                           (if (and (types:IntType? t1) (types:IntType? t2))
                               (types:make-IntType)
                               (error "Type Mismatch")))]

    ;Boolean Expressions:
    [(BoolExpr e1 op e2) (let ([t1 (typeCheck e1 env)]
                               [t2 (typeCheck e2 env)])
                           (cond
                             [(and (types:IntType? t1) (types:IntType? t2)) (types:make-BoolType)]
                             [(and(and (types:RecordType? t1) (types:RecordType t2)) (or (eq? op '<>) (eq? op '=))) (types:make-BoolType)]
                             [(and(and (types:ArrayType? t1) (types:ArrayType t2)) (or (eq? op '<>) (eq? op '=))) (types:make-BoolType)]
                             [else (error "TYPEEEEEE")]))]

    ;Logic Expressions:
    [(LogicExpr e1 op e2) (let ([t1 (typeCheck e1 env)]
                                [t2 (typeCheck e2 env)])
                            (cond
                              [(and (types:BoolType? t1) (types:BoolType? t2))] ;(types:make-LogicType) ????
                              [else (error "Logic Type not Compatable")]))]

    ;Assignment Expression:
    [(AssignmentExpr name expr) (let ([t1 (apply-env env (string->symbol name))]
                                      [t2 (typeCheck expr env)])
                                  (cond
                                    [(eq? t1 t2)]
                                    [else (error "AssignmentExpression Suckssssss")]))]
                                  
                                      

    ;Record Expressions (Dot Notation):
    ;[(RecordExpr name field) (let ([nameRec (typeCheck name env)])
     ;                        (cond
      ;                         [(equal? nameRec (apply-env env (RecordExpression-name)))]
       ;                        [else (error "Not a valid Record")]))]
                             
                               
    ;Variable Declaration: 
    [(VarDecl type id expr) (let ([t1 (typeCheck expr env)])
                              (cond
                                [(eq? type #f)(extend-env env (string->symbol id) t1)]
                                [(equal?(apply-env env (string->symbol type)) t1)
                                 (extend-env env (string->symbol id) t1)]
                                
                                [else (error "Type Msmatch!!!")]))]

    ;Function Declaration:
    [(FunDecl name args rettype body next) (let ([nameFunc (typeCheck name env)]
                                                 [returnTy (typeCheck rettype env)])
                                             (cond
                                               [(eq? name #f) (extend-env (string->symbol name) nameFunc)]))]
                                               ;check if return type of Function matches what is actually being returned?

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
                                                    [(types:BoolType? testExpr)]
                                                    [(or (eq? t2 t3) (eq? t3 '()))] ;ask if this must return anything
                                                    [else (error "If Thing Errorssss")]))]
                             
                     
    [_ (error "Node not implemented yet!")]
    
    ))