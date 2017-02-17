#lang racket
(require "Project2.rkt"
         "Project3Env.rkt"
         (prefix-in types: "Project3Types.rkt"))

(define (tc-str str)
  (let ([env (empty-env)])
    (typeCheck (first (parse-str str)) env)))


;Recursive
;Done: NumExpr, StringExpr, VarExpr, MathExpr, NoVal, VarDecl, LetExpr,
;To Do: FunDecl, ArrayExpr, RecordExpr, FunCallExpr, BoolExpr, LogicExpr, FieldAssign, NewRecordExpr, NewArrayExpr, IfExpr, WhileExpr, AssignmentExpr, BreakExpr, PengExpr, WithExpr
(define (typeCheck ast env)
  (match ast
    ['() (types:make-VoidType)]
    ;List with one element, List with 1+ element
    [(list expr) (typeCheck expr env)] 
    [(cons e1 e2)(begin
                   (typeCheck e1 env)
                   (typeCheck e2 env))]

    [(NameType name kind next) (let ([nameSym (string->symbol name)])
                                 (extend-env env nameSym (types:actual-type(apply-env env kind))))]

    ;Numbers
    [(NumExpr val) (types:make-IntType)]
    ;No Value
    [(NoVal) (types:make-VoidType)]
    ;Strings
    [(StringExpr str) (types:make-StringType)]
    ;Array Expression:
    [(ArrayExpr name expr) (types:make-ArrayType expr)]
    ;Record Expression:
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
                             
                               
    ;Variable Declaration: 
    [(VarDecl type id expr) (let ([t1 (typeCheck expr env)])
                              (cond
                                [(eq? type #f)(extend-env env (string->symbol id) t1)]
                                [(equal?(apply-env env (string->symbol type)) t1)
                                 (extend-env env (string->symbol id) t1)]
                                [else (error "Type Msmatch!!!")]))]

    ;Let Expressions
    [(LetExpr decls exprs) (let ([env1 (push-scope env)])
                             (typeCheck decls env1)
                             (typeCheck exprs env1))]
                             
                     
    [_ (error "Node not implemented yet!")]
    
    ))