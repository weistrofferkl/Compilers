#lang racket
(require "Project2.rkt"
         "Project3Env.rkt"
         (prefix-in types: "Project3Types.rkt"))
(require test-engine/racket-tests)

(define (tc-str str)
  (let ([env (empty-env)])
    (extend-env env 'int (types:make-IntType))
    (extend-env env 'string (types:make-StringType))
    (extend-env env 'bool (types:make-BoolType))
    (extend-env env 'peng (types:make-PengType))
   ; (printf "env: ~a~n" env)
   (typeCheck (first (parse-str str)) env)))
;(printf "envFinal: ~a~n" (typeCheck (first (parse-str str)) env))))

(define (nameFields fields env)
  (map (lambda (field)
       (types:NameTypePair
        (apply-env env (string->symbol (TypeField-kind field)))
        (string->symbol (TypeField-name field))))
         fields))

(define (findit item lst)
  (cond
    [(null? lst) #f]
    [(equal? item (types:NameTypePair-name (first lst))) (first lst)]
    [else (findit item (rest lst))]))

;;(define (finditAr item arrKind)
;  (cond
;    [(null? lst) #f]
;    [(equal? item (types:NameTypePair-name (first lst))) (first lst)]
;    [else (findit item (rest lst))]))

(define (recordSearch assignments env recTy recTyFields)
  (cond
    [(and(null? assignments) (null? recTyFields)) recTy]
    [(not(equal? (length assignments) (length recTyFields))) #f]
    ; want to ask if the first name in the FieldAssign matches the first name in the NameTypePair, AND
    ; if the typechecked expression in the first FieldAssign matches the type of the first NameTypePair
    [(and(equal?(string->symbol(FieldAssign-name(first assignments)))(types:NameTypePair-name(first recTyFields)))
                 (equal? (typeCheck (FieldAssign-expr(first assignments)) env) (types:NiType-actual (first recTyFields))))

             (recordSearch (rest assignments) env recTy (rest recTyFields))]))

;if so, extend env. with that array with "expr" elements, and have that kind assigned to each element

;(define (arrSearch name expr kind env)
 ; (cond
  ;  [(equal? expr 0) kind]
   ; [(extend-env env 

(define (funSearch pars env funTy funTyFields)
  (cond
    [(and(null? pars) (null? funTyFields)) (types:FunValue-return-type funTy)]
    [(not(equal? (length pars) (length funTyFields))) #f]
    ; want to ask if the first name in the FieldAssign matches the first name in the NameTypePair, AND
    ; if the typechecked expression in the first FieldAssign matches the type of the first NameTypePair
    [(equal? (typeCheck (first pars) env) (types:NiType-actual (first funTyFields)))

             (funSearch (rest pars) env funTy (rest funTyFields))]))

;Used in FunDecls --> Push Scope, extend environment for arguments, return body type
(define (enterNewScope funName args rettype body env)
  (let* ([newEnv (push-scope env)] 
        [rTy  (if (equal? rettype #f) (types:make-VoidType)
                 (begin (apply-env newEnv (string->symbol rettype))))])
     (for-each(lambda (arg)
                (let ([argName (string->symbol(TypeField-name arg))]
                      [argKind (apply-env env (string->symbol (TypeField-kind arg)))])
                 ; (printf "argkind: ~a~n" argKind)
                 ; (printf "argname: ~a~n" argName)
                 ; (printf "arg: ~a~n" arg)
                  
                 (extend-env newEnv argName (types:VarValue argKind)))) args)
    ;(printf "body: ~a~n" (equal? (typeCheck body newEnv) rTy))
    ;(printf "rTy: ~a~n" rTy)
    (cond
      [(equal? (typeCheck body newEnv) rTy)(extend-env env (string->symbol funName) (types:FunValue (nameFields args newEnv) rTy)) rTy]
      [(null? body) (types:make-VoidType)]
      [else (error "FunDecl Error")])))

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

;(define (funForEach funct decl)
;(if (empty? decl) '()
 ;   (begin
  ;    (funct decl)
   ;   [(FunDecl name args rettype body next) (funForEach funct (FunDecl-next decl))]
    ;  )))

;(extend-env env nameSym (types:actual-type(apply-env env (string->symbol kind)))))]
(define (typeCheckTD decl env)
  (modForEach (lambda (type)
                (match type
                  [(NameType name kind next) (extend-env env (string->symbol name) (types:make-NameType '()))]
                  [(ArrayType name kind next) (extend-env env (string->symbol name) (types:make-NameType '()))]
                  [(RecordType name fields next) (extend-env env (string->symbol name) (types:make-NameType '()))])) decl)
  (modForEach (lambda (type)
                (match type
                  [(RecordType name fields next)
                   (let ([recTy (apply-env env (string->symbol name))])
                     (types:set-NiType-actual! recTy (makeRecType name fields next env)))]

                  [(ArrayType name kind next)
                   (let ([arrTy (apply-env env (string->symbol name))])
                     (types:set-NiType-actual! arrTy (makeArrType name kind next env)))]

                  [(NameType name kind next)
                   (let ([nameTy (apply-env env (string->symbol name))])
                     (types:set-NiType-actual! nameTy (makeNType name kind next env)))]))decl))


;(define (tcFun decl env)
;  (funForEach (lambda (type)
;                (match type
;                [(FunDecl name args rettype body next)
;                 (let
;                     ([pList (nameFields args env)]
;                      [rTy
;                       (if (equal? rettype #f) (types:make-VoidType)
;                       (apply-env env rettype))])
;
;                     (extend-env env (string->symbol name) (types:FunVal pList rTy)))])) decl)
; ; (funForEach (lambda (type) ;typecheck bodies, make sure bodies match return type
;                ;
;  ;              (let ([funTy (apply-env env (string->symbol name))])
;   ;               ())))
                 
          
                
              
(define (makeRecType name fields next env)
  (let ([nameSym (string->symbol name)])
    (extend-env env nameSym (types:make-RecordType (nameFields fields env)))))

(define(makeArrType name kind next env)
  (let ([nameSym (string->symbol name)])
    (extend-env env nameSym (types:make-ArrayType (apply-env env (string->symbol kind))))))

(define(makeNType name kind next env)
  (let ([nameSym (string->symbol name)])
    (extend-env env nameSym (types:actual-type(apply-env env (string->symbol kind))))))
  
  
                 
  
       
        

;Done: NumExpr, StringExpr, VarExpr, MathExpr, NoVal, VarDecl, LetExpr, NameType, RecordType, ArrayType,
;BoolExpr, LogicExpr, AssignmentExpr, IfExpr, BreakExpr, WhileExpr,WithExpr, RecordExpr (dot notation),
;NewRecordExpr,FieldAssign, FunDecl, FunCallExpr, ArrayExpr (bracket access), NewArrayExpr,

;In Progress/NotSure:  PengExpr(null),  Mutual Recursion,

;To Do:Break only in With/While

;commented out Check-expects =
;Tricky Error Cases Chris said not to worry about until later
;Crazy Long program at the end


;Mutual Recursion (names/arrays/records/functions)
;types and functions: modified for-each
;walk through all names of types and create name-type for them (NameType '())
;then walk through again, and bind to the nameTypes



;Recursive
(define (typeCheck ast env)
 (printf "typeCheck with ~a~n~n" (object-name ast))
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
    ;[(NameType name kind next) (let ([nameSym (string->symbol name)])
                              ;   (extend-env env nameSym (types:actual-type(apply-env env (string->symbol kind)))))]
    [(NameType name kind next) (typeCheckTD ast env)]

    ;RecordType
   ; [(RecordType name fields next) (let ([nameSym (string->symbol name)])
                                        
                                     ;(extend-env env nameSym (types:make-RecordType (nameFields fields env))))]
    [(RecordType name fields next) (typeCheckTD ast env)]
                 
    ;ArrayType
    ;[(ArrayType name kind next) (let ([nameSym (string->symbol name)])
                                  ;(extend-env env nameSym (types:make-ArrayType (apply-env env (string->symbol kind)))))]
    [(ArrayType name kind next) (typeCheckTD ast env)]

    ;Numbers
    [(NumExpr val) (types:make-IntType)]
    ;No Value
    [(NoVal) (types:make-VoidType)]
    ;Strings
    [(StringExpr str) (types:make-StringType)]
    ;Array Expression (BracketAccess):
    ;(typeCheck expr env)
    [(ArrayExpr name expr) (let* ([nameAr (typeCheck name env)] ;(string->symbol expr)
                                 [arField (typeCheck expr env)]
                                 [arType (types:ArrayType-element-type nameAr)])
                             (printf "BracketName ~a~n" name)
                             (printf "BracketExpr ~a~n" expr)
                             ;(printf "BracketType ~a~n" (types:ArrayType-element-type nameAr))
                             (cond
                               [(and (types:ArrayType? nameAr) (types:IntType? arField)) arType]
                               [else (error "Not a valid Array Access")]))]
                           

    ;Record Expressions (Dot Notation):
    ;Check if Record name is a record
    ;Check if field is declared as legit in that record --> return that field's return type
    
    [(RecordExpr name field) (let* ([nameRec (typeCheck name env)]
                                 
                              [recField (findit (string->symbol field) (types:RecordType-fields nameRec))])
                             ;  (printf "namerec: ~a~n recField: ~a~n" nameRec recField)
                             (cond
                               [(and(types:RecordType? nameRec) (not(equal? recField #f))) (types:NiType-actual recField)]
                               [else (error "Not a valid Record")]))]
                             
                               
    ;Variable Expression:
    [(VarExpr name) (let ([t1 (apply-env env(string->symbol name))])
                      (printf "~n~n~a" 'name)
                      (cond
                        
                        [(equal? name "true") (types:make-BoolType)]
                        [(equal? name "false") (types:make-BoolType)]
                        [else (types:VarValue-type t1)]))]

    ;Math Expressions:
    [(MathExpr e1 op e2) (let ([t1 (typeCheck e1 env)]
                               [t2 (typeCheck e2 env)])
                           ;(printf "~n~ne1 and e2 ~a~a~n" e1 e2)
                           (if (and (types:IntType? t1) (types:IntType? t2))
                               (types:make-IntType)
                               (error "Type Mismatch in MathExpression")))]

    ;Boolean Expressions:
    [(BoolExpr e1 op e2) (let ([t1 (typeCheck e1 env)]
                               [t2 (typeCheck e2 env)])
                            (printf "~n~ne1 and e2 ~a~a~n" e1 e2)
                           (cond
                            
                             [(and (types:StringType? t1) (types:StringType? t2))(types:make-BoolType)]
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
    [(AssignmentExpr name expr)
     ;(printf "name ~a~n" )
     (let ([t1 (typeCheck name env)] ;(apply-env env (string->symbol name))
           [t2 (typeCheck expr env)])
       (printf "name ~a~n" t1)
       (printf "t2 ~a~n" t2)
       (cond
         [(equal? t1 t2) (types:make-VoidType)]
         [else (error "AssignmentExpression Suckssssss")]))]

    ;New Record Expressions
    [(NewRecordExpr name assignments) (let ([recName (apply-env env (string->symbol name))])
                                      (recordSearch assignments env recName (types:RecordType-fields recName)))]

     ;New Array Expression:
    ;type must be declared as an array type
    ;if so, extend env. with that array with "expr" elements, and have that kind assigned to each element 
    [(NewArrayExpr name expr kind) (let ([arrName (apply-env env (string->symbol name))]
                                         [arrExp (typeCheck expr env)]
                                         [arrKind (apply-env env kind)])
                                     
                                     (printf "arrname: ~a~n" arrName)
                                     (printf "expr: ~a~n" arrExp)
                                    (printf "kind: ~a~n" kind) 
                                     (cond
                                       ;[(and(and (types:ArrayType? arrName) (types:IntType? arrExp)) (NumExpr-val kind)) arrName]
                                       [(and (types:ArrayType? arrName)(types:IntType? arrExp)) arrName]
                                       [else (error "Not arrType")]))]

   
    ;(printf "~n~a"(extend-env env arrName (types:make-VoidType)))
    ;Variable Declarations
    [(VarDecl type id expr) (let ([t1 (typeCheck expr env)])
                           ;   (printf "env: ~a~n" env)e
                           ;   (printf "t1 ~a~n" t1)
                              (cond
                                [(and (eq? type #f) (not (types:PengType? t1))) (extend-env env (string->symbol id) (types:VarValue t1))] ;return type?
                                [(equal? (apply-env env (string->symbol type)) t1)
                                 (begin
                               ;    (printf "BeforeExt: ~a~n" (apply-env env (string->symbol type)))
                                 (extend-env env (string->symbol id) (types:VarValue t1)))]
                                [(and (types:RecordType? (apply-env env (string->symbol type)))  (types:PengType? t1)) (types:make-VoidType)]
                                
                                [else (error "ERMERGRD")]))]

    ;Function Declaration:  (enterNewScope args rettype body env)
    ;Collect name, parameters, store them in environment (FunValue)
    ;Must enter new scope, add bindings to parameters (extend environment)
    ;typecheck body, return final type of expression, compare this against function definition
    [(FunDecl name args rettype body next) (enterNewScope name args rettype body env)]

    ;Let Expressions
    [(LetExpr decls exprs) (let ([env1 (push-scope env)])
                             (typeCheck decls env1)
                           ;  (printf "env1: ~a~n" env1)
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
                                      (printf "expr3: ~a~n" expr3)
                                    (cond
                                      [(and (and (types:IntType? expr1) (types:IntType? expr2))(types:VoidType? expr3)) (types:make-VoidType)]
                                      [else (error "WithExpression Error")]))]

    ;FunCall Expression:
    ;If ID indicates a function w/o return val then it must not return a val
    ;(recordSearch assignments env recName (types:RecordType-fields recName)))]
    [(FuncallExpr name args) (let ([id (apply-env env (string->symbol name))]
                                   [argList (typeCheck args env)])
                               (funSearch args env id (types:FunValue-parameters id))  
                               )]
                                 
                                        
                             
                             
                     
    [_(begin
        (display ast)
     (error "Node not implemented yet!"))]
    
    ))

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
;(check-expect (tc-str "
;let
;  define color kind as { int r, int g, int b }
;  ni color col is 
;    let
;      define point kind as { int x, int y, int z }
;    in
;      let
;         define dot kind as { point p, color c }
;         ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is color { r is 1, g is 1, b is 1 } }
;      in
;        d.c
;      end
;    end
;in
;  col.g
;end") (types:make-IntType))

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
;(check-expect (tc-str "let define i kind as array of int ni iarr is i[10] of 0 in now iarr[2] is 12 end") (types:make-VoidType))
; assign peng
;(check-expect (tc-str "let define no kind as { } ni x is no {} in now x is peng end") (types:make-VoidType))
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