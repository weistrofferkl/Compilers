#lang racket

(require test-engine/racket-tests)
(require racket/cmdline)
(require "Project2.rkt")
(provide (all-defined-out))


;TYPES

(struct NiType (actual) #:transparent
  #:guard (λ (actual typename)
            (if (equal? typename 'NiType)
                (error "Can't instantiate NiType Directly")
                (if (or (eq? actual '())
                        (NiType? actual)) (values actual)
                                          (error "Can only instantiate with NiTypes or '()")))))
                

(struct StringType NiType () #:transparent)
(struct VoidType NiType () #:transparent)
(struct IntType NiType () #:transparent)
(struct ArrayType NiType (element-type) #:transparent)
; for records, we need two structs
(struct RecordType NiType (fields) #:transparent)
; in this case, the name is the symbol name of a field, 
; and actual will refer to the actual type
(struct NameTypePair NiType (name) #:transparent)


;VALUES
(struct VarValue (type) #:transparent)
; as with records, we need something for parameters,
; so this will be stored as a list of NameTypePair structs
(struct FunValue (parameters return-type) #:transparent)



;SYMBOLS
; create a new, empty environment
(define (empty-env) (list (make-hash)))

; extend the given environment with a symbol and value associated with it
; and then return the newly modified environment
(define (extend-env env sym val)
  (hash-set! (first env) sym val) env)

; apply the given environment with the symbol and return its value
(define (apply-env env sym)
  (cond ;Add base case
    [(eq? env '()) #f]
    [(hash-has-key? (first env) sym) (hash-ref (first env) sym)]
    [else (apply-env (rest env) sym)]))
    

; push a new scope onto the environment list and return the new environment
(define (push-scope env) (cons (make-hash) env))

; pops a scope from the environment list and return the remaining environment
(define (pop-scope env) (rest env))

(define (make-IntType)
  (IntType '()))
(define (make-StringType)
  (StringType '()))

(define (get-ast in)
  (lambda ()
    (let ([ast (niparser in)])
      (typeCheck (first ast)))))

;Recursive
(define (typeCheck ast env)
  (match ast
    [(NumExpr val) (make-IntType)]
    [(StringExpr str) (make-StringType)]
    [(VarExpr name) (let ([t1 (apply-env env(string->symbol name))])
                      t1)]
    [(MathExpr e1 op e2) (let ([t1 (typeCheck e1 env)]
                               [t2 (typeCheck e2 env)])
                           (if (and (IntType? t1) (IntType? t2))
                               (make-IntType)
                               (error "Type Mismatch")))]
    [(NoVal) (VoidType)]
    [(VarDecl type id expr) (let ([t1 (typeCheck expr env)])
                              (cond
                                [(eq? type #f)(extend-env env (string->symbol id) t1)]
                                [(equal?(apply-env env (string->symbol type)) t1)
                                 (extend-env env (string->symbol id) t1)]
                                [else (error "Type Msmatch!!!")]))]
    [_ (error "Node not implemented yet!")]
    
    ))

(define (tc-str str)
  (let ([env (empty-env)])
    (typeCheck (first (parse-str str)) env)))

; make sure you defined the empty environment properly
(check-expect (empty-env) `(,(hash-copy #hash())))

; simple tests for adding things to the environment with only one level of scope
(check-expect (extend-env (empty-env) 'x 5) `(,(hash-copy #hash((x . 5)))))
(check-expect (extend-env (extend-env (empty-env) 'x 5) 'y 6) `(,(hash-copy #hash((y . 6) (x . 5)))))
(check-expect (extend-env (extend-env (extend-env (empty-env) 'x 5) 'y 6) 'z 7) `(,(hash-copy #hash((z . 7) (y . 6) (x . 5)))))

; tests for checking if something is there
(check-expect (apply-env (extend-env (empty-env) 'x 5) 'x) 5)
(check-expect (apply-env (extend-env (extend-env (empty-env) 'x 5) 'y 6) 'y) 6)
(check-expect (apply-env (extend-env (extend-env (extend-env (empty-env) 'x 5) 'y 6) 'z 7) 'z) 7)

; pushing and popping tests (trival)
(check-expect (push-scope (empty-env)) `(,(make-hash) ,(make-hash)))
(check-expect (pop-scope (push-scope (empty-env))) `(,(make-hash)))

; something more complicated
(check-expect (apply-env (extend-env (push-scope (extend-env (empty-env) 'x 5)) 'y 6) 'y) 6)
(check-expect (apply-env (extend-env (push-scope (extend-env (empty-env) 'x 5)) 'y 6) 'x) 5)
(check-expect (apply-env (pop-scope (extend-env (push-scope (extend-env (empty-env) 'x 5)) 'y 6)) 'x)5)
(test)
