#lang racket

(require "array-list.rkt"
         "iloc.rkt"
         "Project2.rkt"
         "array-list.rkt"
         "errors.rkt"
         "Project3TypeCheck.rkt"
         "names.rkt"
         data/gvector)
;Booleans, Logic

(provide (all-defined-out))
; simple translator that only outputs the stuff that you've added, not all
; the preamble and postamble materials
(define (trans str)
  (clear-errors)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse error")
        (let ([ty (typecheck-ast ast)])
          (if (error-generated?)
              (error "cannot translate due to type error")
              (let ([alist (make-array-list)])
                (ast->iloc! (first ast) alist)
                ; now walk through the alist
                (printf "translation resulted in ~a ILOC instructions:~n"
                        (array-list-length alist))
                (display-array-list alist (current-output-port))
                alist))))))


; actually translates the string and emits the entire 
(define (translate-str str)
  ; clear the errors first
  (clear-errors)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse errors")
        (let ([ty (typecheck-ast ast)])
          (if (error-generated?)
              (error "cannot translate due to type error")
              (begin
                (translate-ast ast)))))))

(define (translate-ast ast)
  ; need an array list to store the iloc in
  (let ([alist (make-array-list)])
    (ast->iloc! (first ast alist))))

(provide ast->iloc!)
; translate a ni AST into an array-list of iloc because I think it will
; be easier to handle given the algorithms in the book (I mean we could
; convert all the iterative versions into recursive versions, but that
; seems like extra work we probably don't need). Note, because of this, we
; have to pass an 'accumulator', which is our current array list of iloc.
; Oh, this also means, we need to do more 'add-note' kind of stuff with
; our iloc structs (like with 'label) and also, we mutate alist (not the ast)
(define (ast->iloc! ast alist)
  (let ([result
         (match ast
           ; deal with lists, like in let expressions
           ['() '()]
           [(list node) (ast->iloc! node alist)]
           [(cons first rest) (ast->iloc! first alist) (ast->iloc! rest alist)]
           
           ; and so we begin again...a numeric literal
           [(NumExpr val) (num->iloc val alist)]

           [(MathExpr _ _ _) (math->iloc ast alist)]

           [(BoolExpr _ _ _) (bool->iloc ast alist)]

           [(LogicExpr _ _ _) (logic->iloc ast alist)]
           ; translate boolean values, which are integer 1 and 0s
           [(BoolVal val) (boolVal->iloc val alist)]
           [_ (error "Translation of " ast " not implemented yet")])])
    result))


; to load a number into a register, since we pretty much need to do that
; with values (well, we could see if it's a math expression or something else
; but that's later with optimization
(define (num->iloc val alist)
  (let ([result (make-temp-result)])
    (array-list-add-item! alist (loadI (string->number val) result #f))
    result))

(define (boolVal->iloc val alist)
  (let ([result (make-temp-result)])
    (cond
      [(equal? val 1) (array-list-add-item! alist (load "true") result #f)]
      [(equal? val 0) (array-list-add-item! alist (load "false") result #f)])
    result))
      


(define (math->iloc ast alist)
  (let ([e1 (MathExpr-expr1 ast)]
        [e2 (MathExpr-expr2 ast)]
        [op (MathExpr-op ast)])
    (let ([res1 (ast->iloc! e1 alist)]
          [res2 (ast->iloc! e2 alist)]
          [result (make-temp-result)])
      (let ([item
             (cond
               [(eq? op '+) (add res1 res2 result)]
               [(eq? op '-) (sub res1 res2 result)]
               [(eq? op '*) (mult res1 res2 result)]
               [(eq? op '/) (div res1 res2 result)])])
        (array-list-add-item! alist item)
        result))))

(define (bool->iloc ast alist)
  (let ([e1 (BoolExpr-expr1 ast)]
        [e2 (BoolExpr-expr2 ast)]
        [op (BoolExpr-op ast)])
    (let ([res1 (ast->iloc! e1 alist)]
           [res2 (ast->iloc! e2 alist)]
           [result (make-temp-result)])
      (let ([item
             (cond
               [(eq? op 'eq) (cmp_EQ res1 res2 result)]
               [(eq? op 'ne) (cmp_NE res1 res2 result)]
               [(eq? op 'lt) (cmp_LT res1 res2 result)]
               [(eq? op 'gt) (cmp_GT res1 res2 result)]
               [(eq? op 'le) (cmp_LE res1 res2 result)]
               [(eq? op 'ge) (cmp_GE res1 res2 result)]
               [else (error "what the heck is op??")])])
        (array-list-add-item! alist item)
        result))))

(define (logic->iloc ast alist)
  (let ([e1 (LogicExpr-expr1 ast)]
        [e2 (LogicExpr-expr2 ast)]
        [op (LogicExpr-op ast)])
    (let ([res1 (ast->iloc! e1 alist)]
          [res2 (ast->iloc! e2 alist)]
          [result (make-temp-result)])
      (let ([item
             (cond
               [(eq? op 'and) (and res1 res2 result)]
               [(eq? op 'or) (or res1 res2 result)])])
        (array-list-add-item! alist item)
        result))))
        
        