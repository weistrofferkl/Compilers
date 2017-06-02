#lang racket

(require "array-list.rkt"
         "iloc.rkt"
         "Project2.rkt"
         "array-list.rkt"
         "errors.rkt"
         "Project3TypeCheck.rkt"
         "names.rkt"
         data/gvector)

(provide (all-defined-out))

;;;;; REGISTERS AND OFFSETS ;;;;;;
;;
;; These define static register names (Rarp) and offsets for various fields
;; in our activation record. We use these instead of hard numbers so we can
;; optimize (possibily?) and delay deciding on the actual frame layout until later!
(define Rarp (Result (Temp "Rarp" 0) #f #f))
; holds an offset for the static link
(define static-link-offset (make-frame-offset 'static-link))
; holds an offset for the return address
(define return-addr-offset (make-frame-offset 'return-addr))
; holds an offset for the address of where to store the return value
(define return-value-offset (make-frame-offset 'return-value))
; holds an offset for the address of the previous activation record, this is
; used because we can just store where to pop off a record instead of
; needing to figure out the math for it later
(define previous-arp (make-frame-offset 'previous-arp))

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
           ; translate boolean values, which are integer 1 and 0s
           [_ (error "Translation of " ast " not implemented yet")])])
    result))


; to load a number into a register, since we pretty much need to do that
; with values (well, we could see if it's a math expression or something else
; but that's later with optimization
(define (num->iloc val alist)
  (let ([result (make-temp-result)])
    (array-list-add-item! alist (loadI (string->number val) result #f))
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
               [(eq? op '+) (add res1 res2 result)])])
        (array-list-add-item! alist item)
        result))))
        