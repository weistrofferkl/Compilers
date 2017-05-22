#lang racket

(require "ast-walk.rkt"
         "frame.rkt"
         (prefix-in t: "Project3Types.rkt")
         "names.rkt"
         "Project2.rkt"
         "Project3TypeCheck.rkt"
         "errors.rkt")




;;Making the Stack:
(define stack (cons (make-frame (Label "Lmain")) '()))
(define (push thing)
  (set! stack (cons thing stack)))
(define (pop)
  (let ((result (car stack)))
    (set! stack (cdr stack))
    result))
(define (peek)
  (first stack))

(define (build-frames ast)
  ; mode is either 'pre or 'post
  ;when walking frames you have to do 2 things
  ;calculating static link, keep stack of calcuated frames and where the static link of a variable points in realtion to the stack itself
  ;Look at varDecls and varExpressions to figure out where....
  ;pre-call: Build frame
  ;post-call: Pop frame
  ;build stack using a list
  ;when see a varDecl, allocate a variable in that frame


  
  (let ([walker
         (λ (node mode)
           (match node
             [(FunDecl names args rettype body next)
            
                (cond
                  [(eq? mode 'pre)
                   (let* ([name (make-label)]
                          [funval (get-note node 'FunVal)]
                          [thisFrame(Frame name (Frame-name(peek)) #f funval)])
                     (t:set-FunValue-frame! funval thisFrame)
                  
                     (push thisFrame)
                     
                     )
                     
                   
                   ;Build frame
                   ;push onto stack
                   (let* ([funval (get-note node 'FunVal)]
                     [frame (t:FunValue-frame funval)])
                     (output-frame (current-output-port) frame))]
                  [(eq? mode 'post)
                   (pop)
                   ;pop frame from stack)
                   ])]
             [_ '()]))])
    (ast-walk walker ast)))

(define (output-frames str)
  (set! stack (cons (make-frame (Label "Lmain")) '()))
  (clear-errors)
  (reset-names)
  (let ([ast (parse-str str)])
    (typecheck-ast ast)
    (build-frames ast)))
                  