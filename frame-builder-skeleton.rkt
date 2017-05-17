#lang racket

(require "ast-walk.rkt"
         "frame.rkt"
         (prefix-in t: "Project3Types.rkt")
         "names.rkt"
         "Project2.rkt")


(define stack '())
(define (push thing)
  (set! stack (cons thing stack)))
(define (pop)
  (let ((result (car stack)))
    (set! stack (cdr stack))
    result))

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
              (let* ([funval (get-note node 'funval)]
                     [frame (t:FunValue-frame funval)])
                (output-frame (current-output-port) frame))]
             [_ '()]))])
    (ast-walk walker ast)))
                  