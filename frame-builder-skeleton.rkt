#lang racket

(require "ast-walk.rkt"
         "frame.rkt"
         (prefix-in t: "types.rkt")
         "names.rkt"
         "niparser.rkt")

(define (build-frames ast)
  ; mode is either 'pre or 'post
  (let ([walker
         (λ (node mode)
           (match node
             [(FunDecl names args rettype body next)
              (let* ([funval (get-note node 'funval)]
                     [frame (t:FunValue-frame funval)])
                (output-frame (current-output-port) frame))]
             [_ '()]))])
    (ast-walk walker ast)))
                  