#lang racket
(require racket/cmdline)
(require "Project2.rkt")



; create a new, empty environment
(define (empty-env) ...)

; extend the given environment with a symbol and value associated with it
; and then return the newly modified environment
(define (extend-env env sym val) ...)

; apply the given environment with the symbol and return its value
(define (apply-env env sym) ...)

; push a new scope onto the environment list and return the new environment
(define (push-scope env) ...)

; pops a scope from the environment list and return the remaining environment
(define (pop-scope env) ...)

