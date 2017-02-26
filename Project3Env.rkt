#lang racket
(require test-engine/racket-tests)
(provide (all-defined-out))

;Environements
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


;
;; make sure you defined the empty environment properly
;(check-expect (empty-env) `(,(hash-copy #hash())))
;
;; simple tests for adding things to the environment with only one level of scope
;(check-expect (extend-env (empty-env) 'x 5) `(,(hash-copy #hash((x . 5)))))
;(check-expect (extend-env (extend-env (empty-env) 'x 5) 'y 6) `(,(hash-copy #hash((y . 6) (x . 5)))))
;(check-expect (extend-env (extend-env (extend-env (empty-env) 'x 5) 'y 6) 'z 7) `(,(hash-copy #hash((z . 7) (y . 6) (x . 5)))))
;
;; tests for checking if something is there
;(check-expect (apply-env (extend-env (empty-env) 'x 5) 'x) 5)
;(check-expect (apply-env (extend-env (extend-env (empty-env) 'x 5) 'y 6) 'y) 6)
;(check-expect (apply-env (extend-env (extend-env (extend-env (empty-env) 'x 5) 'y 6) 'z 7) 'z) 7)
;
;; pushing and popping tests (trival)
;(check-expect (push-scope (empty-env)) `(,(make-hash) ,(make-hash)))
;(check-expect (pop-scope (push-scope (empty-env))) `(,(make-hash)))
;
;; something more complicated
;(check-expect (apply-env (extend-env (push-scope (extend-env (empty-env) 'x 5)) 'y 6) 'y) 6)
;(check-expect (apply-env (extend-env (push-scope (extend-env (empty-env) 'x 5)) 'y 6) 'x) 5)
;(check-expect (apply-env (pop-scope (extend-env (push-scope (extend-env (empty-env) 'x 5)) 'y 6)) 'x)5)
;(test)