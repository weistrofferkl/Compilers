#lang racket

(require "Project3Types.rkt")

(provide (all-defined-out))

(define temp-num (make-parameter 0))
(define label-num (make-parameter 0))

; resets the counting for all temps and labels
(define (reset-names)
  (temp-num 0)
  (label-num 0))

; get the next temp number and increment it
(define (get-next-temp-num)
  (temp-num (add1 (temp-num)))
  (temp-num))

; increment the next temp number
(define (get-next-label-num)
  (label-num (add1 (label-num)))
  (label-num))

; the temp struct
(struct Temp (name num) #:transparent
  #:guard (λ (name num tyname)
            (cond
              [(not (and (string? name) (number? num)))
               (raise-arguments-error 'Temp "Temp must be constructed with a string and number"
                                     "name" name "num" num)]
              [else (values name num)])))

; the label struct
(struct Label (name) #:transparent
  #:guard (λ (name tyname)
            (cond 
              [(not (string? name))
               (raise-arguments-error 'Label "Label must be constructed with a string"
                                      "name" name)]
              [else (values name)])))

; the result struct, this stores either a label or a temp
(struct Result (name [global? #:mutable] [value #:mutable]) #:transparent
  #:guard (λ (name global? value tyname)
            (cond
              [(not (or (Temp? name) (Label? name)))
               (error 'Result
                      "name argument must be a Temp or a Label")]
              [(not (boolean? global?))
               (error 'Result "global? must be a boolean")]
              [(and (eq? global? #t) (not (Label? name)))
               (error 'Result "Cannot make a global temporary (just doesn't make sense)")]
              [(not (or (VarValue? value) (eq? value #f)))
               (error 'Result "value must be a VarValue or #f")]
              [else (values name global? value)])))

; get a new label
(define (make-label [num (get-next-label-num)])
  (Label (string-append "L" (number->string num))))

(define (make-named-temp-result num)
  (Result (make-temp num) #f #f))
  
; get a new temp
(define (make-temp [num (get-next-temp-num)])
  (Temp (string-append "t" (number->string num)) num))

; construct a global
(define (make-global-result)
  (Result (make-label) #t #f))

; construct a label result
(define (make-label-result)
  (Result (make-label) #f #f))

; another name for making a label
(define (make-frame-result)
  (Result (make-label) #f #f))

(define (make-frame-offset sym)
  (Result (Label (string-append "@" (symbol->string sym))) #f #f))

; construct a temp result
(define (make-temp-result)
  (Result (make-temp) #f #f))

; construct a string name for the result, which is determined by
; whether or not it's global and/or 
(define (result->string res)
  (match res
    [(Result (Label name) #t _)
     (string-append "@" name)]
    [(Result (Label name) #f _)
     (if (and (> (string-length name) 0) (equal? (string-ref name 0) #\@))
         name
         (string-append "%" name))]
    [(Result (Temp name num) _ _)
     (string-append "%" name)]
    [else (error "Unknown result type!")]))

; true or false if the result is a temp kind
(define (in-register? res)
  (Temp? (Result-name res)))

; true or false if it's an in-frame result (it could be a global, for example)
(define (in-frame? res)
  (and (Label? (Result-name res)) (not (Result-global? res))))

; true or false if a result is a global
(define (global? res)
  (and (Label? (Result-name res)) (Result-global? res)))

; it's a one way trip, haha
(define (set-global! res)
  (set-Result-global?! res #t))