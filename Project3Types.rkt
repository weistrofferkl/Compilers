#lang racket

(provide (all-defined-out))
         
; types for Ni
(struct NiType ([actual #:mutable]) #:transparent
  #:guard (λ (actual typename)
            (if (eq? typename 'NiType)
                (error "Can't instantiate NiType directly.")
                (if (or (eq? actual '())
                        (NiType? actual)) (values actual)
                                          (raise-arguments-error typename
                                           "Can only instantiate with NiTypes or '()"
                                           "actual" actual)))))

(struct StringType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (write-string "t:str" port)))])
(struct VoidType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (write-string "t:void" port)))])
(struct IntType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (write-string "t:int" port)))])

(struct NameType NiType (name) #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (let ([theprinter (case mode
                                                                       [(#t) write]
                                                                       [(#f) display]
                                                                       [else (lambda (p port) (print p port mode))])])
                                                     (write-string "<name: " port)
                                                     (theprinter (NiType-actual ty) port)
                                                     (write-string ">" port))))]
  #:guard (λ (actual name tyname)
            (cond
              [(and (not (symbol? name)) (raise-arguments-error tyname
                                                                "NameType name must be a symbol"
                                                                "actual" actual
                                                                "name" name))]
              [else (values actual name)])))
                                                   

(struct BoolType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (write-string "t:bool" port)))])
(struct PengType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (write-string "t:peng" port)))])

; label will be used during emission to give it a unique name so that name types don't clash
(struct ArrayType NiType (name element-type [label #:mutable #:auto]) #:transparent
  #:auto-value #f
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (let ([theprinter (case mode
                                                                       [(#t) write]
                                                                       [(#f) display]
                                                                       [else (lambda (p port) (print p port mode))])])
                                                     (display "t:array[ " port)
                                                     (theprinter (ArrayType-element-type ty) port)
                                                     (display " ]" port))))])

; for records, we need two structs
(struct RecordType NiType (name fields [label #:mutable #:auto]) #:transparent
  #:auto-value #f
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (let ([theprinter (case mode
                                                                       [(#t) write]
                                                                       [(#f) display]
                                                                       [else (lambda (p port) (print p port mode))])])
                                                     (display "t:rec{ " port)
                                                     (for-each (lambda (field)
                                                                 (theprinter field port)
                                                                 (display " " port)) (RecordType-fields ty))
                                                     (display "}" port))))])

  
; in this case, the name is the symbol name of a field, 
; and actual will refer to the actual type
(struct NameTypePair (type name) #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (let ([theprinter (case mode
                                                                       [(#t) write]
                                                                       [(#f) display]
                                                                       [else (lambda (p port) (print p port mode))])])
                                                     (display "<" port)
                                                     (theprinter (NameTypePair-name ty) port)
                                                     (display ", " port)
                                                     (theprinter (NameTypePair-type ty) port)
                                                     (display ">" port))))]
  #:guard (λ (type name typename)
            (cond [(and (or (VarValue? type) (NiType? type)) (symbol? name))
                   (values type name)]
                  [else
                   (raise-arguments-error "NameTypePair requires a VarValue/NiType and symbol"
                                          typename
                                          "type" type
                                          "name" name)])))
                                                  



; values for Ni
(struct VarValue (type
                  [read-only? #:mutable]
                  [level #:mutable] ; the lexical scope of the variable
                  [escape? #:mutable]
                  [offset #:mutable] ; the offset of the declaration in the frame
                  [result #:auto #:mutable]) #:transparent #:auto-value #f)

; as with records, we need something for parameters,
; so this will be stored as a list of NameTypePair structs, but note
; we use VarValues for the types and symbols for the names
; Also, wouldn't putting its name in here be so great???
; note, for the translation phase, we need parameters to be a list
; of varvalues
(struct FunValue (name parameters return-type [label #:mutable #:auto]
                             [frame #:mutable #:auto])
  #:transparent #:auto-value #f)

(define (bind-nametype! nametype type)
  (set-NiType-actual! nametype type))

; by default, var values do not escape
(define (make-VarValue ty)
  (VarValue ty #f #f #f #f))

(define (make-IntType)
  (IntType '()))

(define (make-StringType)
  (StringType '()))

(define (make-VoidType)
  (VoidType '()))

(define (make-ArrayType name etype)
  (ArrayType '() name etype))

(define (make-RecordType name fields)
  (RecordType '() name fields))

(define (make-PengType)
  (PengType '()))

(define (make-BoolType)
  (BoolType '()))

(define (make-NameType actual name)
  (NameType actual name))

; returns the actual type of any type, this is primarily designed to handle Name aliases
(define (actual-type ty)
  (let ([actual (NiType-actual ty)])
    (cond
      [(and (null? actual) (NameType? ty)) (error "NameType must have an actual by the time you call actual-type on it!")]
      [(null? actual) ty]
      [else (actual-type actual)])))

(define (base-type-name? name)
  (or (eq? name 'int)
      (eq? name 'string)
      (eq? name 'bool)
      (eq? name 'peng)))

(define (base-type? ty)
  (or (IntType? ty)
      (StringType? ty)
      (BoolType? ty)
      (PengType? ty)
      (VoidType? ty)))

; this checks to see if you have a name cycle among name types
(define (name-cycle? nametype)
  (letrec ([found-name? (λ (type name)
                          (cond
                            ; empty list, or non NameType and we're done
                            [(or (empty? type) (not (NameType? type))) #f]
                            ; otherwise, see if the name is equal to this one
                            ; and return true if it is
                            [(eq? name (NameType-name type)) #t]
                            ; finally, if it's not equal, check the rest
                            [else (found-name? (NiType-actual type) name)]))])
    (cond
      ; empty list, or non NameType and we're done
      [(or (empty? nametype) (not (NameType? nametype))) #f]
      ; otherwise, see if this name lives in any of its children, if so, return true
      [(eq? (found-name? (NiType-actual nametype) (NameType-name nametype)) #t) #t]
      ; otherwise, recursively try the next one
      [else (name-cycle? (NiType-actual nametype))])))

; tests to see if it's only name types recursively
(define (names-only? nametype)
  (cond
    [(not (NiType? nametype)) (raise-arguments-error 'nametype "You can only pass NiTypes to names-only?" "nametype" nametype)]
    [(NameType? nametype) (let ([next (NiType-actual nametype)])
                            (if (empty? next) #t (names-only? next)))]
    [else #f]))


(define (type=? t1 t2)
  (if (and (NiType? t1) (NiType? t2)) 
      (let ([at1 (actual-type t1)]
            [at2 (actual-type t2)])
        (cond
          [(and (IntType? at1) (IntType? at2)) #t]
          [(and (StringType? at1) (StringType? at2)) #t]
          [(and (BoolType? at1) (BoolType? at2)) #t]
          [(and (PengType? at1) (PengType? at2)) #t]
          [(and (VoidType? at1) (VoidType? at2)) #t]
          [(and (RecordType? at1) (RecordType? at2) (eq? at1 at2)) #t]
          [(and (ArrayType? at1) (ArrayType? at2) (eq? at1 at2)) #t]
          [else #f]))
      #f))