#lang racket
(provide (all-defined-out))

;TYPES

; types for Ni
(struct NiType ([actual #:mutable]) #:transparent
 #:guard (λ (actual typename)
            (if (eq? typename 'NiType)
                (error "Can't instantiate NiType directly.")
                (if (or (eq? actual '()) (NiType? actual)) 
                    (values actual)
                    (error "Can only instantiate with NiTypes or '()")))))
                

(struct StringType NiType () #:transparent)
(struct VoidType NiType () #:transparent)
(struct IntType NiType () #:transparent)
(struct NameType NiType () #:transparent)
(struct BoolType NiType () #:transparent)
(struct PengType NiType () #:transparent)
(struct ArrayType NiType (element-type) #:transparent)
; for records, we need two structs
(struct RecordType NiType (fields) #:transparent)
; in this case, the name is the symbol name of a field, 
; and actual will refer to the actual type
(struct NameTypePair NiType (name [result #:auto #:mutable]) #:transparent #:auto-value #f)


;do actual type
(define (actual-type ty)
  (match ty
  [(StringType _) ty]
  [(VoidType _) ty]
  [(IntType _) ty]
  [(ArrayType _ _) ty]
  [(RecordType _ _) ty]
  [(BoolType _)ty]
  [(PengType _) ty]
  [(NameType fieldName)
   (cond
     [(eq? fieldName '()) ty]
     [else (actual-type fieldName)])])
 )

(define (make-StringType)
  (StringType '()))
(define (make-VoidType)
  (VoidType '()))
(define (make-IntType)
  (IntType '()))
(define (make-ArrayType expr)
  (ArrayType '() expr))
(define (make-RecordType fields)
  (RecordType '() fields))
(define (make-BoolType)
  (BoolType '()))
(define (make-PengType)
  (PengType '()))
(define (make-NameType fieldName)
  (NameType fieldName))

;VALUES
(struct VarValue (type readOnly [result #:auto #:mutable]) #:transparent #:auto-value #f)
; as with records, we need something for parameters,
; so this will be stored as a list of NameTypePair structs
(struct FunValue (parameters return-type [result #:auto #:mutable]) #:transparent #:auto-value #f)
