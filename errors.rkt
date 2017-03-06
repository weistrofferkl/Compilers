#lang racket

; generally, we just want a centralized module for errors when compiling
(provide (all-defined-out))

(define scan-error (make-parameter #t))
(define parse-error (make-parameter #t))
(define type-error (make-parameter #t))

; clears all the errors 
(define (clear-errors)
  (scan-error #f)
  (parse-error #f)
  (type-error #f))
