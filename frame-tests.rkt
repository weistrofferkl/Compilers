#lang racket/base

(require rackunit
         rackunit/text-ui
         (prefix-in lex: parser-tools/lex)
         "frame.rkt")

(define frame-tests
  (test-suite
   "Tests for the Ni frame module"



   ))

(run-tests frame-tests)