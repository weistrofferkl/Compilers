#lang racket
(require racket/cmdline)
(require test-engine/racket-tests)
(require parser-tools/cfg-parser)

(require "Project1.rkt")
(provide (all-defined-out))

(struct vardecl (type id expr))
(define niparser
  (cfg-parser
   (src-pos)
   (start )
   (end EOF)
   (tokens value-tokens paren-types operators punctuation comparators boolops keywords endoffile)
   (grammar
    (var-declarations
     [(NI ID IS expr) (...)]


