#lang racket

(require "errors.rkt"
         "niparser.rkt"
         (prefix-in lex: parser-tools/lex)
         test-engine/racket-tests)

(provide (all-defined-out))

; the file we're currently parsing--we can set it so we can log the filename easier
(define currentFile (make-parameter ""))
(define currentOut (make-parameter (current-output-port)))

(define (log str)
  (fprintf (currentOut) "~a~n" str))


; logs the string with the given prefix
(define log-prefix (lambda (prefix str astnode . args)
  (let ([pos (get-note astnode 'position)])
    (if (string=? (currentFile) "")
        (apply fprintf (currentOut)
                 (string-append "~a at line: ~a, col ~a: " str "~n") prefix
                 (lex:position-line pos) (lex:position-col pos)
                 args)
        (apply fprintf (currentOut)
                 (string-append "~a in ~a at line: ~a, col ~a: " str "~n")
                 prefix (currentFile) (lex:position-line pos)
                 (lex:position-col pos) args)))))
           
           
; logs a syntax error, setting the syntax error flag to true
(define log-syntaxerror (lambda (str astnode . args)
  (parse-error #t)
  (apply log-prefix "Syntax error" str astnode args)))

; logs a type error, setting the type error flag to true
(define log-typeerror (lambda (str astnode . args)
  (type-error #t)
  (apply log-prefix "Type error" str astnode args)))

; logs a lexing error, setting the scan-error flag to true
(define log-scanerror (lambda (str astnode . args)
  (scan-error #t)
  (apply log-prefix "Scanning error" str astnode args)))


  