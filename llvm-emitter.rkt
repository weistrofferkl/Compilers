#lang racket

;; the purpose of this module is to ease the emission of llvm code
(require "names.rkt"
         "Project3Types.rkt")

(provide (all-defined-out))
         
; this port is used for writing to, by default it's to the screen
(define emission-port (make-parameter (current-output-port)))
; if true, output will also be sent to the screen, but note, if (emission-port) is
; a terminal port, then we'll ignore to-screen? (so we don't double print everything)
; on the other hand, if it isn't a terminal port, then we'll write to there
(define to-screen? (make-parameter #t))
; use the global writer when trying to write to the globals section of your file
(define global-writer (make-parameter (open-output-string)))
; use the fun writer when defining functions
(define fun-writer (make-parameter (open-output-string)))
; use the main writer for defining the main expression 
(define main-writer (make-parameter (open-output-string)))
; a stack of writers

; the current writer is what you're currently writing to
(define current-writer (make-parameter (main-writer)))

; the collection of writers, we can push the current one, depending one what we're doing,
; and restore the last one when we're done--but note, this *won't* work for nested functions
; because the fun-writer assumes one function at a time using this style--you'd need a list
; or something of those writers so you could write them out sequentially 
(define writers (make-parameter '()))

  
; pushes the writer onto the list of writers
(define (push-writer wr)
  (writers (cons wr (writers))))

; pops and returns the last writer
(define (pop-writer)
  (let ([wr (first (writers))])
    (writers (rest (writers)))
    wr))
  
    
; these are just syntactic sugar to make it clear what we're doing
; we have two methods for starting and ending global data
(define (begin-global-defn)
  (push-writer (current-writer))
  (current-writer (global-writer)))

(define (end-global-defn)
  (current-writer (pop-writer)))

; two methods for starting and ending function data
(define (begin-fun-defn)
  (push-writer (current-writer))
  (current-writer (fun-writer)))
(define (end-fun-defn)
  (current-writer (pop-writer)))

; methods for handling main definitions
(define (begin-main-defn)
  (push-writer (current-writer))
  (current-writer (main-writer)))
(define (end-main-defn)
  (current-writer (pop-writer)))


; this should be called prior to any other emissions--it sets up the llvm file correctly
(define (emit-header)
  (begin-global-defn)
  (displayln "; target data layout for Mac, change to m:w instead for Windows" (current-writer))
  (displayln "target datalayout = \"e-m:o-i64:64-f80:128-n8:16:32:64-S128\"" (current-writer))
  (displayln (current-writer))
  (displayln "; target triple for Mac" (current-writer))
  (displayln "target triple = \"x86_64-apple-macosx10.10.0\"" (current-writer))
  (displayln (current-writer))

  ; output the struct defn for strings
  (displayln "%struct.string = type { i64, i8* }" (current-writer))

  ; output struct defns for arrays
  (displayln "%struct.array = type { i64, i64* }" (current-writer))

  ; note that globals are up and coming
  (displayln "; GLOBAL variables, defined in the program" (current-writer))

  (end-global-defn)
  
  ; now emit functions to handle strings
  (emit-standard-library))

; this emits headers for the standard library, which is written in C. We just compile
; it with cc -c NiStdLib.c to get a NiStdLib.o file, which we can then compile along
; with the generated source, so cc NiStdLib.o *.ll
(define (emit-standard-library)
  (begin-fun-defn)
  
   ; now just linking with the object file, seems easier, less chance for error
  (displayln "; STANDARD LIB DECLARATIONS" (current-writer))
  (displayln "declare %struct.string* @makeString(i8* %str)" (current-writer))
  (displayln "declare %struct.string* @getChar()" (current-writer))
  (displayln "declare i64 @ord(%struct.string* nocapture readonly %str)" (current-writer))
  (displayln "declare %struct.string* @chr(i64 %i)" (current-writer))
  (displayln "declare i64 @size(%struct.string* nocapture readonly %str)" (current-writer))
  (displayln "declare %struct.string* @substring(%struct.string* nocapture readonly %str, i32 %first, i32 %n)" (current-writer))
  (displayln "declare %struct.string* @concat(%struct.string* nocapture readonly %s1, %struct.string* nocapture readonly %s2)" (current-writer))
  (displayln "declare void @Exit(i64 %i)" (current-writer))
  (displayln "declare void @print(%struct.string* nocapture readonly %str)" (current-writer))
  (displayln "declare void @printi(i64 %v) #0" (current-writer))
  (displayln "declare %struct.string* @intToString(i64 %val)" (current-writer))
  (displayln "declare %struct.array* @makeArray(i64 %numElements)" (current-writer))
  (displayln "declare i64* @getElementAddressAt(%struct.array* %arr, i64 %index)" (current-writer))
  (displayln "; Function Attrs: nounwind" (current-writer))
  (displayln "declare noalias i8* @malloc(i64) #1" (current-writer))
  
  (end-fun-defn))

; emits the header for main, which is required as the entry point into a program
; in essence, much of the code in the let body or expression will appear here
(define (emit-main-header)
  (displayln (current-writer))
  (displayln "; Function Attrs: nounwind ssp uwtable" (current-writer))
  (displayln "define i32 @main(i32 %argc, i8** %argv) #0 {" (current-writer))
  (displayln "%1 = alloca i32, align 4" (current-writer))
  (displayln "%2 = alloca i32, align 4" (current-writer))
  (displayln "%3 = alloca i8**, align 8" (current-writer))
  (displayln "store i32 0, i32* %1" (current-writer))
  (displayln "store i32 %argc, i32* %2, align 4" (current-writer))
  (displayln "store i8** %argv, i8*** %3, align 8" (current-writer))
  (displayln (current-writer))
  (displayln "; Ni program to follow..." (current-writer)))

; emits code that ends the ni program, basically a return from main, plus loads of other stuff
(define (emit-main-trailer)
  (displayln "; ... end Ni program" (current-writer))
  (displayln "ret i32 0" (current-writer))
  (displayln "}" (current-writer))

  (displayln "attributes #0 = { nounwind ssp uwtable " (current-writer))
  (displayln "\"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" " (current-writer))
  (displayln "\"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" " (current-writer))
  (displayln "\"no-nans-fp-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" " (current-writer))
  (displayln "\"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" } " (current-writer)))


; this is called when you're finally done to wrap things up
(define (finish-emission)
  (println "; GLOBALS")
  (print (get-output-string (global-writer)))
  (println "; FUNCTIONS")
  (print (get-output-string (fun-writer)))
  (println "; MAIN")
  (print (get-output-string (main-writer))))
 
; try to simplify output, don't double print though
(define (println str . rest)
  (cond
    [(and (to-screen?) (not (terminal-port? (emission-port)))) (displayln (string-append* str rest))])
  (displayln (string-append* str rest) (emission-port)))

(define (newline)
  (cond
    [(and (to-screen?) (not (terminal-port? (emission-port)))) (displayln "")])
  (displayln "" (current-writer)))

; try to simplify output, don't double print though
(define (print str . rest)
  (cond
    [(and (to-screen?) (not (terminal-port? (emission-port)))) (display (string-append* str rest))])
  (display (string-append* str rest) (emission-port)))


;;; these are helper functions to emit code for what they do
(define (comment str)
  (newline)
  (fprintf (current-writer) "; ~a~n" str))


; Usage: to emit proper llvm math expressions 
; emit-math: symbol? (or Result? string?) (or Result? String?) Result? -> void
; this function will emit a math llvm ir sequence if you pass it one
; of the following as the first arg: 'add 'sub 'mul 'div, generally
; you use this by passing two result arguments to it and it will return
; the third one back to you (LLVM requires new names, we assume these go into
; register temps and to save some work we create it for you)
(define (emit-math mathsym v1 v2 [result (make-temp-result)])
  (let ([v1str (if (Result? v1) (result->string v1) v1)]
        [v2str (if (Result? v2) (result->string v2) v2)])
    (let ([resstr (result->string result)]
          [tyname "i64 "])
      (cond
        [(not (and (symbol? mathsym) (Result? v1) (Result? v2)))
         (raise-arguments-error 'emit-math "mathsym should be a symbol and v1 and v2 should be Result types"
                                "mathsym" mathsym
                                "v1" v1
                                "v2" v2)]
        [(eq? mathsym 'add) (println resstr " = add " tyname v1str ", " v2str)]
        [(eq? mathsym 'sub) (println resstr " = sub " tyname v1str ", " v2str)]
        [(eq? mathsym 'mul) (println resstr " = mul " tyname v1str ", " v2str)]
        [(eq? mathsym 'div) (println resstr " = sdiv " tyname v1str ", " v2str)]
        [else (raise-arguments-error 'emit-math "mathsym must be 'add, 'sub, 'mul, or 'div"
                                     "mathsym" mathsym)])
      result)))


(define (get-type-name nitype)
  (let ([ty (actual-type nitype)])
    (match ty
      [(IntType _) "i64"]
      [(BoolType _) "i8"]
      [(StringType _) "%struct.string *"]
      [(VoidType _) "void"]
      [(ArrayType _ _) "%struct.array *"]
      ; you'll want to add records here...
      [_ (error "get-type-name not worknig for your type!")])))
                     




