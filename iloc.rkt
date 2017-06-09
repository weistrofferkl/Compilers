#lang racket

(require "names.rkt")

(provide (all-defined-out))

; base struct for our instructions
(struct iloc (src1 src2 target) #:transparent)

; macro fun in racket! Just to be clear, this simply creates a structure that
; inherits from iloc but that doesn't add any fields--if you're bored, you could
; rewrite this for me to let me just pass a list of names and then generate a set
; of structures that inherits from that list of names and we could shrink this
; (not including comments) down to a couple of lines ;)
(define-syntax (iloc-child stx)
  (syntax-case stx ()
    [(_ name)
     #'(struct name iloc () #:transparent)]))


; note, the => syntax indicates that the result is stored in that location
; and we use rk to indicate register k, MEMORY(...) to indicate memory access
; and ck to indicate an immediate (or constant) value

; math ops between registers
(iloc-child add)       ; r1 + r2 => r3
(iloc-child sub)       ; r1 - r2 => r3
(iloc-child mult)      ; r1 * r2 => r3
(iloc-child div)       ; r1 / r2 => r3

; immediate math ops (2nd target is a constant value)
(iloc-child addI)      ; r1 + c2 => r3
(iloc-child subI)      ; r1 - c2 => r3
(iloc-child rsubI)     ; c1 - r1 => r3
(iloc-child multI)     ; r1 * c2 => r3
(iloc-child divI)      ; r1 / c2 => r3
(iloc-child rdivI)     ; c2 / r1 => r3

; bit shifting in registers, without and with constants
(iloc-child lshift)    ; r1 << r2 => r3
(iloc-child rshift)    ; r1 >> r2 => r3
(iloc-child lshiftI)   ; r1 << c2 => r3
(iloc-child rshiftI)   ; r1 >> c2 => r3

; logic ops, without and with constants
(iloc-child and)       ; r1 & r2 => r3
(iloc-child or)        ; r1 | r2 => r3
(iloc-child xor)       ; r1 xor r2 => r3
(iloc-child andI)      ; r1 & c2 => r3
(iloc-child orI)       ; r1 | c2 => r3
(iloc-child xorI)      ; r1 xor c2 => r3

; ops for loading
(iloc-child loadI)     ; c1 => r2  -- loads the value c1 into r2
(iloc-child load)      ; MEMORY(r1) => r2  -- loads the value at memory location r1 into r2
(iloc-child loadAI)    ; MEMORY(r1 + c2) => r3 -- adds the immediate value c1 to the register r1 to calculate the memory address
(iloc-child loadAO)    ; MEMORY(r1 + r2) => r3 -- adds the value at offset r2 from memory location r1 into r3

; character loading
(iloc-child cload)     ; character load
(iloc-child cloadAI)   ; character loadAI
(iloc-child cloadAO)   ; character loadAO

; storing values
(iloc-child store)     ; r1 => MEMORY(r2) -- stores r1 at memory location r2
(iloc-child storeAI)   ; r1 => MEMORY(r2 + c3)
(iloc-child storeAO)   ; r1 => MEMORY(r2 + r3)

; storing characters
(iloc-child cstore)    ; character store
(iloc-child cstoreAI)  ; character storeAI
(iloc-child cstoreAO)  ; character storeAO

; register to register copying
(iloc-child i2i)       ; r1 => r2
(iloc-child c2c)       ; r1 => r2
(iloc-child c2i)       ; convert character to integer in r1 and copy to r2
(iloc-child i2c)       ; convert integer to character in r1 and copy to r2

; jump/branch
(iloc-child jump)      ; r1 -> PC
(iloc-child jumpI)     ; l1 -> PC
(iloc-child cbr)       ; if r1 = true, l2 -> PC, else l3 -> PC
(iloc-child tbl)       ; r1 might hold l2

; comparisons
(iloc-child cmp_LT)    ; true => r3 if r1 < r2, else false => r3
(iloc-child cmp_LE)    ; true => r3 if r1 <= r2, else false => r3
(iloc-child cmp_EQ)    ; true => r3 if r1 = r2, else false => r3
(iloc-child cmp_GE)    ; true => r3 if r1 >= r2, else false => r3
(iloc-child cmp_GT)    ; true => r3 if r1 > r2, else false => r3
(iloc-child cmp_NE)    ; true => r3 if r1 <> r2, else false => r3

; compares and deposits the result of that comparison into cc3
; to indicate if it's less than, greater than, etc
(iloc-child comp)      ; sets cc3 to the comparison of r1 and r2

; compare and branch
(iloc-child cbr_LT)    ; l2 -> PC if cc3 = LT, otherwise l3 -> PC
(iloc-child cbr_LE)    ; l2 -> PC if cc3 = LE, otherwise l3 -> PC
(iloc-child cbr_EQ)    ; l2 -> PC if cc3 = EQ, otherwise l3 -> PC
(iloc-child cbr_GE)    ; l2 -> PC if cc3 = GE, otherwise l3 -> PC
(iloc-child cbr_GT)    ; l2 -> PC if cc3 = GT, otherwise l3 -> PC
(iloc-child cbr_NE)    ; l2 -> PC if cc3 = NE, otherwise l3 -> PC

(iloc-child nop)

(define (display-iloc ir out)
  (let ([src1 (iloc-src1 ir)]
        [src2 (iloc-src2 ir)]
        [target (iloc-target ir)])
    (let ([str1 (cond
                  [(Result? src1) (result->string src1)]
                  [(string? src1) src1]
                  [(number? src1) src1]
                  [else ""])]
          [str2 (cond
                  [(Result? src2) (result->string src2)]
                  [(string? src2) src2]
                  [(number? src2) src2]
                  [else ""])]
          [tstr (cond
                  [(Result? target) (result->string target)]
                  [(string? target) target]
                  [(number? target) target]
                  [else ""])])
      (let-values ([(sym fcount acount access-proc mutate-proc imm-k sup-type skip?)
                    (struct-type-info (let-values ([(type skip?) (struct-info ir)]) type)
                                        )])
        (let ([formatstr
               (cond
                 [(eq? sym 'add) "~a ~a + ~a -> ~a~n"]
                 [(eq? sym 'sub) "~a ~a - ~a -> ~a~n"]
                 [(eq? sym 'mult) "~a ~a * ~a -> ~a~n"]
                 [(eq? sym 'div) "~a ~a / ~a -> ~a~n"]
                 [(eq? sym 'loadI) "~a ~a -> ~a ~a~n"]
                 [(eq? sym 'cmp_EQ) "~a ~a = ~a -> ~a~n"]
                 [(eq? sym 'cmp_GE) "~a ~a >= ~a -> ~a~n"]
                 [(eq? sym 'cmp_LE) "~a ~a <= ~a -> ~a~n"]
                 [(eq? sym 'cmp_GT) "~a ~a > ~a -> ~a~n"]
                 [(eq? sym 'cmp_LT) "~a ~a < ~a -> ~a~n"]
                 [(eq? sym 'cmp_NE) "~a ~a <> ~a -> ~a~n"]
                 [(eq? sym 'and) "~a ~a & ~a -> ~a~n"]
                 [(eq? sym 'or) "~a ~a | ~a -> ~a~n"]
                 [(eq? sym 'not) "~a ~~~a -> ~a ~a~n"]
                 [else "~a ~a, ~a ~a~n"])])
          (fprintf out formatstr
                   sym str1 str2 tstr))))))