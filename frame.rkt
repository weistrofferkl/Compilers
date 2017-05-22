#lang racket

(require "Project3Types.rkt"
         "names.rkt")

(provide (all-defined-out))

; we're assuming a 64-bit machine, this would change depending on the
; target architecture and you'd probably do some fancy module thing
(define WORD_SIZE 8)


; a frame consists of its name, which is a label (labels are addresses)
; a static-link, which is also a label, an offset, which just counts how
; many locals and escaping variables have been seen, and finally, a
; reference to the funval associated with this Frame (because we'll need
; parameters
(struct Frame (name ;label of surounding function
               [static-link #:mutable] ;points to outerframe, top of stack
               [offset #:mutable]
               [funval #:mutable])
  #:transparent
  #:guard (λ (name static-link offset funval tyname)
            (cond
              [(not (or (Label? name)
                        (or (Label? static-link) (eq? static-link #f))
                        (exact-positive-integer? offset)
                        (or (FunValue? funval) (eq? funval #f))))
               (raise-arguments-error tyname "argument type mismatch for Frame"
                                      "Label? name" name "Label? static-link" static-link
                                      "number? offset" offset "FunValue? funva" funval)]
              [else (values name static-link offset funval)])))

            

; build a frame, now with less typing, this always allocates one word because
; we need a static link (probably), and this gets us there
(define (make-frame name)
  (Frame name #f WORD_SIZE #f))

; create a frame based off info from the funvalue
(define (make-frame-from-funvalue funval)
  (let ([frame (make-frame (make-label))])
    ; set up references to and from the frame
    (set-FunValue-frame! funval frame)
    (set-Frame-funval! frame funval)
    frame))
     
; well, by default, all types are 1 word size, since we have ints and pointers,
; but in theory, you'd modify this by the size of the actual type (like a float
; would be 4 bytes, a character fewer possibly
(define (get-type-size ty)
  WORD_SIZE)


; a function to call that allocates a local variable on the frame
(define (alloc-local! frame ty)
  ; get the number of bytes it takes up
  (let ([bytes (get-type-size ty)]
        [current-offset (Frame-offset frame)])
    (set-Frame-offset! frame (+ current-offset bytes))
    ; and then return the current-offset (not the new one)
    ; so it can be passed back and stored in the VarValue
    ; itself as needed
    current-offset))

(define (output-frame out frame)
  (fprintf out "Frame: ~a, static-link: ~a, allocated-bytes: ~a, function: ~a~n"
           (Label-name (Frame-name frame)) (Label-name (Frame-static-link frame))
           (Frame-offset frame) (FunValue-name (Frame-funval frame))))