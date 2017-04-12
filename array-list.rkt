#lang racket

(require "iloc.rkt")

; require a 'growable' vector, we're going to hide it in case we
; we decide to replace it with something else for some reason
(require data/gvector)

(provide (all-defined-out))

; create a new array-list, but don't call the constructor, use make-array-list
(struct array-list (vec) #:transparent)

(define (vector->array-list vec)
  (array-list (vector->gvector vec)))

; creates a new array list
(define (make-array-list [capacity 32])
  (array-list (make-gvector #:capacity capacity)))

; set something at a specific index
(define (array-list-set-item! lst index item)
  (gvector-set! (array-list-vec lst) index item))

; returns the element at a specific index
(define (array-list-item-ref lst index)
  (gvector-ref (array-list-vec lst) index))

; adds an item to the end of the list
(define (array-list-add-item! lst item)
  (gvector-add! (array-list-vec lst) item)
  lst)

; returns the length of the array list 
(define (array-list-length lst)
  (gvector-count (array-list-vec lst)))

; removes whatever is at the given index and copies up
(define (array-list-remove-index lst index)
  (gvector-remove! (array-list-vec lst) index))

(define (display-array-list alist out)
  (for ([i (array-list-vec alist)])
    (if (iloc? i)
        (display-iloc i out)
        (displayln i out))))

(define (array-list-equal? a1 a2)
  (if (= (array-list-length a1)
         (array-list-length a2))
      (letrec ([array-list-equal-helper
                (λ (a1 a2 count)
                  (if (>= count (array-list-length a1))
                      #t
                      (if (equal? (array-list-item-ref a1 count)
                                  (array-list-item-ref a2 count))
                          (array-list-equal-helper a1 a2 (add1 count))
                          #f)))])
        (array-list-equal-helper a1 a2 0))
      #f))