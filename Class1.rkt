#lang racket
(define x 5)

;this is a function that adds 1 to variable x
(define (add1 x)
  ( + x 1))

(if (= x 5) "Yay" "Nay")

(cond ([= x 5] "MER")
      ([= x 4] "HER")
      (else "NO 5"))