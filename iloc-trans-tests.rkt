#lang racket

(require "iloc.rkt"
         "iloc-trans-skeleton.rkt"
         "array-list.rkt"
         "names.rkt")
(require rackunit
         rackunit/text-ui)

; macro to substitute properly and simplify testing
(define-syntax-rule (check-type-error src msg)
  (check-exn exn:fail? (thunk (trans src)) msg))

(define-check (trans-equal? src expected msg)
  (with-handlers [(exn:fail? (λ (e) (fail-check msg)))]
    (reset-names)
    (let ([alist (trans src)])
      (if (array-list-equal? alist (vector->array-list expected)) #t
      (fail-check msg)))))
      
(define translation-tests
  (test-suite
   "Tests for the Ni to ILOC translator"

   (trans-equal? "4" `#(,(loadI 4 (make-named-temp-result 1) #f)) "integer test")
   (trans-equal? "2" `#(,(loadI 2 (make-named-temp-result 1) #f)) "integer test")
   (trans-equal? "5+6" `#(,(loadI 5 (make-named-temp-result 1) #f)
                          ,(loadI 6 (make-named-temp-result 2) #f)
                          ,(add (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "math add test")
   (trans-equal? "5-6" `#(,(loadI 5 (make-named-temp-result 1) #f)
                          ,(loadI 6 (make-named-temp-result 2) #f)
                          ,(sub (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "math sub test")
   (trans-equal? "5*6" `#(,(loadI 5 (make-named-temp-result 1) #f)
                          ,(loadI 6 (make-named-temp-result 2) #f)
                          ,(mult (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "math mult test")
   (trans-equal? "5/6" `#(,(loadI 5 (make-named-temp-result 1) #f)
                          ,(loadI 6 (make-named-temp-result 2) #f)
                          ,(div (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "math div test")
   
   (trans-equal? "true" `#(,(loadI 1 (make-named-temp-result 1) #f)) "true literal test")
   (trans-equal? "false" `#(,(loadI 0 (make-named-temp-result 1) #f)) "true literal test")

   ; boolean expressions
   (trans-equal? "5>6" `#(,(loadI 5 (make-named-temp-result 1) #f)
                          ,(loadI 6 (make-named-temp-result 2) #f)
                          ,(cmp_GT (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "greater than test")
   (trans-equal? "5>=6" `#(,(loadI 5 (make-named-temp-result 1) #f)
                          ,(loadI 6 (make-named-temp-result 2) #f)
                          ,(cmp_GE (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "greater than equal test")
   (trans-equal? "5<=6" `#(,(loadI 5 (make-named-temp-result 1) #f)
                          ,(loadI 6 (make-named-temp-result 2) #f)
                          ,(cmp_LE (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "less than equal test")
   (trans-equal? "5<6" `#(,(loadI 5 (make-named-temp-result 1) #f)
                          ,(loadI 6 (make-named-temp-result 2) #f)
                          ,(cmp_LT (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "less than test")
   (trans-equal? "5=6" `#(,(loadI 5 (make-named-temp-result 1) #f)
                          ,(loadI 6 (make-named-temp-result 2) #f)
                          ,(cmp_EQ (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "equal test")
   (trans-equal? "5<>6" `#(,(loadI 5 (make-named-temp-result 1) #f)
                          ,(loadI 6 (make-named-temp-result 2) #f)
                          ,(cmp_NE (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "not equal test")

   ; logic ops
   (trans-equal? "true & false" `#(,(loadI 1 (make-named-temp-result 1) #f)
                          ,(loadI 0 (make-named-temp-result 2) #f)
                          ,(and (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "logical and test")
   (trans-equal? "true | false" `#(,(loadI 1 (make-named-temp-result 1) #f)
                          ,(loadI 0 (make-named-temp-result 2) #f)
                          ,(or (make-named-temp-result 1)
                                (make-named-temp-result 2)
                                (make-named-temp-result 3))) "logical or test")
   ))

(run-tests translation-tests)