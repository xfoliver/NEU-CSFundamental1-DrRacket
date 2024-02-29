;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/2: Design the data SimpleExpression that allows you to represent
;           arithmetic expressions given the following requirements:
;           - There are two valid operations: add (+) and multiply (*)
;           - Each operation is applied to at least one operand, which may be a
;             number or another expression
;
;           Some example expressions include...
;            5
;            3.14 * 2
;            (3 + 4) * (1 + -2 + 7) * (5)
;            
;           Make sure to follow all steps of the design recipe for data and
;           define at least the examples above.

; A Valid Operation is one of the following:
; - add (+)
; - multiply (*)

; Interpretation: Mathematic Operations that produces a numerical result based on its operator

(define addition "+")

(define multiplication "*")

(define (operation-temp operator)
  (...
   [(string=? operator addition) ...]
   [(string=? operator multiplication) ...]))


; 
; A SimpleExpression is one of:
; num
; (make-simpleExpression [List-of simpleExpression] operator)

(define-struct simpleExpression [operator listofsimpleExpression])

;(define (simpleExpression-temp a)
;   (cond
;     [(number? a)...]
;     [(simpleExpression? a)
;      (operation-temp (simpleExpression-operator a)...)
;      (simpleExpression-listofNumExpression a)...]))

  




; TODO 2/2: Now design the function evaluate that takes a SimpleExpression and
;           produces its numerical result. So for the examples above (where <=
;           means that the expression on the right evaluates to the value on the
;           left...
;
;            5    <= 5
;            6.28 <= 3.14 * 2
;            210  <= (3 + 4) * (1 + -2 + 7) * (5)


; evaluate: SimpleExpression -> Number
; takes a SimpleExpression and produces its numerical result

(define (evaluate sexp)
  (cond
    [(number? sexp) sexp]
    [else
     (cond
       [(string=? (simpleExpression-operator sexp) addition)
        (foldr + 0 (map evaluate (simpleExpression-listofsimpleExpression sexp)))]
       [(string=? (simpleExpression-operator sexp) multiplication)
        (foldr * 1 (map evaluate (simpleExpression-listofsimpleExpression sexp)))])]))
      


(check-expect (evaluate (make-simpleExpression multiplication (list 3.14 2))) 6.28)

(check-expect (evaluate 5) 5)

(check-expect (evaluate (make-simpleExpression
                         multiplication
                         (list (make-simpleExpression addition (list 3 4))
                               (make-simpleExpression multiplication (list 1 5))))) 35)


(check-expect (evaluate (make-simpleExpression
                         multiplication
                         (list (make-simpleExpression addition (list 3 4))
                               (make-simpleExpression addition (list 1 -2 7))
                               (make-simpleExpression multiplication (list 5 1))))) 210)

