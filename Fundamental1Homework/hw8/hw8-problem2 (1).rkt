;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw8-problem2 (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; IMPORTANT:
; 1. The functions that you design for this problem *must* use the ISL list
;    abstraction(s); you MAY NOT use recursion: doing so will lead you to get
;    no code credit for the function :(
;
; 2. The planning part of this problem is in the SAME file used for ALL parts
;    of Homework 8.


; Your task here will be to design the function elim-contains-char, which takes
; a list of strings and produces a list of the same strings, in the same order,
; but excluding those strings that contain a supplied character (represented as
; a 1String). For clarity, here is the intended signature:

; elim-contains-char : 1String [List-of String] -> [List-of String]


; Note: For purposes of this problem, you should NOT use string-contains? (or
;       similar). Instead, use the explode function to treat the supplied
;       string as a list of characters (each represented as a 1String).


; TODO 1/2: Plan your solution, using the planning interface described on the
;           canvas page for this assignment. ALL PLANNING FOR THIS HW WILL BE
;           DONE IN THE SAME PLACE, AND SUBMITTED TOGETHER.

; TODO 2/2: Design the function elim-contains-char using the ISL list
;           abstractions. YOUR CODE SHOULD NOT USE ANY RECURSION.

; elim-contains-char : 1String [List-of String] -> [List-of String]
; from tking a list of strings, it will produce the same list,
; by filtering out the strings in the list that contains the supplied character given (1string)

(check-expect (elim-contains-char "a" (list "aaa" "bbb" "abc")) (list "bbb"))
(check-expect (elim-contains-char "d" (list "bcd" "lll" "ccc")) (list "lll" "ccc"))
(check-expect (elim-contains-char "e" (list "ecd" "ell" "ecc")) (list))

(define (elim-contains-char character list)
  (local [
          ; include? : String -> Boolean
          ; If the string include the character, its true and will take it out from the list.
          ; False will leave it in the list
          (define (include? string)
            (not (ormap (lambda (l) (string=? character l)) (explode string))))]
    (filter include? list)))



    
    




