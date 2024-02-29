;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Back to Wordle (https://www.nytimes.com/games/wordle/)!

; Recall that in HW2 you designed the type LetterStatus, an enumeration
; that categorized letters as correct, misplaced, or wrong.

; TODO 1/1: Design LetterStatusPair - data that represents pairing a letter
;           (1String) with its status (LetterStatus). Example values might
;           include that the the letter "A" is correct, "B" is wrong, or
;           "C" is misplaced.
;
;           Notes:
;           - You are welcome to use your own (correct) design of
;             LetterStatus, or ours (once released); either way, include
;             it below as a part of your solution to this problem.
;           - Follow all steps of the design recipe for data and remember
;             that in templates, if the type of a field is a data definition,
;             you need to call its associated template!


;; a LetterStatus is one of:
;; - "correct"
;; - "misplaced"
;; - "wrong"
;; represents the three categories of letters.

;example
(define LS-correct "correct")
(define LS-misplaced "misplaced")
(define LS-wrong "wrong")

;template
(define (LS-temp x)
  (... (cond [(string=? x LS-correct) ...]
             [(string=? x LS-misplaced) ...]
             [(string=? x LS-wrong) ...])))

(define-struct LetterStatusPair [LetterString LS])
;A LetterStatusPair is a (make-LetterStatusPair [1string LetterStatus])
;Interpretation: represent a string's status
;LetterString: The letter that I input
;LS: The status of the letter


;example
(define LetterStatusPair-1 (make-LetterStatusPair "A" LS-correct))
(define LetterStatusPair-2 (make-LetterStatusPair "B" LS-misplaced))
(define LetterStatusPair-3 (make-LetterStatusPair "C" LS-wrong))

;template
(define (LetterStatusPair-temp lsp)
  (... (LetterStatusPair-LetterString lsp) ...
       (LetterStatusPair-LS lsp) ...))



