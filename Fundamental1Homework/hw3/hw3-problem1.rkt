;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following designed data...

(define-struct sc [str count])

; A StringCount (SC) is a (make-sc String Nat)
; Interpretation: a string and its count of occurrences

(define SC-A1 (make-sc "A" 1))
(define SC-B1 (make-sc "B" 1))
(define SC-C1 (make-sc "C" 1))
(define SC-A2 (make-sc "A" 2))
(define SC-A3 (make-sc "A" 3))

(define (sc-temp sc)
  (... (sc-str sc) ...
       (sc-count sc) ...))

; TODO 1/2: List all signatures of all functions that are defined by
;           this structure and data definition. Your signatures should
;           be as precise as possible. For example, the constructor
;           used in the examples would be...

; make-sc: String Nat -> StringCount
; sc: Any->StringCount
; sc-str: sc->string
; sc-count: sc->Nat
; SC-A1: String Num -> StringCount
; SC-B1: String Num -> StringCount
; SC-C1: String Num -> StringCount
; SC-A2: String Num -> StringCount
; SC-A3: String Num -> StringCount



; TODO 2/2: Design the function increment-if-matches, which accepts a
;           String and a StringCount and produces a new StringCount
;           where the count is updated only if the supplied string
;           is the same as the string in the StringCount. For clarity,
;           you have been supplied tests that should pass; you can
;           create more if you wish, but do not have to.

(check-expect (increment-if-matches "A" SC-A1) SC-A2)
(check-expect (increment-if-matches "A" SC-A2) SC-A3)
(check-expect (increment-if-matches "A" SC-B1) SC-B1)
(check-expect (increment-if-matches "B" SC-C1) SC-C1)
  
(define (sc-w sc)
  (make-sc (sc-str sc)
           (sc-count sc)))


(define (increment-if-matches st sc)                  
  (make-sc (sc-str sc)                
           (cond [(string=?
                   (string-upcase st) (sc-str sc))
                  (+ (sc-count sc) 1)]
                 [else (sc-count sc)])))
      
  
  


