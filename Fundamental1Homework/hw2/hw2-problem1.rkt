;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/1: Design the function valid-5mc? that determines if a supplied
;           1-character string (a 1String) is a valid response to a 5-option
;           multiple-choice question, either upper- or lower-case. So "A",
;           "c" and "E" are all valid (and so the function should return #true;
;           whereas "f", "7", "?", and "Z" are all invalid (and so the function
;           should return #false).
;
;           Be sure you follow all steps of the design recipe and include
;           check-expects that cover both result values, as well as making sure
;           upper- and lower-case examples work properly.
;
;           Importantly, you should NOT use code that follows the pattern...
;
;              (if expression #true #false)
;
;           since this could be replaced with just expression


;;valid-5mc: 1String -> boolean
;;is 1-character string valid?

(check-expect (valid-5mc? "A") #true)
(check-expect (valid-5mc? "c") #true)
(check-expect (valid-5mc? "E") #true)
(check-expect (valid-5mc? "f") #false)
(check-expect (valid-5mc? "7") #false)
(check-expect (valid-5mc? "?") #false)
(check-expect (valid-5mc? "Z") #false)

(define (valid-5mc? 1string)
  (cond [(string=? 1string "A") #true]
        [(string=? 1string "a") #true]
        [(string=? 1string "B") #true]
        [(string=? 1string "b") #true]
        [(string=? 1string "C") #true]
        [(string=? 1string "c") #true]
        [(string=? 1string "D") #true]
        [(string=? 1string "d") #true]
        [(string=? 1string "E") #true]
        [(string=? 1string "e") #true]
        [else #false]))
        
  
  




