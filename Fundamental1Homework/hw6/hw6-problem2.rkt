;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; It's super useful to be able to answer the question: does a value appear
; in a list? But of course that question can be phrased multiple ways...

; TODO 1/3: Design two functions: string-in-list? and string-in-list-ci?.
;           The first returns #true if a supplied string appears
;           *exactly* in a list of strings, whereas the second returns
;           #true if a supplied string occurs in a list of strings if
;           we ignore lower/upper-case. You have been supplied some tests
;           for clarity (which you can use in your design, but should
;           supplement). Make sure your code follows the list template!


; string-in-list? : 1String [List-Of String] -> Boolean
; The first returns #true if a supplied string appears exactly in a list of strings

(define (string-in-list? 1string los)
  (cond [(empty? los) #false]
        [(cons? los)
         (or (string-contains? 1string (first los))
             (string-in-list? 1string (rest los)))]))


(check-expect (string-in-list? "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list? "A" (list "a" "b" "c")) #f)


; string-in-list-ci? : 1String [List-Of String] -> Boolean
; returns #true if a supplied string occurs in a list of strings if we ignore lower/upper-case

(define (string-in-list-ci? 1string los)
  (cond [(empty? los) #false]
        [(cons? los)
         (or (string-contains-ci? 1string (first los))
             (string-in-list-ci? 1string (rest los)))]))

(check-expect (string-in-list-ci? "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list-ci? "A" (list "a" "b" "c")) #t)




; TODO 2/3: Those two functions probably feel rather similar - so now
;           design the abstraction value-in-list? based on these two
;           functions.

;           Notes:
;           - Think through your signature to make sure it is as general
;             as possible, while still not making promises your abstraction
;             cannot keep!
;           - Don't forget to improve your implementations for the last
;             step! (Importantly: keep the old code by renaming the
;             functions string-in-list?/old and string-in-list-ci?/old;
;             you do not need to change/reproduce any parts of the function
;             design recipe for these old function implementations.)


; value-in-list: [List-Of X] [X -> Num] X -> Boolean
; abstracts the two functions defined above
; Allows a comparison between a supplied x and the list (recurion will occur for all x in list)

(check-expect (value-in-list? (list "a" "b" "d") string-contains? "n") #f)

(check-expect (value-in-list? (list "a" "b" "d") string-contains? "a") #t)

(check-expect (value-in-list? (list "a" "b" "d") string-contains-ci? "a") #t)

(define (value-in-list? los function x)
  (cond [(empty? los) #false]
        [(cons? los)                
         (or (function (first los) x) 
             (value-in-list? (rest los) function x))]))


; TODO 3/3: Now put your fancy new abstraction to good use! Design the function
;           anything-bigger? that determines if any of a list of numbers is
;           bigger than a supplied number. You have been supplied some tests
;           for clarity (which you can use in your design, but should supplement).

; anything-bigger? : Num [List-Of-Num] -> Boolean
; determines if any of a list of numbers is bigger than a supplied number.


(check-expect (anything-bigger? 5 (list 10 -1 3)) #t)
(check-expect (anything-bigger? 100 (list 10 -1 3)) #f)


(define (anything-bigger? num lon)
  (value-in-list? lon > num))
