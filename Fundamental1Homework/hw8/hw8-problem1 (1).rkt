;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw8-problem1 (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; IMPORTANT:
; 1. The functions that you design for this problem *must* use the ISL list
;    abstraction(s); you MAY NOT use recursion: doing so will lead you to get
;    no code credit for the function :(
;
; 2. The planning part of this problem is in the SAME file used for ALL parts
;    of Homework 8.


; Recall Homework 6, where you designed a function to help bookstores - you
; are now going to solve the same problem, but this time using ISL list
; abstractions.

; As before, assume that bookstores keep all authors whose last name starts
; with the same letter on the same shelf, and those shelves are labeled with
; that letter. A record of which authors are on a given shelf would be
; represented using the following data definitions:


(define-struct shelf [letter authors])

; A Shelf is a (make-shelf 1String [List-of String])
; Interpretation: a record of the letter the authors' last name *should* start
; with, and the list of the *actual* last names on the shelf.

(define SHELF-1 (make-shelf "A" (list "Austen" "Hurston" "Angelou" "Butler" "Alvarez")))
(define SHELF-2 (make-shelf "B" (list)))
(define SHELF-3 (make-shelf "C" (list "Carle" "Coates")))

(define (shelf-temp s)
  (... (shelf-letter s) ...
       (list-of-string-temp (shelf-authors s)) ...))


; As before, your task will be to design the function fix-shelves, which takes
; a list of Shelf records and produces a list of Shelf records where at least
; one author does not belong on the Shelf. The output Shelf records should
; only contain the authors who don't belong on that shelf. Shelf records and
; the authors within those records should be in the same order in the output
; as they appear in the input. Do not generate empty Shelf records; this
; generates needlessly long reports, which annoys the employees.


; TODO 1/2: Plan your solution, using the planning interface described on the
;           canvas page for this assignment. ALL PLANNING FOR THIS HW WILL BE
;           DONE IN THE SAME PLACE, AND SUBMITTED TOGETHER.

; TODO 2/2: Design the function fix-shelves using the ISL list abstractions.
;           You have been supplied a test for clarity (which you can use in
;           your design, but should supplement). YOUR CODE SHOULD NOT USE ANY
;           RECURSION.


; fix-shelves : [List-of Shelves] -> [List-of Shelves]
; By taking in the list of shelves, it will return a new list of shelves,
; exposing the ones that shouldn't be in the list
(define (fix-shelves los)
  (local [; bad-author: 1String String -> Boolean
          ; checks if the name in the specific one shelf is considered a "bad author"(not in the list)
          (define (bad-author lofShelf name)
            (not (string=? lofShelf (substring name 0 1))))
          ; fix-one-shelf: Shelf -> Shelf
          ; Intakes one shelf and fixes a shelf, one by one, comparing the author (bad-author)
          ; to the list in the shelf
          (define (fix-one-shelf shelf)
            (make-shelf (shelf-letter shelf)
                        (filter (λ (name) (bad-author (shelf-letter shelf) name))
                                (shelf-authors shelf))))]
    (filter (λ (shelf) (not (empty? (shelf-authors shelf)))) (map fix-one-shelf los))))


; Shelf -> Shelf
;(define (fix-one-shelf shelf)
; (filter bad-author (shelf-letter shelf) 
; )

;(define (fix-one-shelf shelf)
;  (make-shelf (shelf-letter shelf)
;       (filter (λ (name) (bad-author (shelf-letter shelf) name)) (shelf-authors shelf))))


  
; Authorname letterofShelf
;(define (bad-author lofShelf name)
;  (not(string=? lofShelf (substring name 0 1))))




(check-expect (fix-shelves (list SHELF-1 SHELF-2 SHELF-3))
              (list (make-shelf "A" (list "Hurston" "Butler"))))


(check-expect (fix-shelves (list SHELF-2 SHELF-1 SHELF-3))
              (list (make-shelf "A" (list "Hurston" "Butler"))))



(check-expect (fix-shelves (list))
              '())


(check-expect (fix-shelves (list SHELF-2 SHELF-3))
              '())