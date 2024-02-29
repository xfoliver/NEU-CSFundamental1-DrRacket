;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In this problem you'll practice with lists by designing a function to
; help bookstores!

; Most bookstores sort the books on their shelves by the author’s last
; name. Unfortunately, some bookstore patrons do not preserve this order
; when browsing books, and simply place books back wherever they fit.

; Assume that bookstores keep all authors whose last name starts with the
; same letter on the same shelf, and those shelves are labeled with that
; letter. A record of which authors are on a given shelf would be represented
; using the following data definitions:

(define-struct shelf [letter authors])

; A Shelf is a (make-shelf 1String [List-of String])
; Interpretation: a record of the letter the authors' last name *should* start
; with, and the list of the *actual* last names on the shelf.

(define SHELF-1 (make-shelf "A" (list "Austen" "Hurston" "Angelou" "Butler" "Alvarez")))
(define SHELF-2 (make-shelf "B" (list)))
(define SHELF-3 (make-shelf "C" (list "Carle" "Coates")))
(define SHELF-4 (make-shelf "D" (list "Danny" "David" "Chris")))
(define SHELF-5 (make-shelf "E" (list)))
(define SHELF-6 (make-shelf "F" (list "Frank" "Finn")))

(define (shelf-temp s)
  (... (shelf-letter s) ...
       (list-of-string-temp (shelf-authors s)) ...))


; TODO 1/1: Design the function fix-shelves that takes a list of Shelf records
;           and produces a list of Shelf records where at least one author does
;           not belong on the Shelf. The output Shelf records should only contain
;           the authors who don't belong on that shelf. Shelf records and the authors
;           within those records should be in the same order in the output as they
;           appear in the input. Do not generate empty Shelf records; this generates
;           needlessly long reports, which annoys the employees. You have been
;           supplied a test for clarity (which you can use in your design, but
;           should supplement). Make sure your solution follows the (list) templates!

; fix-shelves: [List-Of Shelf] -> [List-Of Shelf]
; takes a list of Shelf records and produces a list of Shelf records where at least one
; author does not belong on the Shelf.

(check-expect (fix-shelves (list SHELF-1 SHELF-2 SHELF-3))
              (list (make-shelf "A" (list "Hurston" "Butler"))))

(check-expect (fix-shelves (list SHELF-4 SHELF-5 SHELF-6))
              (list (make-shelf "D" (list "Chris"))))
 
(define (fix-shelves l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (if (shelf-empty? (fix-first-shelf (first l)))
         (fix-shelves (rest l))
         (cons
          (fix-first-shelf (first l))
          (fix-shelves (rest l))))]))


; shelf-empty? : [List-Of Shelf] -> Boolean
; Checks if a shelf within the list is empty, will returnn true or false

(check-expect (shelf-empty? SHELF-1) #f)
(check-expect (shelf-empty? SHELF-2) #t)
(check-expect (shelf-empty? SHELF-3) #f)
(check-expect (shelf-empty? SHELF-4) #f)
(check-expect (shelf-empty? SHELF-5) #t)
(check-expect (shelf-empty? SHELF-6) #f)

(define (shelf-empty? a)
  (empty? (shelf-authors a)))

; fix-first-shelf : [List-Of Shelf] -> [List-Of Shelf]
; fixes the shelf (if it is not fixed, a make-shelf will occur, highlighting the ones that are wrong)
; (if it is fixed, it will return '())


(check-expect (fix-first-shelf SHELF-1) (make-shelf "A" (list "Hurston" "Butler")))
(check-expect (fix-first-shelf SHELF-2) (make-shelf "B" '()))
(check-expect (fix-first-shelf SHELF-3) (make-shelf "C" '()))
(check-expect (fix-first-shelf SHELF-4) (make-shelf "D" (list "Chris")))
(check-expect (fix-first-shelf SHELF-5) (make-shelf "E" '()))
(check-expect (fix-first-shelf SHELF-6) (make-shelf "F" '()))

(define (fix-first-shelf s)
  (make-shelf (shelf-letter s)
              (compare-letter (shelf-authors s) (shelf-letter s))))

   

; compare-letter: [List-of-Shelf] 1String -> [List-of-Strings]
; Inputting a supplied string and a list, it will compare the supplied string to the list of the
; first part of each string in list. If it is in the list, it will go on to the rest of the list.
; If not, it will extract it and create a new list with the ones that aren't supposed to be in the
; list


(check-expect (compare-letter (list) "A") (list))

(check-expect (compare-letter (list "Hurston" "Butler") "B") (list "Hurston"))

(define (compare-letter los shelf-letter)
  (cond
    [(empty? los) los]
    [(cons? los)
     (if (string=? shelf-letter (substring (first los) 0 1))
         (compare-letter (rest los) shelf-letter)

         (cons (first los) (compare-letter (rest los) shelf-letter)))]))







