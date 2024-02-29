;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/2: Finish designing the function countdown that takes a natural number
;           and a non-empty list of strings and produces a countdown message.
;
;           Note: you are NOT allowed to use list abstractions for this
;                 function; doing so will result in ZERO code credit :(


; countdown : Nat [NEList-of String] -> String
; produces a message of counting down the numbers and then
; listing out the messages


(check-expect (countdown 3 (list "go")) "3!2!1!go!")
(check-expect (countdown 0 (list "howdy")) "howdy!")
(check-expect (countdown 2 (list "check" "expect")) "2!1!check!expect!")


(define (countdown number los)
  (cond
    [(= number 0) (countdown-helper los)]
    [(> number 0)
     (string-append (exclamation (number->string number)) (countdown (- number 1) los))]))
    
;    [(cons? los)
;     (number? number)
;     (countdown-helper number los)]))


(define (countdown-helper los)
  (cond
    [(empty? (rest los)) (exclamation (first los))]
    [(cons? (rest los))
     (string-append (exclamation (first los)) (countdown-helper (rest los)))]))

;exclamation: String -> String
; helper function that includes an exclamtion point to each part after each string
(define (exclamation string)
  (string-append string "!"))


; TODO 2/2: Finish designing the function lists-same? that determines if two
;           supplied lists have the "same" contents, as determined by a supplied
;           predicate.
;
;           Note: you are NOT allowed to use list abstractions for this
;                 function; doing so will result in ZERO code credit :(


; lists-same? : (X Y) [List-of X] [List-of Y] [X Y -> Boolean] -> Boolean
; determines if the two lists are the "same" based upon the predicate


(check-expect (lists-same? '() '() =) #t)
(check-expect (lists-same? '() (list 1) =) #f)
(check-expect (lists-same? (list 1 2) '() =) #f)

(check-expect (lists-same? (list 1) (list 1) =) #t)
(check-expect (lists-same? (list 1) (list 100) =) #f)
(check-expect (lists-same? (list 1 100) (list 1 2) =) #f)
(check-expect (lists-same? (list "a" "b" "c") (list "a" "b" "c") string=?) #t)
(check-expect (lists-same? (list "a" "b" "c") (list "c" "b" "a") string=?) #f)
(check-expect (lists-same? (list "a" "b" "c") (list "a" "b" "a") string=?) #f)

(check-expect (lists-same? (list 1 2 3) (list "1" "2" "3")
                           (λ (x y) (string=? (number->string x) y))) #t)

(check-expect (lists-same? (list "howdy" "world") (list "Howdy" "WORLD")
                           (λ (x y) (string=? (string-upcase x)
                                              (string-upcase y)))) #t)

(define (lists-same? list1 list2 equation)
  (cond 
    [(and (empty? list1) (empty? list2)) #true]
    [(and (cons? list1) (cons? list2))
     (and (equation (first list1) (first list2))
          (lists-same? (rest list1) (rest list2) equation))]
    [(and (empty? list1) (cons? list2)) #false]
    [(and (cons? list1) (empty? list2)) #false]))






