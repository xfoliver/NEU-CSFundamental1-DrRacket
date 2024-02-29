;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following data definitions & interpretations...

(define-struct address [num st city us-state zip])

; An Address is a (make-address Nat String String String Nat)
; - num is the number of the building on the street
; - st is the name of the street
; - city is the city the building is in
; - us-state is the state the city is in
; - zip is the zipcode of the building
; Interpretation: a US address

(define-struct student [first last nuid local perm])

; An NUStudent is a (make-student String String PositiveNumber Address Address)
; - first is the student's first name
; - last is the student's last name
; - nuid is the student's NUID
; - local is the student's local address
; - perm is the student's permanent address
; Interpretation: a Northeastern student


; TODO 1/3: complete the data design recipe for Address and NUStudent

;;example
(define Address-1 (make-address 50 "Tremont" "Boston" "MA" 02120))
(define Address-2 (make-address 70 "Huilongguan" "Beijing" "Beijing" 100000))


;;template
(define (make-address-temp ua)
  (... (make-address-buildingNum ua) ...
       (make-address-streetName ua)
       (make-address-cityName ua) ...
       (make-address-us-state ua) ...
       (make-address-zipNum ua) ...
       ))

;;example
(define NS-1 (make-student "Zhehao" "Zhang" 002175250 Address-1 Address-2))

;;template
(define (NUStudent ns)
  (... (student-firstname ns)...
       (student-lastname ns)...
       (student-NUID ns)...
       (student-location ns)...
       (student-perm ns)...))


; TODO 2/3: Design the function student-email which takes an NUStudent and
;           produces a string representing that student’s email address.
;           For simplicity we will say that a student’s email address is always
;           their last name (all lowercase),  followed by a period, followed
;           by the first initial of their first name (also lowercase; you can
;           assume this exists), and finished with "@northeastern.edu".


; student-email : NUStudent -> String
; Take a Northeastern University Student and get their email address

(check-expect (student-email NS-1) "zhang.z@northeastern.edu")

(define (student-email student)
  (string-append
   (string-downcase (student-last student))
   "."
   (string-downcase (substring (student-first student) 0 1))
   "@northeastern.edu"))



; TODO 3/3: Design the function update-perm-zipcode which takes an NUStudent
;           and a natural number, representing the new zip code of the person,
;           and updates their permanent address to have that zip code.
;
;           Be sure to follow the template!

;;update-perm-zipcode: NUStudent-> String
;;update NUStudent's permanent address to have new zip code
(check-expect (update-perm-zipcode NS-1 12607)
              (make-student "Zhehao" "Zhang" 002175250 Address-1
                            (make-address 70 "Huilongguan" "Beijing" "Beijing" 12607)))

(define (NewPerm ua zip)
  (make-address (address-num ua)
                (address-st ua)
                (address-city ua)
                (address-us-state ua)
                zip))

(define (update-perm-zipcode student zip) 
  (make-student (student-first student)
                (student-last student)
                (student-nuid student)
                (student-local student)
                (NewPerm (student-perm student) zip)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note: to receive full credit, submit as much as you complete - you do NOT
;       have to finish all parts in lab.


; You are to design a program text-mover to display and manipulate text on a
; background. Your program should accept some phrase to show, as well as initial
; location and color (we only support three: red, black, or purple) - you should
; then display the phrase on the screen as described.

; When the user presses a mouse button, the program should move the text to the
; location that they clicked. When the user presses any key on the keyboard, the
; program should rotate colors.

; Here is our suggested plan for this program...

; 1. Design the text-mover function - think through the arguments to the
;    function, how you will represent the world state, and what handlers
;    you need to support.
;
;    - Hint A: since your state has multiple parts that change, you'll need a
;              structure to hold them, but the parts themselves might also be new.
;    - Hint B: you've been provided some data definitions below that will be quite
;              useful.

; 2. Finish designing the data from #1; think ahead to make examples that are
;    useful for testing such operations as changing location and color.

; 3. Design your to-draw handler, making use of the template(s) you 
;    designed in #2.

; 4. Design your remaining handler(s), again following the appropriate template(s).
;
;    - Hint #1: for the mouse, you'll want to respond only to the "button-up"
;               event, which you can check using the mouse=? function. Here's code
;               to get you started...

(define (mouse-handler state x y me)
  (if (mouse=? me "button-up")
      ...
      ...))

;    - Hint #2: make sure to follow your templates, which may involve breaking 
;               the handlers into helper functions.


; TODO 1/1: Design the text-mover World program!


; A Position is a (make-posn Real Real)
; Interpretation: a 2D location



; A RedBlackPurple (RBP) is one of:
; - "red"
; - "black"
; - "purple"
; Interpretation: available font colors

(define RBP-Red "red")
(define RBP-Black "black")
(define RBPPurple "purple")

(define (RedBlackPurple-temp rbp)
  (...
   (cond [(color=? RBP-Red rbp)...]
         [(color=? RBP-Red rbp)...]
         [(color=? RBP-Red rbp)...])))


(define-struct tm [str pos col])
; A TextMover (TM) is a (make-tm String Position RBP)
; - str is the text to be displayed
; - pos is the location of the text
; - col is the color of the text
; Interpretation: all the information needed for the text-mover program.
(define Fundies (text  "fundies" 80 "black"))
(define background(rectangle 400 300 "solid" "white"))
(define (text state)
  (big-bang state
    [to-draw draw-text]
    [on-mouse change-place]
    [on-key change-color]))
;;
;;
(check-expect (draw-text 100 50 "red")
              (place-image (text 100 50 "red") BG))
(define (text-state x-pos y-pos col)
  (place-image (text x-pos y-pos col) BG))
;;
;;
(check-expect (mouse-handler 100 30 "button-up") 200 40)
(define (mouse-handler state x y me)
  (if (mouse=? me "button-up" x y
               state))


  