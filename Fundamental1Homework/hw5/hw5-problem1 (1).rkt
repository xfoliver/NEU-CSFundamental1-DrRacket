;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw5-problem1 (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You are going to make yourself a useful interactive app: flash cards
; (https://en.wikipedia.org/wiki/Flashcard).

; To begin, consider the following data definition...


(define-struct flashcard [front back])

; A FlashCard is a (make-flashcard String String)
; Interpretation: the front and back of a card


; TODO 1/4: Complete the design recipe for FlashCard.
(define flashcard0 '())
(define flashcard1 (make-flashcard "person" "人"))
(define flashcard2 (make-flashcard "sky" "天"))
(define flashcard3 (make-flashcard "ground" "地"))

(define (FlshCard-temp f)
  (...
   (cond
     [(empty? f) ...]
     [(flashcard? f) ...
      (flashcard-front f) ...
      (flashcard-back f)])))


; Now a single flash card wouldn't be super useful, and so...

; TODO 2/4: Design ListOfFlashCard (LoFC) to support an arbitrarily sized
;           sequence of flash cards. Importantly...
;           - These should be proper lists (i.e., using cons and '()).
;           - Make sure to give yourself a few example lists, of different sizes;
;             hopefully they are useful in your classes!
;           - Remember that your LoFC template should reflect that your list
;             elements are themselves designed types (FlashCard).
; A ListOfFlashCard (LoFC) is one of:
;- '()
;- (cons String LoFC)
;interpretation: a list of flash cards

(define lofc1 '())
(define lofc2 (cons flashcard1 lofc1))
(define lofc3 (cons flashcard2 lofc2))
(define lofc4 (cons flashcard3 lofc3))


(define (list-temp l)
  (...
   (cond
     [(empty? l) ...]
     [(cons? l) ...
      (first l) ... 
      (list-temp (rest l)) ...])))



; Now, for practice...

; TODO 3/4: Design the function has-text?, which determines if a list of flash
;           cards contains any card that contains a supplied text.
;
;           Hint: the string-contains? function is very useful for determining
;           if one string contains another :)
; has-text? : ListOfFlashCard -> Boolean
; interpretation: determine if the text occurs in the lest

(check-expect (has-text? lofc1 "any") #false)
(check-expect (has-text? lofc2 "person") #true)
(check-expect (has-text? lofc3 "天") #true)
(check-expect (has-text? lofc4 "ground") #true)

(define (has-text? list text)
  (cond
    [(empty? list) #false]
    [(cons? list)
     (or
      (string-contains? text (text? (first list)))
      (has-text? (rest list) text))]))


;text? : FlashCard -> String
;interpretation: determin whether the flashcard has text

(check-expect (text? flashcard0) '())
(check-expect (text? flashcard1) "person 人")
(check-expect (text? flashcard2) "sky 天")
(check-expect (text? flashcard3) "ground 地")

(define (text? t)
  (cond
    [(empty? t) '()]
    [(flashcard? t)
     (string-append (flashcard-front t) " " (flashcard-back t))]))



; Finally, let's put this list to use :)

; TODO 4/4: Design the program go-cards, which helps you study with a supplied list
;           of cards. It starts on the first card and then flips it when a key is
;           pressed, and then goes to the front of the next card when another key is
;           pressed. The program should end when the last card has been flipped, and
;           the go-cards function should return how many cards were in the original
;           list. Some hints...
;           - To get you started, you have been supplied the data definition of a
;             way to represent the state of the program (don't forget to uncomment
;             the structure definition and finish the design recipe for data!).
;           - The return value of this function is a bit challenging, since the list
;             you get at the end is empty! So uncomment the code we've given you below,
;             but to understand: you can *add* the length of the originally supplied
;             list to that of the (empty) final list and still get the right answer :)
;           - Be sure to follow the templates for all your data, which will typically
;             entail helpers for the FS, the LoFC, and the FC.
;           - As long as the program operates as described, you are welcome to make it
;             look as simple or as creative as you would like - we hope it helps you
;             in your classes!! :)


(define-struct fs [cards front?])

; A FlashState (FS) is a (make-fs LoFC Boolean)
; Interpretation: a list of cards, and whether
; the front is face up


; go-cards : LoFC -> Nat
; displays the cards in sequence (flip via key),
; returning the number of cards
(define fs1 (make-fs lofc2 #true))
(define fs11 (make-fs lofc2 #false))
(define fs2 (make-fs lofc3 #true))
(define fs22 (make-fs lofc3 #false))
(define fs3 (make-fs lofc4 #true))
(define fs33 (make-fs lofc4 #false))
  
(define (FlashState-temp f)
  (... (list-temp (fs-cards f)) ...
       (fs-front? f) ...))


(define (go-cards lofc)
  (+
   (length lofc)
   (length (fs-cards
            (big-bang (make-fs lofc #t)
              [to-draw draw-fs]
              [on-key flip-fs]
              [stop-when done-fs?])))))

(define card (square 600 "solid" "white"))

;;draw-fs: flashstate -> Image
;;draw a flash card

(check-expect (draw-fs fs1) (overlay (text "person" 30 "black") card))
(check-expect (draw-fs fs11) (overlay (text "人" 30 "black") card))
(check-expect (draw-fs fs2) (overlay (text "sky" 30 "black") card))
(check-expect (draw-fs fs22) (overlay (text "天" 30 "black") card))
(check-expect (draw-fs fs3) (overlay (text "ground" 30 "black") card))
(check-expect (draw-fs fs33) (overlay (text "地" 30 "black") card))

(define (draw-fs df)
  (draw-fc (fs-cards df) (fs-front? df)))


;;draw-fc: listofflashcards boolean -> Image
;;draw front and back side of cards

(check-expect (draw-fc lofc1 #false) card)
(check-expect (draw-fc lofc2 #true) (overlay (text "person" 30 "black") card))
(check-expect (draw-fc lofc2 #false) (overlay (text "人" 30 "black") card))
(check-expect (draw-fc lofc3 #true) (overlay (text "sky" 30 "black") card))
(check-expect (draw-fc lofc3 #false) (overlay (text "天" 30 "black") card))
(check-expect (draw-fc lofc4 #true) (overlay (text "ground" 30 "black") card))
(check-expect (draw-fc lofc4 #false) (overlay (text "地" 30 "black") card))

(define (draw-fc lofc c)
  (cond
    [(empty? lofc) card]
    [(cons? lofc)
     (if c
         (draw-front (first lofc))
         (draw-back (first lofc)))]))

;;draw-front: FlashCard -> Image
;;draw front side of the card

(check-expect (draw-front flashcard1) (overlay (text "person" 30 "black") card))
(check-expect (draw-front flashcard2) (overlay (text "sky" 30 "black") card))

(define (draw-front f)
  (overlay
   (text (flashcard-front f) 30 "black") card))

;;draw-back: FlashCard -> Image
;;draw back side of the card

(check-expect (draw-back flashcard1) (overlay (text "人" 30 "black") card))
(check-expect (draw-back flashcard2) (overlay (text "天" 30 "black") card))


(define (draw-back b)
  (overlay
   (text (flashcard-back b) 30 "black") card))

;;flip-fs: FlashState KeyEvent -> FlashState
;;when key is pressed, the card flip

(check-expect (flip-fs fs2 "c") fs22)
(check-expect (flip-fs fs3 "c") fs33)

(define (flip-fs fs ke)
  (if
   (fs-front? fs)
   (make-fs (fs-cards fs) (not (fs-front? fs)))
   (make-fs (change-card (fs-cards fs)) (not (fs-front? fs)))))

(define (change-card lofc)
  (cond
    [(empty? lofc) lofc]
    [(cons? lofc) (rest lofc)]))

;;done-fs?: FlashState -> Boolean
;;stop when the last card has been flipped

(check-expect (done-fs? fs33) #false)

(define (done-fs? fs)
  (empty? (fs-cards fs)))
  
