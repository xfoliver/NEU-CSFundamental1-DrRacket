;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab8-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Computing education literature shows that planning before writing code leads
; to better programs. We have therefore created an experimental interface for
; you to express your program plans. We ask you to use it to plan out your
; solution to this problem, and all problems in Homework 8.

; TODO 1/3: Read the description of the planning interface on Canvas, which is
;           linked from both the Lab 8 and Homework 8 assignments.

; Now, to practice using this tool, your task is to design the function
; palindrome?, which takes a string and determines whether it is a
; palindrome (a string that has the same letters in each of forward and reverse
; order, ignoring capitalization). For clarity, here is the intended signature:

; palindrome? : String -> Boolean

; Notes:
; - You should NOT use the built-in ISL function reverse, but you are welcome to
;   define your own as a helper.
; - You are encouraged to use the built-in explode function, which takes a
;   string and produces a list of its characters (each represented as a
;   1String). The implode function then reverses this process.
; - You *must* use the ISL list abstraction(s); you MAY NOT use recursion.


; TODO 2/3: Plan your solution, using the planning interface described on the
;           canvas page for this assignment.

; TODO 3/3: Design the function palindrome? using the ISL list abstractions.
;           YOUR CODE SHOULD NOT USE ANY RECURSION.


(check-expect (palindrome? "Racecar") #true)

(check-expect (palindrome? "Fundies") #false)

(check-expect (palindrome? "") #true)

(define (palindrome? string)
  (local [(define LOWERCASE-STR (string-downcase string))
          (define REVERSED-STR (foldl string-append "" (explode LOWERCASE-STR)))]
    (string=? LOWERCASE-STR REVERSED-STR))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recall that in Homework 6 we were trying to design a Wordle function to
; visualize a letter on a background associated with its status. Here is part
; of the sample solution to that problem (with a slight update given ISL+)...


; A LetterStatus (LS) is one of:
; - "wrong"
; - "misplaced"
; - "right"
; Interpretation: status of a guessed letter

(define LS-WRONG "wrong")
(define LS-MISPLACED "misplaced")
(define LS-RIGHT "right")

; ls->color : LS -> Color
; produces a background color associated with a letter status

(check-expect (ls->color LS-WRONG) "dimgray")
(check-expect (ls->color LS-MISPLACED) "goldenrod")
(check-expect (ls->color LS-RIGHT) "darkgreen")

(define (ls->color ls)
  (cond
    [(string=? ls LS-WRONG) "dimgray"]
    [(string=? ls LS-MISPLACED) "goldenrod"]
    [(string=? ls LS-RIGHT) "darkgreen"]))

; ls->color-fn : LetterStatus -> [NonNegReal -> Image]
; associates a status with a function to produce a background color

; NOTE: ISL+ lets you write easier tests than in HW6 :)
(check-expect ((ls->color-fn LS-WRONG) 50) (square 50 "solid" "dimgray"))
(check-expect ((ls->color-fn LS-MISPLACED) 50) (square 50 "solid" "goldenrod"))
(check-expect ((ls->color-fn LS-RIGHT) 50) (square 50 "solid" "darkgreen"))

(define (ls->color-fn ls)
  (cond
    [(string=? ls LS-WRONG) wrong-bg]
    [(string=? ls LS-MISPLACED) misplaced-bg]
    [(string=? ls LS-RIGHT) right-bg]))

; wrong-bg : NonNegReal -> Image
; produces a sized background for wrong guesses

(check-expect (wrong-bg 50) (square 50 "solid" "dimgray"))
(check-expect (wrong-bg 100) (square 100 "solid" "dimgray"))

(define (wrong-bg size)
  (square size "solid" (ls->color LS-WRONG)))

; misplaced-bg : NonNegReal -> Image
; produces a sized background for misplaced guesses

(check-expect (misplaced-bg 50) (square 50 "solid" "goldenrod"))
(check-expect (misplaced-bg 100) (square 100 "solid" "goldenrod"))

(define (misplaced-bg size)
  (square size "solid" (ls->color LS-MISPLACED)))

; right-bg : NonNegReal -> Image
; produces a sized background for right guesses

(check-expect (right-bg 50) (square 50 "solid" "darkgreen"))
(check-expect (right-bg 100) (square 100 "solid" "darkgreen"))

(define (right-bg size)
  (square size "solid" (ls->color LS-RIGHT)))


; Do you remember feeling really strange writing three functions that feel SO
; similar!? Well now with your lambda super-powers, you can do better :)


; TODO 1/1: Provide new code for the ls->color-fn function that utilizes lambda
;           in order to abstract the three -bg functions. If you are successful,
;           you will be able to delete all three functions, all tests will still
;           pass, AND you will have a much better design overall :)


; ls->color-fn/improved : LS -> [NonNegReal -> Image]
; associates a ststus with a function to produce a background

(check-expect ((ls->color-fn/improved LS-WRONG) 50) (square 50 "solid" "dimgray"))
(check-expect ((ls->color-fn/improved LS-MISPLACED) 50) (square 50 "solid" "goldenrod"))
(check-expect ((ls->color-fn/improved LS-RIGHT) 50) (square 50 "solid" "darkgreen"))

(define (ls->color-fn/improved ls)
  (λ (size) (square size "solid" (ls->color ls))))



;(define (ls->color-fn ls)
;  (cond
;    [(string=? ls LS-WRONG) wrong-bg]
;    [(string=? ls LS-MISPLACED) misplaced-bg]
;    [(string=? ls LS-RIGHT) right-bg]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recall that in Homework 7 we were trying to design ways to stack a list of
; images vertically and horizontally, with a bit of buffer between each image.
; Here is part of the sample solution to that problem...


(define GAP (square 5 "solid" "white"))

; stack/v : [List-of Images] -> Image
; space-separates a list of images vertically

(check-expect
 (stack/v '())
 GAP)

(check-expect
 (stack/v
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (above
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

(define (stack/v images)
  (foldr
   stack-up
   GAP
   images))

; stack-up : Image Image -> Image
; puts the supplied image atop the supplied background with a gap

(check-expect (stack-up (text "C" 50 "black") GAP)
              (above GAP (text "C" 50 "black") GAP))

(check-expect (stack-up (text "B" 50 "black")
                        (above GAP (text "C" 50 "black") GAP))
              (above GAP
                     (text "B" 50 "black")
                     (above GAP (text "C" 50 "black") GAP)))

(define (stack-up i bg)
  (above GAP i bg))

; stack/h : [List-of Image] -> Image
; space-separates a list of images horizontally

(check-expect
 (stack/h '())
 GAP)

(check-expect
 (stack/h
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (beside
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

(define (stack/h images)
  (foldr
   stack-across
   GAP
   images))

; stack-across : Image Image -> Image
; puts the supplied image beside the supplied background with a gap

(check-expect (stack-across (text "C" 50 "black") GAP)
              (beside GAP (text "C" 50 "black") GAP))

(check-expect (stack-across (text "B" 50 "black")
                            (beside GAP (text "C" 50 "black") GAP))
              (beside GAP
                      (text "B" 50 "black")
                      (beside GAP (text "C" 50 "black") GAP)))

(define (stack-across i bg)
  (beside GAP i bg))


; Notice again how *similar* stack/v and stack/h ended up being!  Well now with
; your local super-powers, you can do better :)


; TODO 1/1: Design the abstraction stack, which utilizes local in order to
;           abstract the functionality of the stack-across/stack-up functions,
;           and then use stack to replace the code in stack/h and stack/v. If
;           you are successful, you will be able to delete the stack-across and
;           stack-up functions, all tests will still pass, AND you will have a
;           much better design overall :)


;        similar           difference
; stack: [List-of Images]  [Image Image -> Image] -> Image
; space-separates a list of images using the inputed orientation function

(check-expect
 (stack '() beside)
 GAP)

(check-expect
 (stack
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black"))
  beside)
 (beside
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))
;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-expect
 (stack '() above)
 GAP)

(check-expect
 (stack
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black"))
  above)
 (above
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))




(define (stack images func)
  (local [; buffer : Image Image -> Image
          ; puts the supplied image (beside/above) the supplied background with a gap
          (define (buffer i bg)
            (func GAP i bg))]
    (foldr buffer GAP images)))




(check-expect
 (stack/h-improved '())
 GAP)

(check-expect
 (stack/h-improved
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (beside
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

;stack/h-improved: [List-ofImage] -> Images
; crestes list of images horizontally
(define (stack/h-improved images)
  (stack images beside))


(check-expect
 (stack/v-improved '())
 GAP)

(check-expect
 (stack/v-improved
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (above
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

;stack/v-improved: [List-ofImage] -> Images
; crestes list of images vertically
(define (stack/v-improved images)
  (stack images above))

