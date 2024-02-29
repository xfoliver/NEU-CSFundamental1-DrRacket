;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recently a new website (https://neal.fun/design-the-next-iphone/) was made
; to allow you to design the next iPhone by mixing & matching features.
; Let's make a simple version of this as a World program :)

; TODO 1/3: Design the data PhoneModel that allows you to represent a set
;           of (at least) four enumerated models. You are welcome to be
;           creative, but an example set of options could be "Small",
;           "Regular", "Max", or "Fold"


; A PhoneModel is one of:
; - "Small"
; - "Regular"
; - "Max"
; - "Fold"
; Interpretation:phones' size model

(define PM-SMALL "Small")
(define PM-REGULAR "Regular")
(define PM-MAX "Max")
(define PM-FOLD "Fold")

; pm-temp : PhoneModel -> ?
(define (pm-temp pm)
  (... (cond [(string=? pm PM-SMALL) ...]
             [(string=? pm PM-REGULAR) ...]
             [(string=? pm PM-MAX) ...]
             [(string=? pm PM-FOLD) ...]) ...))


; TODO 2/3: Now design the data PhoneCompany that allows you to represent a set
;           of (at least) three enumerated company names, such as "Apple",
;           "Google", "Amazon".


; A PhoneCompany is one of:
; - "Apple"
; - "Google"
; - "Amazon"
; Interpretaion:the name of phone companys

(define PC-APPLE "Apple")
(define PC-GOOGLE "Google")
(define PC-AMAZON "Amazon")

; pc-temp : PhoneCompany -> ?
(define (pc-temp pc)
  (... (cond [(string=? pc PC-APPLE) ...]
             [(string=? pc PC-GOOGLE) ...]
             [(string=? pc PC-AMAZON) ...] ...)))


; TODO 3/3: Now design a World program that allows someone to interactively see all
;           combinations of models and companies.
;
;           Notes:
;           - You will need to design data to represent the current combination of
;             model/company, which should be a structure. Your program can accept
;             the starting value and should return the last when the window is closed.
;           - When the "m" keyboard key is pressed, you should proceed to the next
;             model (e.g., "Small" -> "Regular" -> "Max" -> "Fold" -> "Small"...);
;             the "c" key should similarly work for companies.
;           - Your drawing function should overlay some visualization of the company
;             (e.g., could be just the text, or a pretty logo) on top of a
;             visualization of the model (e.g., could differ in size, camera, ...).
;           - Be sure to follow the templates, which will guide you as to when to
;             create data-specific helper functions!!

  
(define-struct combin [model company])

; A Combination (C) is a (make-combination PhoneModel PhoneCompany)
; Interpretation:
; - PhoneModel is model of phone 
; - PhoneCompany is company of phone

(define Combin-1 (make-combin "Small" "Apple"))
(define Combin-2 (make-combin "Regular" "Google"))
(define Combin-3 (make-combin "Max" "Amazon"))
(define Combin-4 (make-combin "Fold" "Apple"))
(define Combin-5 (make-combin "Regular" "Apple"))
(define Combin-6 (make-combin "Regular" "Amazon"))


(define (combin-temp cb)
  (... (pm-temp (combin-model cb)) ...
       (pc-temp (combin-company cb)) ...))


(define SMALL (rectangle 200 200 "solid" "black"))
(define REGULAR (rectangle 300 300 "solid" "black"))
(define MAX (rectangle 400 400 "solid" "black"))
(define FOLD (rectangle 500 500 "solid" "black"))

(define APPLE (text "Apple" 25 "white"))
(define GOOGLE (text "Google" 25 "white"))
(define AMAZON (text "Amazon" 25 "white"))


; draw-c : Combination -> Image
; Overlay company on top of the model

(define (draw-c c)
  (overlay
   (cond [(string=? (combin-company c) PC-APPLE) APPLE]
         [(string=? (combin-company c) PC-GOOGLE) GOOGLE]
         [(string=? (combin-company c) PC-AMAZON) AMAZON])
   (cond [(string=? (combin-model c) PM-SMALL) SMALL]
         [(string=? (combin-model c) PM-REGULAR) REGULAR]
         [(string=? (combin-model c) PM-MAX) MAX]
         [(string=? (combin-model c) PM-FOLD) FOLD])))


; change-company : PhoneCompany -> PhoneCompany
; Proceeds to the next phone company

(check-expect (change-company PC-APPLE) PC-GOOGLE)
(check-expect (change-company PC-GOOGLE) PC-AMAZON)
(check-expect (change-company PC-AMAZON) PC-APPLE)

(define (change-company pc)
  (cond [(string=? pc PC-APPLE) PC-GOOGLE]
        [(string=? pc PC-GOOGLE) PC-AMAZON]
        [(string=? pc PC-AMAZON) PC-APPLE]))


; change-model : PhoneModel -> PhoneModel
; Proceeds to the next phone model

(check-expect (change-model PM-SMALL) PM-REGULAR)
(check-expect (change-model PM-REGULAR) PM-MAX)
(check-expect (change-model PM-MAX) PM-FOLD)
(check-expect (change-model PM-FOLD) PM-SMALL)

(define (change-model pm)
  (cond [(string=? pm PM-SMALL) PM-REGULAR]
        [(string=? pm PM-REGULAR) PM-MAX]
        [(string=? pm PM-MAX) PM-FOLD]
        [(string=? pm PM-FOLD) PM-SMALL]))



(check-expect (key-c Combin-1 "m") Combin-5)
(check-expect (key-c Combin-2 "c") Combin-6)

(define (key-c c k)
  (cond
    [(key=? k "c") (cond [(string=? (combin-company c) PC-APPLE)
                          (make-combin (combin-model c)
                                       (change-company PC-APPLE))]
                         [(string=? (combin-company c) PC-GOOGLE)
                          (make-combin (combin-model c)
                                       (change-company PC-GOOGLE))]
                         [(string=? (combin-company c) PC-AMAZON)
                          (make-combin (combin-model c)
                                       (change-company PC-AMAZON))])]
    [(key=? k "m") (cond [(string=? (combin-model c) PM-SMALL)
                          (make-combin (change-model PM-SMALL)
                                       (combin-company c))]
                         [(string=? (combin-model c) PM-REGULAR)
                          (make-combin (change-model PM-REGULAR)
                                       (combin-company c))]
                         [(string=? (combin-model c) PM-MAX)
                          (make-combin (change-model PM-MAX)
                                       (combin-company c))]
                         [(string=? (combin-model c) PM-FOLD)
                          (make-combin (change-model PM-FOLD)
                                       (combin-company c))])]))
               

(big-bang Combin-1
  [to-draw draw-c 500 500]
  [on-key key-c])