;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now let's think about what goes into making a particularly useful app, Weather!
;
; Consider the following data definition:


(define-struct cloudy [morn? eve?])
(define-struct rain [chance])
(define-struct snow [inches])

; A Prediction is one of:
; - "sunny"
; - (make-cloudy Boolean Boolean)
; - (make-rain Nat[1, 100])
; - (make-snow Nat)
; Intepretation: weather prediction, either...
; - Sunny!
; - Cloudy (either in the morning, evening, both, or unsure)
; - Raining (with provided % chance as 1-100)
; - Snow (with provided accumulation)



; TODO 1/2: Complete the data design recipe for Prediction.


(define SUNNY "sunny")
(define MORNINGCLOUDY (make-cloudy #true #false))
(define EVENINGCLOUDY (make-cloudy #false #true))
(define BOTHCLOUDY (make-cloudy #true #true))
(define UNSURECLOUDY (make-cloudy #false #false))
(define RAINING (make-rain (random 100)))
(define SNOW (make-snow (random 48)))

(define (prediction-temp weather)
  (cond
    [(string? weather) ...]
    [(cloudy? weather) (make-cloudy (cloudy-morn? weather) (cloudy-eve? weather))]
    [(rain? weather) (make-rain (rain-chance weather))]
    [(snow? weather) (make-snow (snow-inches weather))]))








  



; TODO 2/2: Design the function announcement, which given
;           a prediction (e.g., "sunny"), produces a short
;           text announcement to display (e.g., "It's going
;           to be sunny!").

;           Some other example announcements include:
;           - "It's going to be cloudy in the morning."
;           - "There's a 60% chance of rain."
;           - "It's going to snow, with 2 inches on the ground."

;annoucement: Prediction -> Short Text






;(define-struct cloudy [morn? eve?])
;(define-struct rain [chance])
;(define-struct snow [inches])


;annoucement: Prediction -> Short Text
;given a prediction, it will produce a short text announcement to display




(define (announcement prediction)
  (cond
    [(string? prediction) "It's going to be sunny!"]
    [(cloudy? prediction)
     (cond
       [(and (cloudy-morn? prediction) (not (cloudy-eve? prediction)))
        "It's going to be cloudy in the morning."]
       [(and (not (cloudy-morn? prediction)) (cloudy-eve? prediction))
        "Its going to be cloudy in the evening."]
       [(and (cloudy-morn? prediction) (cloudy-eve? prediction))
        "Its going to be cloudy the whole day."]
       [(not (and (cloudy-morn? prediction) (cloudy-eve? prediction)))
        "It is unsure whether it will become cloudy or not."])]
    [(rain? prediction)
     (string-append "There's a " (number->string (rain-chance prediction)) "% chance of rain.")]
    [(snow? prediction)
     (string-append "It's going to snow, with " (number->string (snow-inches prediction))
                    " inches on the ground. ")]))


(check-expect (announcement SUNNY)
              "It's going to be sunny!")
(check-expect (announcement RAINING)
              (string-append "There's a " (number->string (rain-chance RAINING)) "% chance of rain."))
(check-expect (announcement MORNINGCLOUDY)
              "It's going to be cloudy in the morning.")
(check-expect (announcement EVENINGCLOUDY)
              "Its going to be cloudy in the evening.")
(check-expect (announcement BOTHCLOUDY)
              "Its going to be cloudy the whole day.")
(check-expect (announcement SNOW)
              (string-append "It's going to snow, with " (number->string (snow-inches SNOW))
                             " inches on the ground. "))





