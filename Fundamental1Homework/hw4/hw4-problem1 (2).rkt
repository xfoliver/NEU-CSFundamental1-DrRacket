;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw4-problem1 (2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's think about what goes into designing notifications for a mobile device.
;
; Consider the following data definitions...


(define-struct info [app message])

; An InfoMessage is a (make-info String String)
; Interpretation: a message from an app

(define-struct badge [app num])

; A Badge is a (make-badge String Nat)
; Interpretation: a numeric indicator for an app

(define-struct confirm [app yestxt notxt])

; A Confirmation is a (make-confirm String String String)
; Interpretation: a yes/no question from an app, with
; associated text to display for each option


; TODO 1/2: Complete the design recipe for InfoMessage,
;           Badge, and Confirmation. You should come up
;           with reasonable examples, but are welcome
;           to be creative :)



(define message (make-info "iMessage" "Hey, whats up"))
(define email (make-info "Gmail" "Hello sir"))
(define GIF (make-info "GIPHY" "click on 'Giphy' to see inserted gif"))
(define emoji (make-info "BeReal" "Take your BeReal photo"))

;process-infomessage: info -> info
(define (infomess-temp info)
  (... (infomess-app info)
       (infomess-message info)))


  
  
(define tikTok (make-badge "TikTok" 1))
(define facebook (make-info "Facebook" 2))
(define twitter (make-info "Twitter" 3))
(define instagram (make-info "Instagram" 4))

;process-thebadge: badge -> badge
(define (badge-temp bd)
  (... (badge-app bd)
       (badge-num bd) ...))



  
(define message-c (make-confirm "iMessage" "yes i can make it" "no i cant make it"))
(define email-c (make-confirm "Gmail" "yes i can help" "no i cannot help"))
(define GIF-c (make-confirm "GIPHY" "See gif? yes" "See gif? no"))
(define emoji-c (make-confirm "BeReal" "Take photo? yes" "Take photo? no"))

;process-theconfirm: confirm -> confirm
(define (confirm-temp c)
  (... (confirm-app c)
       (confirm-yestxt c)
       (confirm-notxt c)))

;All definitions and interpreations are listed above
  

  


  





; TODO 2/2: Design the data type Notification, which represents
;           a single notification that could be of any of the
;           types described above.



; A Notification is one of the following:

; - info (message)
; - badge (badge number)
; - confirm (yes/no)


;Interpretation: represents a single notification that could be any type

(define NOTI-1 message)
(define NOTI-2 email)
(define NOTI-3 GIF)
(define NOTI-4 tikTok)


(define (notification-temp item)

  (cond
    [(info? item) (infomess-temp item)]
    [(badge? item) (badge-temp item)]
    [(confirm? item) (confirm-temp item)]))
  




