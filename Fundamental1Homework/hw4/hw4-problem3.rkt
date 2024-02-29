;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now let's think about what goes into making an app that works with purchase
; receipts.
;
; Consider the following data definition:


(define-struct item [desc qty unit sale? next])

; A ReceiptItem is one of:
; - "nothing"
; - (make-item String Nat PosReal Boolean ReceiptItem)
; Intepretation: either end of the receipt or
; an item's description, quantity purchased,
; unit price (in $), whether it was on sale,
; and the next item on the receipt


; TODO 1/4: Complete the data design recipe for ReceiptItem.
;           You *must* have examples that (at least) represent the following
;           three receipts...
;           - An empty receipt
;           - A grocery receipt...
;             (1 box of cereal, $4.28),
;             (2 apples on sale, $1.67 each)
;           - A computer invoice...
;             (2 RaspberryPi on sale, $32 each),
;             (1 monitor, $135),
;             (2 wireless touch keyboards, $27 each)

(define EMPTY-RECEIPT "nothing")

(define GROCERY-RECEIPT (make-item "cereal" 1 4.28 #false EMPTY-RECEIPT))
(define GROCERY-RECEIPT1 (make-item "apples" 2 1.67 #true GROCERY-RECEIPT))

(define COMPUTER-INVOICE (make-item "RaspberryPi" 2 32 #true EMPTY-RECEIPT))
(define COMPUTER-INVOICE1 (make-item "monitor" 1 135 #false COMPUTER-INVOICE))
(define COMPUTER-INVOICE2 (make-item  "wireless touch keyboards" 2 27 #false COMPUTER-INVOICE1))
(define COMPUTER-INVOICE3 (make-item "headset" 1 12 #false COMPUTER-INVOICE2))


;ReceiptItem -> Receipt (?)
(define (receipt-temp r)
  (...
   (cond
     [(string? r) ...]
     [(item? r)
      (item-qty r)
      (item-unit r)
      (item-sale? r)
      (receipt-temp (item-next r)) ...])))






; TODO 2/4: Design the function total-cost, which calculates the total cost
;           of a receipt. For instance, the empty receipt is 0; the grocery
;           is (1 x 4.28) + (2 x 1.67) = 7.62; and the computer receipt is
;           (2 x 32) + (1 x 135) + (2 x 27) = 253.


(check-expect (total-cost EMPTY-RECEIPT) 0)
(check-expect (total-cost GROCERY-RECEIPT1) 7.62)
(check-expect (total-cost COMPUTER-INVOICE) 64)
(check-expect (total-cost COMPUTER-INVOICE2) 253)


;total-cost: Receipt -> TotalCost
;calculates the total cost of a receipt

(define (total-cost r)
  (cond
    [(string? r) 0]
    [(item? r)
     (+ (* (item-qty r) (item-unit r))
        (total-cost (item-next r)))]))
    
    








; TODO 3/4: Design the function any-sale?, which determines if any item in the
;           receipt is on sale. For example, the empty receipt does not have
;           any sale items, but both other examples do.



;any-sale?: Receipt -> Boolean
;determines if any item in the receipt is on sale


(check-expect (any-sale? EMPTY-RECEIPT) #false)
(check-expect (any-sale? GROCERY-RECEIPT1) #true)
(check-expect (any-sale? COMPUTER-INVOICE) #true)
(check-expect (any-sale? COMPUTER-INVOICE2) #false)
(check-expect (any-sale? COMPUTER-INVOICE3) #false)


(define (any-sale? r)
  (cond
    [(string? r) #false]
    [(item? r)
     (item-sale? r)]))




; TODO 4/4: Design the function expensive, which produces a new receipt that only
;           contains items that are greater than $100 (unit cost). For example,
;           both the empty and grocery receipts would produce empty receipts,
;           whereas the computer receipt would produce a new list only containing
;           the monitor.

;expensive: ReceiptItem -> ReceiptItem (new)
;produces a new receipt that only contains items that are greater than $100 (unit cost)

(define (expensive r)
  (cond
    [(string? r) EMPTY-RECEIPT]
    [(item? r)
     (if (< 100 (item-unit r))
         (make-item (item-desc r) (item-qty r) (item-unit r) (item-sale? r) (expensive (item-next r)))
         (expensive (item-next r)))]))

(check-expect (expensive EMPTY-RECEIPT) EMPTY-RECEIPT)
(check-expect (expensive GROCERY-RECEIPT1) EMPTY-RECEIPT)
(check-expect (expensive COMPUTER-INVOICE) EMPTY-RECEIPT)
(check-expect (expensive COMPUTER-INVOICE1) (make-item "monitor" 1 135 #false EMPTY-RECEIPT))
(check-expect (expensive COMPUTER-INVOICE2) (make-item "monitor" 1 135 #false EMPTY-RECEIPT))
(check-expect (expensive COMPUTER-INVOICE3) (make-item "monitor" 1 135 #false EMPTY-RECEIPT))




;(define-struct item [desc qty unit sale? next])    
  

  

;(define (receipt-temp r)
;(...
;(cond
; [(string? r)...]
;[(item? r)
;(item-qty r)
;(item-unit r)
;(item-sale? r)
;(receipt-temp (item-next r))...])))


  
#|
(define (good-for-friday? sq)
  (cond
    [(boolean? sq) QUEUE-EMPTY]
    [(video? sq)
     (if
      (or (string=? (video-genre sq) GENRE-COMEDY)
          (string=? (video-genre sq) GENRE-ACTION))
      #true
      (good-for-friday? (video-next sq)))]))"
|#