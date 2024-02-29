;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following data definition...


(define-struct leaf [val])
(define-struct node [left right])

; A [BinTree-of X] is one of:
; - (make-leaf X)
; - (make-node [BinTree-of X] [BinTree-of X])
; Interpretation: a binary tree that holds values at the leaves


; and now the following abstraction...


; reduce-tree : (X Y) [Y Y -> Y] [X -> Y] [BinTree-of X] -> Y
; "folds" a binary tree (similar to how foldr "folds" a list)

(define (reduce-tree node-func leaf-func bt)
  (cond
    [(leaf? bt) (leaf-func (leaf-val bt))]
    [(node? bt)
     (node-func (reduce-tree node-func leaf-func (node-left bt))
                (reduce-tree node-func leaf-func (node-right bt)))]))


; In the remainder of this problem, you will be using this abstraction to
; implement other binary tree functions. In each case, you will be provided a
; purpose and tests, and must then produce a corresponding signature and code.
;
; Note: the code for each function must be ENTIRELY created using a single call
;       to reduce-tree.

; For example, consider the following purpose and tests...


; adds up all the leaves in a binary tree of numbers

(check-expect (add-all (make-leaf 5)) 5)
(check-expect (add-all (make-node (make-node (make-leaf 3)
                                             (make-leaf 10))
                                  (make-leaf 5))) 18)


; an appropriate response would be...


; add-all : [BinTree-of Number] -> Number

(define (add-all bton)
  (reduce-tree + identity bton))


; TODO 1/3: As described above, provide a signature and code (making just a
;           single call to reduce-tree) for the function max-length.

;max-length: [BinTree-of X] -> Number
; finds the length of the longest string in a binary tree of strings

(check-expect (max-length (make-leaf "")) 0)
(check-expect (max-length (make-node (make-node (make-leaf "howdy")
                                                (make-leaf "world"))
                                     (make-node (make-leaf "isl+")
                                                (make-leaf "function")))) 8)

(define (max-length bton)
  (reduce-tree max string-length bton))
              

; TODO 2/3: As described above, provide a signature and code (making just a
;           single call to reduce-tree) for the function flatten.

;flatten: [BinTree-of X] -> [List-of X]
; makes a list of all the leaf values in a tree


(check-expect (flatten (make-leaf "howdy")) (list "howdy"))
(check-expect (flatten (make-node (make-node (make-leaf 3)
                                             (make-leaf 10))
                                  (make-leaf 5)))
              (list 3 10 5))



(define (flatten leafVal)
  (reduce-tree append list leafVal)) ;adds all lists with 'append'

; TODO 3/3: As described above, provide a signature and code (making just a
;           single call to reduce-tree) for the function map-tree.


;map-tree: [BinTree-of Number] [X->Y] -> (New [BinTree-of Number])
; produces a new binary tree by applying a supplied function to the
; value of every leaf node


(check-expect (map-tree (make-leaf "5") string->number)
              (make-leaf 5))

(check-expect (map-tree (make-node (make-node (make-leaf 3)
                                              (make-leaf 10))
                                   (make-leaf 5))
                        even?)
              (make-node (make-node (make-leaf #f)
                                    (make-leaf #t))
                         (make-leaf #f)))


(define (map-tree bton func)
  (reduce-tree make-node (λ (x) (make-leaf (func x))) bton))
