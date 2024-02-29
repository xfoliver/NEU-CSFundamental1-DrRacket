;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw11-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Lab 11 involves designing a few functions on a particular representation of a
; graph - but as we saw in class, there are many ways to represent a graph. SO,
; in this homework you will actually work on the exact same problems, but with a
; different data representation. SO...

; TODO 0/4: If you haven't already, review the Lab 11 problems, sample solution,
;           and walkthrough video. (Note: Lab 11 will be released at the same
;           time as the Lab 10 sample solution, just after Lab 10 is due.)


; NOW, consider the following data definition:


(define-struct graph [v c e])
 
; A Graph is a (make-graph [List-of Nat] [Nat -> String] [Nat Nat -> Boolean])
; Interpretation: a graph!
; - v are the vertices (nodes) of the graph, each assumed to
;   be a unique natural number
; - c is a function that associates each node with a color
;   (the input to this function is assumed to be a valid vertex)
; - e is a function that returns true when the second vertex
;   supplied to the function is connected to the first
;   (both inputs are assumed to be valid vertices)

(define GRAPH-0
  (make-graph '()
              (λ (v) "")
              (λ (s d) #false)))

(define GRAPH-123
  (make-graph (list 1 2 3)
              (λ (v)
                (cond
                  [(= v 1) "purple"]
                  [(= v 2) "green"]
                  [(= v 3) "blue"]))
              (λ (s d)
                (cond
                  [(= s 1) (= d 2)]
                  [(= s 2) #false]
                  [(= s 3) #false]))))

(define GRAPH-789
  (make-graph (list 7 8 9)
              (λ (v)
                (cond
                  [(= v 7) "red"]
                  [(= v 8) "orange"]
                  [(= v 9) "orange"]))
              (λ (s d)
                (cond
                  [(= s 7) (= d 8)]
                  [(= s 8) (or (= d 7)
                               (= d 9))]
                  [(= s 9) #false]))))


; You should find the supplied examples to be representing the same data as
; those in Lab 11. If you don't understand this, ask!! :)

; Now you are going to finish designing the same functions as in Lab 11. You
; will find the sample solutions useful, but you'll need to figure out how
; to solve the same problems using this new data representation. As with Lab 11,
; you are encouraged to make appropriate use of list abstractions.


; TODO 1/4: Finish designing the function in-graph? that determines if a
;           supplied node id is in the supplied graph.

; in-graph? : Nat Graph -> Boolean
; is the supplied id in the graph?

(define (in-graph? n graph)
  (ormap (λ (a) (= n a)) (graph-v graph)))


(check-expect (in-graph? 1 GRAPH-0) #false)

(check-expect (in-graph? 1 GRAPH-123) #true)
(check-expect (in-graph? 2 GRAPH-123) #true)
(check-expect (in-graph? 3 GRAPH-123) #true)
(check-expect (in-graph? 4 GRAPH-123) #false)

(check-expect (in-graph? 1 GRAPH-789) #false)
(check-expect (in-graph? 7 GRAPH-789) #true)
(check-expect (in-graph? 8 GRAPH-789) #true)
(check-expect (in-graph? 9 GRAPH-789) #true)





; TODO 2/4: Finish designing the function change-color that takes two colors and
;           a supplied graph and changes the color of all nodes that have the
;           first color into the second color. (Note: because of the different
;           data representation, the tests had to be adapted, but they are
;           actually looking for the same behavior as Lab 11.)

; change-color : String String Graph -> Graph
; changes the color (color1 -> color2) of all nodes in the graph


(check-expect
 ((graph-c (change-color "purple" "green" GRAPH-123)) 1)
 "green")

(check-expect
 ((graph-c (change-color "purple" "green" GRAPH-123)) 2)
 "green")

(check-expect
 ((graph-c (change-color "purple" "green" GRAPH-123)) 3)
 "blue")

(check-expect
 (graph-v (change-color "purple" "green" GRAPH-123))
 (graph-v GRAPH-123))

(check-expect
 ((graph-e (change-color "purple" "green" GRAPH-123)) 1 1)
 ((graph-e GRAPH-123) 1 1))

(check-expect
 ((graph-e (change-color "purple" "green" GRAPH-123)) 1 2)
 ((graph-e GRAPH-123) 1 2))

(check-expect
 ((graph-e (change-color "purple" "green" GRAPH-123)) 1 3)
 ((graph-e GRAPH-123) 1 3))

(check-expect
 ((graph-e (change-color "purple" "green" GRAPH-123)) 2 1)
 ((graph-e GRAPH-123) 2 1))

(check-expect
 ((graph-e (change-color "purple" "green" GRAPH-123)) 2 2)
 ((graph-e GRAPH-123) 2 2))

(check-expect
 ((graph-e (change-color "purple" "green" GRAPH-123)) 2 3)
 ((graph-e GRAPH-123) 2 3))

(check-expect
 ((graph-e (change-color "purple" "green" GRAPH-123)) 3 1)
 ((graph-e GRAPH-123) 3 1))

(check-expect
 ((graph-e (change-color "purple" "green" GRAPH-123)) 3 2)
 ((graph-e GRAPH-123) 3 2))

(check-expect
 ((graph-e (change-color "purple" "green" GRAPH-123)) 3 3)
 ((graph-e GRAPH-123) 3 3))



(check-expect
 ((graph-c (change-color "orange" "red" GRAPH-789)) 7)
 "red")

(check-expect
 ((graph-c (change-color "orange" "red" GRAPH-789)) 8)
 "red")

(check-expect
 ((graph-c (change-color "orange" "red" GRAPH-789)) 9)
 "red")

(check-expect
 (graph-v (change-color "orange" "red" GRAPH-789))
 (graph-v GRAPH-789))

(check-expect
 ((graph-e (change-color "orange" "red" GRAPH-789)) 7 7)
 ((graph-e GRAPH-789) 7 7))

(check-expect
 ((graph-e (change-color "orange" "red" GRAPH-789)) 7 8)
 ((graph-e GRAPH-789) 7 8))

(check-expect
 ((graph-e (change-color "orange" "red" GRAPH-789)) 7 9)
 ((graph-e GRAPH-789) 7 9))

(check-expect
 ((graph-e (change-color "orange" "red" GRAPH-789)) 8 7)
 ((graph-e GRAPH-789) 8 7))

(check-expect
 ((graph-e (change-color "orange" "red" GRAPH-789)) 8 8)
 ((graph-e GRAPH-789) 8 8))

(check-expect
 ((graph-e (change-color "orange" "red" GRAPH-789)) 8 9)
 ((graph-e GRAPH-789) 8 9))

(check-expect
 ((graph-e (change-color "orange" "red" GRAPH-789)) 9 7)
 ((graph-e GRAPH-789) 9 7))

(check-expect
 ((graph-e (change-color "orange" "red" GRAPH-789)) 9 8)
 ((graph-e GRAPH-789) 9 8))

(check-expect
 ((graph-e (change-color "orange" "red" GRAPH-789)) 9 9)
 ((graph-e GRAPH-789) 9 9))





(define (change-color c1 c2 graph)
  (make-graph (graph-v graph)
              ;[Nat -> String]
              (λ (n) (if (string=? c1 ((graph-c graph) n)) c2 ((graph-c graph) n)))
              (graph-e graph)))




; TODO 3/4: Finish designing the function has-a-dot? that determines if the
;           supplied graph has a node that does not connect to any node and
;           isn't connected to by any other node :'(

; has-a-dot? : Graph -> Boolean
; is there a node that doesn't connect to any nodes and isn't
; connected to by any other node?


(check-expect (has-a-dot? GRAPH-0) #false)
(check-expect (has-a-dot? GRAPH-123) #true)
(check-expect (has-a-dot? GRAPH-789) #false)


(define (has-a-dot? graph)
  (local [; connected? : Nat -> Boolean
          ; determines if the node is connected in the graph
          (define (connected? node)
            (not (ormap (λ (a) ((graph-e graph) a node)) (graph-v graph))))

          ; connected-to-others? : Nat -> Boolean
          ; determines if a node has a neighbor (connected with other nodes)
          (define (connected-to-others? node)
            (not (ormap (λ (a) ((graph-e graph) node a)) (graph-v graph))))
          
          ; lonely-node? : Node -> Boolean
          ; is this node lonely? (verifies if both are true to considered it as lonely)
          (define (lonely-node? n)
            (and (connected? n)
                 (connected-to-others? n)))]              
    (ormap lonely-node? (graph-v graph))))


;(define GRAPH-123
; (make-graph (list 1 2 3)
;            (λ (v)
;           (cond
;               [(= v 1) "purple"]
;\              [(= v 2) "green"]
;;              [(= v 3) "blue"]))
;         (λ (s d)
;          (cond
;          [(= s 1) (= d 2)]
;           [(= s 2) #false]
;         [(= s 3) #false]))))


; TODO 4/4: Finish designing the function can-reach-in-time? that takes a Graph,
;           the IDs of two nodes in the Graph, and a number of "steps" (a
;           natural number). It determines whether you can get from the node
;           with the first ID to the node with the second ID in the given number
;           of steps, where a step is when you move from a node to its neighbor.

; can-reach-in-time? : Nat Nat Nat Graph -> Boolean
; can you get from start to end in the given number of steps?


(check-expect (can-reach-in-time? 1 1 0 GRAPH-123) #true)
(check-expect (can-reach-in-time? 1 1 1 GRAPH-123) #true)

(check-expect (can-reach-in-time? 1 2 0 GRAPH-123) #false)
(check-expect (can-reach-in-time? 1 2 1 GRAPH-123) #true)
(check-expect (can-reach-in-time? 1 2 2 GRAPH-123) #true)

(check-expect (can-reach-in-time? 7 9 0 GRAPH-789) #false)
(check-expect (can-reach-in-time? 7 9 1 GRAPH-789) #false)
(check-expect (can-reach-in-time? 7 9 2 GRAPH-789) #true)
(check-expect (can-reach-in-time? 7 9 3 GRAPH-789) #true)



(define (can-reach-in-time? a b steps graph)
  (cond
    [(zero? steps) (equal? a b)]
    [(positive? steps)
     (or (and (ormap (lambda (n) (can-reach-in-time? n b (- steps 1) graph))
                     (filter (λ (v) ((graph-e graph) a v)) (graph-v graph)))
              (not (= a b)))
         (= a b))]))