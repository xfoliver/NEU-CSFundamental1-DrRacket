;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw12-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In Homework 3 you worked with the following data...


(define-struct sc [str count])

; A StringCount (SC) is a (make-sc String Nat)
; Interpretation: a string and its count of occurrences

(define SC-A1 (make-sc "A" 1))
(define SC-B1 (make-sc "B" 1))
(define SC-C1 (make-sc "C" 1))
(define SC-A2 (make-sc "A" 2))
(define SC-A3 (make-sc "A" 3))

(define (sc-temp sc)
  (... (sc-str sc) ...
       (sc-count sc) ...))


; TODO 1/2: Finish designing the function count-strings, which counts the
;           distinct strings in a list (in the order they appear, left to
;           right). For the purposes of this problem, you are NOT allowed to use
;           the foldl abstraction (which would have been quite useful!).
;
;           Here's a high-level guide to the function...
;           1. Use a list of SC as your accumulator (starting empty).
;           2. Write a template-based function (much like foldl) that returns
;              the accumulator when you've run out of strings; otherwise, it
;              adds the count of the current string to the count and THEN
;              recurs to the rest of the list (passing along the newly
;              accumulated counts). To add to the count...
;              a) If the counts are empty, just create a new one, initializing
;                 it with the string count of 1. Otherwise...
;              b) If the string matches the current SC in the list, add 1 to its
;                 count; otherwise...
;              c) Continue searching for the string in the count list.


; count-strings : [List-of String] -> [List-of SC]
; Produces a count of the distinct strings found in the list


(check-expect (count-strings '()) '())

(check-expect (count-strings (explode "A"))
              (list SC-A1))

(check-expect (count-strings (explode "AAA"))
              (list SC-A3))

(check-expect (count-strings (explode "ABC"))
              (list SC-A1 SC-B1 SC-C1))

(check-expect (count-strings (explode "ABACA"))
              (list SC-A3 SC-B1 SC-C1))

(check-expect (count-strings (list "cat" "hat" "cat" "hat" "hat"))
              (list (make-sc "cat" 2)
                    (make-sc "hat" 3)))


(define (count-strings los) 
  (local
    [;;add-element: String List-of-SC -> List-of-SC
     ;;returns the accumulator when you've run out of strings; otherwise, it
     ;;adds the count of the current string to the count and THEN
     ;;recurs to the rest of the list
     (define (add-element string losc)
       (cond
         [(empty? losc) (list (make-sc string 1))]
         [(cons? losc)
          (if (string=? string (sc-str (first losc)))
              (cons (make-sc string (+ 1 (sc-count (first losc)))) (rest losc))
              (cons (first losc) (add-element string (rest losc))))]))
     ;;extract: List-of-String List-of-SC 
     ;;It's like use a list of SC as accumulator, start with empty
     (define (extract los losc)
       (cond
         [(empty? los) losc]
         [(cons? los)
          (extract (rest los)
                   (add-element (first los) losc))]))] 
    (extract los '())))




; NOT GRADED, TOTALLY OPTIONAL!!! :)
; TODO 2/2: Once you have completed part 1, uncomment the following code, which
;           implements the more complex scoring for Wordle, wherein letters that
;           appear more than once in a word can receive different status based
;           upon their (left-to-right) position.
;
;           The basic idea of the function...
;           1. Perform a count of all the letters (i.e., what you did above).
;           2. Decrement from those counts correctly located guessed letters
;              (done via remove-correct-counts).
;           3. During left-to-right per-letter scoring, if a letter is not
;              correctly placed...
;              - If it has a positive count from #2, it is considered misplaced
;                (and the count is decremented in the accumulator);
;              - if it does not, it is considered wrong.
;
;           Note: To make it easy to move to your Wordle file, we bundled a LOT
;                 into the local... not very readable/tested is it!?
;
;           To incorporate into your Wordle (Homework 9) file...
;           1. Remove or rename your old score function (including its tests).
;           2. Copy your count-strings function above (including the data design
;              for SC), as well as the score function below.
;           3. Run and enjoy :)
;              (A good comparison is if the correct word is "ATONE" and you
;              guess "DONOR" and "POOLS" - note before/after the status of each
;              "O" in the two guesses.)

(define-struct pair [first second])

; A [Pair X Y] is a (make-pair X Y)
; Interpretation: a pairing of two values


; A LetterStatus (LS) is one of:
; - "wrong"
; - "misplaced"
; - "right"
; Interpretation: status of a guessed letter

(define LS-WRONG "wrong")
(define LS-MISPLACED "misplaced")
(define LS-RIGHT "right")


; A LetterStatusPair (LSP) is a [Pair 1String LetterStatus]
; Interpretation: a guess letter and its associated status


; score : String String -> [List-of LetterStatusPair]
; Given a guess and the correct string (assumed to be the same length),
; produce the resulting pairing of each character and its status

(check-expect (score "ABC" "ABC")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "B" LS-RIGHT)
                    (make-pair "C" LS-RIGHT)))

(check-expect (score "ABC" "XYZ")
              (list (make-pair "A" LS-WRONG)
                    (make-pair "B" LS-WRONG)
                    (make-pair "C" LS-WRONG)))

(check-expect (score "CBA" "ABC")
              (list (make-pair "C" LS-MISPLACED)
                    (make-pair "B" LS-RIGHT)
                    (make-pair "A" LS-MISPLACED)))

;

(check-expect (score "AAA" "ABC")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "A" LS-WRONG)
                    (make-pair "A" LS-WRONG)))

(check-expect (score "AAAXB" "ABCAD")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "A" LS-MISPLACED)
                    (make-pair "A" LS-WRONG)
                    (make-pair "X" LS-WRONG)
                    (make-pair "B" LS-MISPLACED)))

(check-expect (score "WEARY" "DONOR")
              (list (make-pair "W" LS-WRONG)
                    (make-pair "E" LS-WRONG)
                    (make-pair "A" LS-WRONG)
                    (make-pair "R" LS-MISPLACED)
                    (make-pair "Y" LS-WRONG)))

(check-expect (score "BROIL" "DONOR")
              (list (make-pair "B" LS-WRONG)
                    (make-pair "R" LS-MISPLACED)
                    (make-pair "O" LS-MISPLACED)
                    (make-pair "I" LS-WRONG)
                    (make-pair "L" LS-WRONG)))

(check-expect (score "ROUND" "DONOR")
              (list (make-pair "R" LS-MISPLACED)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "U" LS-WRONG)
                    (make-pair "N" LS-MISPLACED)
                    (make-pair "D" LS-MISPLACED)))

(check-expect (score "DONOR" "DONOR")
              (list (make-pair "D" LS-RIGHT)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "N" LS-RIGHT)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "R" LS-RIGHT)))

(check-expect (score "GOALS" "ATONE")
              (list (make-pair "G" LS-WRONG)
                    (make-pair "O" LS-MISPLACED)
                    (make-pair "A" LS-MISPLACED)
                    (make-pair "L" LS-WRONG)
                    (make-pair "S" LS-WRONG)))

(check-expect (score "AROMA" "ATONE")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "R" LS-WRONG)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "M" LS-WRONG)
                    (make-pair "A" LS-WRONG)))

(check-expect (score "AWOKE" "ATONE")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "W" LS-WRONG)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "K" LS-WRONG)
                    (make-pair "E" LS-RIGHT)))

(check-expect (score "ABODE" "ATONE")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "B" LS-WRONG)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "D" LS-WRONG)
                    (make-pair "E" LS-RIGHT)))

(check-expect (score "ATONE" "ATONE")
              (list (make-pair "A" LS-RIGHT)
                    (make-pair "T" LS-RIGHT)
                    (make-pair "O" LS-RIGHT)
                    (make-pair "N" LS-RIGHT)
                    (make-pair "E" LS-RIGHT)))

(define (score guess correct)
  (local [(define GUESS-CHARS (explode guess))
          (define CORRECT-CHARS (explode correct))

          ; could use mymap2 (just removing a dependency in this file)
          (define PAIRED-CHARS (map make-pair GUESS-CHARS CORRECT-CHARS))

          ; pair-string-match? : [Pair 1String 1String] -> Boolean
          ; do the paired characters match?
          (define (pair-string-match? p1s)
            (string=? (pair-first p1s)
                      (pair-second p1s)))

          ; get-guess : [Pair 1String 1String] -> 1String
          ; gets the guess out of a pair (first of the two 1Strings)
          (define (get-guess p1s)
            (pair-first p1s))

          ; remove-correct-counts : [List-of [Pair 1String 1String]] [List-of SC] -> [List-of SC]
          ; removes 1 count from each correctly placed guess
          (define (remove-correct-counts gcs counts)
            (cond
              [(empty? gcs) counts]
              [(cons? gcs)
               (remove-correct-counts
                (rest gcs)
                (if (pair-string-match? (first gcs))
                    (decrement-counts (get-guess (first gcs)) counts)
                    counts))]))

          ; decrement-counts : String [List-of SC] -> [List-of SC]
          ; decrements the count of the matching string
          (define (decrement-counts g counts)
            (cond
              [(empty? counts) counts]
              [(cons? counts)
               (if (right-string? g (first counts))
                   (cons (decrement-count (first counts))
                         (rest counts))
                   (cons (first counts)
                         (decrement-counts g (rest counts))))]))

          ; right-string? : String SC -> Boolean
          ; is the supplied string the same as this count?
          (define (right-string? s sc)
            (string=? s (sc-str sc)))

          ; decrement-count : SC -> SC
          ; subtracts one from the supplied count
          (define (decrement-count sc)
            (make-sc (sc-str sc)
                     (sub1 (sc-count sc))))
                   
          ; score/acc : [List-of [Pair 1String 1String]] [List-of SC] ->
          ;             [List-of LetterStatusPair]
          ; scores each guess letter
          ; ACCUMULATOR: remaining counts of correct letters
          (define (score/acc gcs counts)
            (cond
              [(empty? gcs) '()]
              [(cons? gcs)
               (local [(define G1S (get-guess (first gcs)))]
                 (cond
                   [(pair-string-match? (first gcs))
                    (cons (make-pair G1S LS-RIGHT)
                          (score/acc (rest gcs) counts))]
                   [(has-remaining? G1S counts)
                    (cons (make-pair G1S LS-MISPLACED)
                          (score/acc (rest gcs)
                                     (decrement-counts G1S counts)))]
                   [else
                    (cons (make-pair G1S LS-WRONG)
                          (score/acc (rest gcs) counts))]))]))

          ; has-remaining? : String [List-of SC] -> Boolean
          ; Does the string have a count greater than 0?
          (define (has-remaining? s losc)
            (ormap
             (λ (sc) (and (string=? s (sc-str sc))
                          (positive? (sc-count sc))))
             losc))]
    (score/acc PAIRED-CHARS
               (remove-correct-counts
                PAIRED-CHARS
                (count-strings CORRECT-CHARS)))))

