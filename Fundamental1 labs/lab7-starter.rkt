;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab7-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider (but do not change) the following data definitions...


; A Genre is one of
; - "Pop"
; - "Classical"
; - "Country"
; - "Rock"
; Interpretation: a song genre

(define GENRE-POP "Pop")
(define GENRE-CLASSICAL "Classical")
(define GENRE-COUNTRY "Country")
(define GENRE-ROCK "Rock")

(define (genre-temp g)
  (...
   (cond
     [(string=? g GENRE-POP) ...]
     [(string=? g GENRE-CLASSICAL) ...]
     [(string=? g GENRE-COUNTRY) ...]
     [(string=? g GENRE-ROCK) ...])))


(define-struct song [name artist duration genre fav?])

; A Song is a (make-song String String Nat Genre Boolean)
; Interpretation: a song
; - name: the title of the song
; - artist: the song's artist
; - duration: the length in seconds
; - genre: the song's genre
; - fav?: is this a liked song?

(define SONG-1 (make-song "Redesigning Women" "The Highwomen" 174 GENRE-COUNTRY #true))
(define SONG-2 (make-song "Your Song" "Elton John" 241 GENRE-POP #true))
(define SONG-3 (make-song "All Along the Watchtower" "Jimi Hendrix" 241 GENRE-ROCK #false))
(define SONG-4 (make-song "Nessun Dorma" "Luciano Pavarotti" 184 GENRE-CLASSICAL #false))

(define (song-temp song)
  (... (song-name song) ...
       (song-artist song) ...
       (song-duration song) ...
       (genre-temp (song-genre song)) ...
       (song-fav? song) ...))


(define-struct pl [name songs])

; A Playlist is a (make-pl String [List-of Song])
; Interpretation: a sequence of songs

(define PL-0 (make-pl "Quiet :)" '()))
(define PL-1 (make-pl "Coding Beats" (list SONG-1 SONG-2 SONG-3 SONG-4)))

(define (pl-temp pl)
  (... (pl-name pl) ...
       (los-temp (pl-songs pl)) ...))


; Design functions for each TODO below, making use of pre-defined list abstraction(s),
; when appropriate. As a reminder, they include:
;
; - map
; - filter
; - andmap
; - ormap
; - foldr
; - foldl
;
; (There are other list abstractions, but you aren't going to need them for this lab!)
;
; Reminder: just because we now have cool abstractions doesn't mean you should forget
; about the design recipe and following templates (which particularly come up for
; abstraction helpers)!


; TODO 1/5: Design the function all-names that produces a list of all the names of all
;           the songs on a playlist. We have given you a signature, purpose statement,
;           and tests (uncomment them!). So, you only need to write the function!

; all-names : Playlist -> [List-of String]
; Produces all the song titles from a supplied playlist. Using map, it will use the song-name
; function of the songs in the playlist.

(define (all-names playlist)
  (map song-name (pl-songs playlist)))


(check-expect (all-names PL-0) '())

(check-expect
 (all-names PL-1) 
 (list "Redesigning Women" "Your Song" "All Along the Watchtower" "Nessun Dorma"))



; TODO 2/5: Design the function any-pop? that determines if a playlist has any pop songs.
;           We have given you the signature, purpose statement, and tests. So, you just
;           need to uncomment the tests and write the code, as well as design any needed
;           helper functions (hint hint!).

; any-pop? : Playlist -> Boolean
; Determines if any (at least one) are pop songs in the supplied playlist.

(define (any-pop? playlist)
  (ormap  checksong? (pl-songs playlist)))



; checksong?: song -> Boolean
; checks if a song is pop (helper function)
(define (checksong? song)
  (cond
    [(string=? (song-genre song) GENRE-POP) #true]
    [(string=? (song-genre song) GENRE-CLASSICAL) #false]
    [(string=? (song-genre song) GENRE-COUNTRY) #false]
    [(string=? (song-genre song) GENRE-ROCK) #false]))

(check-expect (checksong? SONG-1)#false)
(check-expect (checksong? SONG-2)#true)
(check-expect (checksong? SONG-3)#false)
(check-expect (checksong? SONG-4)#false)




(check-expect (any-pop? PL-0) #false)
(check-expect (any-pop? PL-1) #true)
(check-expect (any-pop? (make-pl "Infinite repeat" (list SONG-3))) #false)




; TODO 3/5: Design the function only-faves that when supplied a playlist returns a new 
;           playlist (with the name "Faves") that only contains the liked songs.
;           We have given you the signature, purpose statement, and tests (to uncomment!).

; only-faves : Playlist -> Playlist
; Produces a new "Faves" playlist containing only all the favorites
; in the supplied playlist. Using the filter function, it will create a playlist with only
; songs that are considered as "favorite"

(define (only-faves playlist)
  (make-pl  "Faves" (filter song-fav? (pl-songs playlist))))


(check-expect (only-faves PL-0) (make-pl "Faves" '()))
(check-expect (only-faves PL-1) (make-pl "Faves" (list SONG-1 SONG-2)))




; TODO 4/5: Design the function all-short? that determines if a playlist contains only 
;           songs shorter than three minutes (180 seconds). We have provided some
;           tests for clarity (uncomment below), but you should do the remaining
;           steps of the function design recipe!


; all-short? : playlist -> Boolean
; determines if a playlist contains only songs shorter than three minutes (180 seconds)
; using andmap, it will check if ALL songs meet requirements
(define (all-short? playlist)
  (andmap minutes? (pl-songs playlist)))




; minutes? : song -> Boolean
; checks if the song duration of the song is greater than 180
(define (minutes? song)
  (< (song-duration song) 180))


(check-expect (minutes? SONG-1) #true)
(check-expect (minutes? SONG-2) #false)
(check-expect (minutes? SONG-3) #false)
(check-expect (minutes? SONG-4) #false)

(check-expect (all-short? PL-0) #true)
(check-expect (all-short? PL-1) #false)
(check-expect (all-short? (make-pl "Short" (list SONG-1))) #true)





; TODO 5/5: Design the function total-duration that returns the total length of a 
;           playlist. For reference, an empty playlist is 0 seconds, and the PL-1
;           example is 840 seconds total. For this final part, make sure to do
;           ALL steps of the function design recipe :)


; total-duration : playlist -> PosReal
; returns the total length of a playlist
; using foldr, it will add all of the song-durations of the songs in the playlist



(define (total-duration playlist)
  (foldr + 0 (map song-duration (pl-songs playlist))))

(check-expect (total-duration PL-0) 0)
(check-expect (total-duration PL-1) 840)

