;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw7-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This problem asks you to design several functions that employ the
; following data definitions. The functions that you design *must* use
; list abstraction(s) when appropriate; you MAY NOT use recursion: doing
; so will lead you to get no code credit for the function :(
;
; NOTE #1: Part of the credit for each problem will be based on the choice
; of list abstractions, so make sure that they are a good match for the
; problem.
;
; NOTE #2: For certain problems, you will have to design helper functions
; that do not use list abstractions. You should follow the full design
; recipe (including appropriate use of templates) for all problems. Be sure
; to do this, even if it feels a bit tedious - listen to your templates!!
;
; Data Definitions (do not modify these)


; A Weekday is one of:
; - "Monday"
; - "Tuesday"
; - "Wednesday"
; - "Thursday"
; - "Friday"
; Interpretation: a day that excludes the weekend

(define WEEKDAY-M "Monday")
(define WEEKDAY-T "Tuesday")
(define WEEKDAY-W "Wednesday")
(define WEEKDAY-R "Thursday")
(define WEEKDAY-F "Friday")

(define (weekday-temp w)
  (...
   (cond
     [(string=? w WEEKDAY-M) ...]
     [(string=? w WEEKDAY-T) ...]
     [(string=? w WEEKDAY-W) ...]
     [(string=? w WEEKDAY-R) ...]
     [(string=? w WEEKDAY-F) ...])))


(define-struct meeting [day bname rnum hstart mstart duration])

; A ClassMeeting is a (make-meeting Weekday String String PosInt[8, 18] NonNegInt[0, 59] PosInt)
; Interpretation: when a class is scheduled to meet weekly
; - day: which day of the week
; - bname: name of the building
; - rnum: room number
; - hstart: starting hour (24hr)
; - mstart: starting minute
; - duration: length of the class (in minutes)

(define CM-FUNDIES-M (make-meeting WEEKDAY-M "WVH" "210A" 10 30 65))
(define CM-FUNDIES-W (make-meeting WEEKDAY-W "WVH" "210A" 10 30 65))
(define CM-FUNDIES-R (make-meeting WEEKDAY-R "WVH" "210A" 10 30 65))
;
(define CM-FUNDIES-LAB (make-meeting WEEKDAY-T "WVH" "212" 8 0 100))
;
(define CM-DISCRETE-T (make-meeting WEEKDAY-T "ISEC" "102" 13 35 100))
(define CM-DISCRETE-F (make-meeting WEEKDAY-F "ISEC" "102" 13 35 100))
;
(define CM-DISCRETE-SEM (make-meeting WEEKDAY-W "Hastings" "110" 16 35 65))
;
(define CM-CREATURES-T (make-meeting WEEKDAY-T "Forbidden Forest" "Hut" 13 0 200))
(define CM-POTIONS-R (make-meeting WEEKDAY-R "Hogwarts" "Dungeon" 13 0 200))

(define (classmeeting-temp cm)
  (... (weekday-temp (meeting-day cm)) ...
       (meeting-bname cm) ...
       (meeting-rnum cm) ...
       (meeting-hstart cm) ...
       (meeting-mstart cm) ...
       (meeting-duration cm) ...))


(define-struct course [prefix num name prof meetings])

; A Course is a (make-course String String String String [List-of ClassMeeting])
; Interpretation: a weekly class
; - prefix: the course prefix
; - num: the course number
; - name: the course name
; - prof: name of the professor
; - meetings: weekly meeting times

(define COURSE-EASY-A
  (make-course "SCHED" "101" "Easy A" "Lazy"
               '()))

(define COURSE-FUNDIES-LECTURE
  (make-course "CS" "2500" "Fundies" "Howdy"
               (list CM-FUNDIES-M CM-FUNDIES-W CM-FUNDIES-R)))

(define COURSE-FUNDIES-LAB
  (make-course "CS" "2501" "Fundies Lab" "Awesome TAs"
               (list CM-FUNDIES-LAB)))

(define COURSE-DISCRETE-LECTURE
  (make-course "CS" "1800" "Discrete Structures" "Dr Strange"
               (list CM-DISCRETE-T CM-DISCRETE-F)))

(define COURSE-DISCRETE-SEM
  (make-course "CS" "1802" "Seminar for CS 1800" "Park"
               (list CM-DISCRETE-SEM)))

(define COURSE-CREATURES
  (make-course "HPTR" "2000" "Care of Magical Creatures" "Hagrid"
               (list CM-CREATURES-T)))

(define COURSE-POTIONS
  (make-course "HPTR" "2650" "Potions" "Snape"
               (list CM-POTIONS-R)))

(define (course-temp c)
  (... (course-prefix c) ...
       (course-num c) ...
       (course-name c) ...
       (course-prof c) ...
       (locm-temp (course-meetings c)) ...))


; A CourseSchedule is a [List-of Course]
; Interpretation: a list of weekly courses!

(define SCHEDULE-OOPS '())

(define SCHEDULE-KHOURY
  (list COURSE-FUNDIES-LECTURE
        COURSE-FUNDIES-LAB
        COURSE-DISCRETE-LECTURE
        COURSE-DISCRETE-SEM))

(define SCHEDULE-MAGIC
  (list COURSE-CREATURES
        COURSE-POTIONS))

(define SCHEDULE-CS+MAGIC
  (list COURSE-FUNDIES-LECTURE
        COURSE-FUNDIES-LAB
        COURSE-CREATURES
        COURSE-POTIONS))


; TODO 1/8: Part of healthy course scheduling is making sure to build in time for
;           food, and so you are to design the function lunch-course that produces
;           a "Lunch" course!
;
;           The function should take in a prefix & number (e.g., "FOOD" "101"),
;           a name & professor (e.g., "Exciting Baking" with "Alderton"), as
;           well as a list of weekdays. The function will then makes sure that
;           déjeuner occurs on all of those days at noon (for one hour) in a
;           single location of your choice (e.g., Hogwarts Great Hall).
;
;           Note: make sure to test your function on at least two sets of inputs!



(define LC-WED (make-meeting WEEKDAY-W "IV" "100" 12 00 60))
(define LC-THURS (make-meeting WEEKDAY-R "IV" "100" 12 00 60))

(define LUNCH-COURSE1
  (make-course "FOOD" "101" "Exciting Baking" "Alterton"
               (list LC-WED LC-THURS)))

;(check-expect (lunch-course "Food" "101" "Exciting Baking" "Alterton" (list WEEKDAY-W WEEKDAY-R)))




; lunch-course: String String String String [List-Of Weekday] -> course

(define (lunch-course prefix num name professor weekday)
  (make-course prefix num name professor (map weekday-helper weekday)))

  

(define (weekday-helper weekday)
  (make-meeting weekday "IV" "100" 12 00 60))

(check-expect (weekday-helper WEEKDAY-M) (make-meeting "Monday" "IV" "100" 12 0 60))
(check-expect (weekday-helper WEEKDAY-T) (make-meeting "Tuesday" "IV" "100" 12 0 60))
(check-expect (weekday-helper WEEKDAY-W) (make-meeting "Wednesday" "IV" "100" 12 0 60))
(check-expect (weekday-helper WEEKDAY-R) (make-meeting "Thursday" "IV" "100" 12 0 60))
(check-expect (weekday-helper WEEKDAY-F) (make-meeting "Friday" "IV" "100" 12 0 60))



(check-expect (lunch-course "FOOD" "101" "Exciting Baking" "Alterton" (list WEEKDAY-W WEEKDAY-R))
              LUNCH-COURSE1)

(check-expect (lunch-course "Snack" "300" "East Snacks" "Hescott" (list WEEKDAY-W WEEKDAY-R))
              (make-course "Snack" "300" "East Snacks" "Hescott"
                           (list (make-meeting "Wednesday" "IV" "100" 12 0 60)
                                 (make-meeting "Thursday" "IV" "100" 12 0 60))))




; TODO 2/8: Design the function long-weekend? that determines if a
;           course schedule avoids all classes on Mondays & Fridays.
;           In the examples above, this is true of OOPS and MAGIC.
;           Note: make sure to follow all the templates and
;           sufficiently test all your functions!

;;long-weekend?: class -> boolean
;;determines if a course schedule avoids all classes on Mondays & Fridays

(check-expect (long-weekend? SCHEDULE-OOPS) #true)
(check-expect (long-weekend? SCHEDULE-MAGIC) #true)
(check-expect (long-weekend? SCHEDULE-CS+MAGIC) #false)
(check-expect (long-weekend? SCHEDULE-KHOURY) #false)

(define (long-weekend? schedule)
  (andmap mandf? schedule))

;;mandf?: Course -> Boolean
;;determine wether the course on monday or friday

(check-expect (mandf? COURSE-FUNDIES-LAB) #true)
(check-expect (mandf? COURSE-FUNDIES-LECTURE) #false)

(define (mandf? course)
  (andmap includemf? (course-meetings course)))

;;includemf: Meeting -> Boolean
;;determine wether the meeting day is on monday or friday

(check-expect (includemf? CM-DISCRETE-T) #true)
(check-expect (includemf? CM-FUNDIES-M) #false)

(define (includemf? meet)
  (cond
    [(string=? (meeting-day meet) WEEKDAY-M) #false]
    [(string=? (meeting-day meet) WEEKDAY-F) #false]
    [else #true]))


; TODO 3/8: Design the function only-khoury that takes a course schedule
;           and produces a new schedule only containing classes that
;           have the prefix "CS", "DS", or "CY". So supplying OOPS and
;           KHOURY would result in unaffected schedules, but MAGIC would
;           result in an empty schedule and CS+MAGIC would result in a
;           schedule with only Fundies :)
;
;           Note: since we didn't include any DS/CY courses in the
;           examples, make may need to create example courses to properly
;           test your helper function(s)! Some course suggestions include
;           DS2000 (Programming with Data) and CY2550 (Foundations of
;           Cybersecurity).


; only-khoury: course schedule -> course schedule
; takes a course schedule and produces a new schedule only containing classes that
; have the prefix "CS", "DS", or "CY"
(define (only-khoury c-schedule)
  (filter khoury-class c-schedule)) ; filter will only create a list
;                                     containing only khoury-type classes


; khoury-class : course -> boolean
; this helper function will extract each course prefix. If it is either "CS" "DS" or "CY", it will
; produce true, else will be false
(define (khoury-class course)
  (cond
    [(string=? (course-prefix course) "CS") #t]
    [(string=? (course-prefix course) "DS") #t]
    [(string=? (course-prefix course) "CY") #t]
    [else #false]))

  
(check-expect (only-khoury SCHEDULE-CS+MAGIC)
              (list COURSE-FUNDIES-LECTURE
                    COURSE-FUNDIES-LAB))


(check-expect (only-khoury SCHEDULE-KHOURY)
              (list COURSE-FUNDIES-LECTURE
                    COURSE-FUNDIES-LAB
                    COURSE-DISCRETE-LECTURE
                    COURSE-DISCRETE-SEM))


             
 

; TODO 4/8: Design the function time-in-class that calculates total
;           time spent in class (in minutes each week) for a supplied
;           course schedule. For example, OOPS requires 0 minutes and
;           KHOURY is 560.


; time-in-class: course schedule -> PosReal
; calculates total time spent in class (in minutes each week) for a supplied course schedule
(define (time-in-class c-schedule)
  (foldr + 0 (meeting1 c-schedule))); using foldr and + and 0, strting from 0,
;                                     it will add all of the meetings times of the course schedule


              
; [List-of course] -> [List-of Num]
; this helper function will help create a list of all numbers for each class in the course
(define (meeting1 c-schedule)
  (map course-meet-time c-schedule));map will apply that helper function to all parts of list


; course -> Num
;this helper function will specificy 1 number for each 1 course
(define (course-meet-time course)
  (meet-time (course-meetings course)))



  
; [List-of Meetings] -> Num
(define (meet-time meetings)
  (foldr + 0 (map meeting-duration meetings)))

(check-expect (time-in-class SCHEDULE-KHOURY) 560)
(check-expect (time-in-class SCHEDULE-MAGIC) 400)
(check-expect (time-in-class SCHEDULE-CS+MAGIC) 695)

(check-expect (course-meet-time COURSE-EASY-A) 0)
(check-expect (course-meet-time COURSE-FUNDIES-LECTURE) 195)
(check-expect (course-meet-time COURSE-CREATURES) 200)

  
(check-expect (meeting1 SCHEDULE-KHOURY) (list 195 100 200 65))
(check-expect (meeting1 SCHEDULE-MAGIC) (list 200 200))
(check-expect (meeting1 SCHEDULE-CS+MAGIC) (list 195 100 200 200))

; TODO 5/8: Design the function bring-water? that takes a course schedule
;           and determines if any course has even a single meeting that
;           lasts for longer than two hours. For example, this is true
;           for either of the magic schedules, but none of the others.

; bring-water: [List-of course] -> Boolean
; determines if any course has even a single meeting that lasts for longer than two hours
(define (bring-water? course-schedule)
  (ormap twohours? course-schedule)); ormap will check if AT LEAST ONE is true

; twohours?: course -> Boolean
; this helper function will check if 1 course-meeting of the course falls correct
(define (twohours? course)
  (ormap meetingofcourse (course-meetings course)))

; meetingofcourse: [List-of meetings] -> Boolean
; will chek specifically if the meeting-durations of the meeting of the course
; in the list is greater than 120
(define (meetingofcourse meetings)
  (> (meeting-duration meetings) 120))



(check-expect (bring-water? SCHEDULE-KHOURY) #false)
(check-expect (bring-water? SCHEDULE-MAGIC) #true)
(check-expect (bring-water? SCHEDULE-CS+MAGIC) #true)

(check-expect (twohours? COURSE-FUNDIES-LECTURE) #false)
(check-expect (twohours? COURSE-FUNDIES-LAB) #false)
(check-expect (twohours? COURSE-CREATURES) #true)
(check-expect (twohours? COURSE-POTIONS) #true)
(check-expect (twohours? COURSE-DISCRETE-LECTURE) #false)
(check-expect (twohours? COURSE-DISCRETE-SEM) #false)


(check-expect (meetingofcourse CM-FUNDIES-M) #false)
(check-expect (meetingofcourse CM-FUNDIES-W) #false)
(check-expect (meetingofcourse CM-FUNDIES-R) #false)
(check-expect (meetingofcourse CM-FUNDIES-LAB) #false)
(check-expect (meetingofcourse CM-DISCRETE-T) #false)
(check-expect (meetingofcourse CM-DISCRETE-F) #false)
(check-expect (meetingofcourse CM-DISCRETE-SEM) #false)
(check-expect (meetingofcourse CM-CREATURES-T) #true)
(check-expect (meetingofcourse CM-POTIONS-R) #true)





               
; TODO 6/8: Design the function course->days-abbrev that takes a course
;           and produces a single string that has abbreviations of all
;           days of the week that course meets. For instance, Fundies
;           lecture would produce "MWR", Fundies lab would produce "T",
;           Discrete lecture would be "TF", and the "easy A" class would
;           produce "" (since the lazy prof never wants to meet!).


; course->days-abbrev: [List-of course] -> String
; takes a course and produces a single string that has abbreviations of all
; days of the week that course meets
(define (course->days-abbrev course)
  (foldr string-append "" (map weekdayhelper (course-meetings course))))
; using foldr and string-append it will connect all abbreviations.
; If a class doesnt have meet days, it will be blank


; weekdayhelper: [List-of ClassMeeting] -> String
; using a helper function, it will abbreviate the meeting day of each course
(define (weekdayhelper course)
  (abbreviation (meeting-day course)))


; ; weekdayhelper: meeting-day -> String
;it will compare the meetingday of the course and iif it has,
; it will output the correct weekday as an abbreviation
(define (abbreviation meetingday)
  (cond
    [(string=? meetingday WEEKDAY-M) "M"]
    [(string=? meetingday WEEKDAY-T) "T"]
    [(string=? meetingday WEEKDAY-W) "W"]
    [(string=? meetingday WEEKDAY-R) "R"]
    [(string=? meetingday WEEKDAY-F) "F"]))


(check-expect (course->days-abbrev COURSE-FUNDIES-LECTURE) "MWR")
(check-expect (course->days-abbrev COURSE-EASY-A) "")
(check-expect (course->days-abbrev COURSE-FUNDIES-LAB) "T")
(check-expect (course->days-abbrev COURSE-CREATURES) "T")
(check-expect (course->days-abbrev COURSE-POTIONS) "R")
(check-expect (course->days-abbrev COURSE-DISCRETE-LECTURE) "TF")
(check-expect (course->days-abbrev COURSE-DISCRETE-SEM) "W")




; TODO 7/8: Design the functions stack/h and stack/v, to stack a supplied
;           list of images horizontally and vertically, with a bit of buffer
;           between each image (see the GAP we've defined for you). You have
;           been supplied tests for clarity.




;[List-of Image] -> Image
; will create an image of all of the supplied list horizontally,
; with a gap in between, beside eachother
(define (stack/h image)
  (foldr beside GAP (map stack-helper image)))

;stack-helper: Image-Type -> Image
; this helper will create the GAP next to each part of the list for stack/h
(define (stack-helper list)
  (beside GAP list))


;[List-of Image] -> Image
; will create an image of all of the supplied list horizontally,
; with a gap in between, above eachother
(define (stack/v image)
  (foldr above GAP (map stack-helper1 image)))

;stack-helper: Image-Type -> Image
; this helper will create the GAP next to each part of the list for stack/v
(define (stack-helper1 list)
  (above GAP list))




; GAP: Image
; creates an image of a sqaure box that is 5, solid, and white
(define GAP (square 5 "solid" "white"))


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




; TODO 8/8: Now using your solutions to the previous two parts, design the
;           function viz-schedule, which produces a visual representation
;           of a supplied course schedule, such that each course is a row
;           (with the prefix, num, name, prof, and day abbreviations) and
;           the rows are vertically stacked. You have been supplied tests
;           for clarity.


; viz-schedule course: course -> Image
; produces a visual representation of a supplied course schedule, such that each course is a row
; (with the prefix, num, name, prof, and day abbreviations) and the rows are vertically stacked
(define (viz-schedule course)
  (stack/v (map visualschedule course)))


; visualschedule: course -> Image
; create a text image of the course as asked
(define (visualschedule course)
  (text (string-append (course-prefix course) " " (course-num course)
                       " (" (course-name course) ", " (course-prof course) "): "
                       (course->days-abbrev course)) 50 "black"))


                 


  






(check-expect (viz-schedule SCHEDULE-OOPS) GAP)

(check-expect (viz-schedule SCHEDULE-KHOURY)
              (above GAP
                     (text "CS 2500 (Fundies, Howdy): MWR" 50 "black")
                     GAP
                     (text "CS 2501 (Fundies Lab, Awesome TAs): T" 50 "black")
                     GAP
                     (text "CS 1800 (Discrete Structures, Dr Strange): TF" 50 "black")
                     GAP
                     (text "CS 1802 (Seminar for CS 1800, Park): W" 50 "black")
                     GAP))


(check-expect (visualschedule COURSE-FUNDIES-LECTURE)
              (text "CS 2500 (Fundies, Howdy): MWR" 50 "black"))

(check-expect (visualschedule COURSE-FUNDIES-LAB)
              (text "CS 2501 (Fundies Lab, Awesome TAs): T" 50 "black"))

(check-expect (visualschedule COURSE-DISCRETE-LECTURE)
              (text "CS 1800 (Discrete Structures, Dr Strange): TF" 50 "black"))
