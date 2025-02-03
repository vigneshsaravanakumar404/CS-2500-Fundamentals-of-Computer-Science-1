;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L11_P1-self-ref) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; So, how would we handle something like modelling Jupiter's 80+ moons?
; Let's start with the airline flights, except this time, instead of the pilots,
; we want to support a large set of passengers


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ponder: What would it mean if we had a data definition that referred to itself,
; as in the following?

; This first data definition is routine:

(define-struct passenger [name seat])

; A Passenger is a (make-passenger String String)
; Representing a passenger on a plane
;  - name is the passenger's name
;  - seat is the passenger's seat
 
(define PASSENGER-1 (make-passenger "Alice" "23A"))
(define PASSENGER-2 (make-passenger "Bob" "33D"))
 
(define (passenger-temp p)
  (... (passenger-name p) ...
       (passenger-seat p) ...))


; But this next one is new, and very strange:

(define-struct flight [passenger flight])
 
; A Flight is a (make-flight Passenger Flight)
; Interpretation: the passengers on a flight
;  - passenger is the current passenger
;  - flight is the remainder of the passengers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Assuming something like the above can work, create some data examples:
;<BRAINSTORM>

(define FLIGHT-A ???)







; Observation: we have a chicken-and-egg problem: we need a Flight to create a Flight.
; Hmm. Ideas?
;<BRAINSTORM>







; What if we used the "is one of" approach?
; Change the Flight data design to:
 
; A Flight is one of:
; - #false
; - (make-flight Passenger Flight)
; Interpretation: the passengers on a flight
;  - passenger is the current passenger
;  - flight is the remainder of the passengers
; or a flight with no passengers (#false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Again, try to create some data examples.
;<DO NOW><SCROLL>








(define FLIGHT-0 #false)
(define FLIGHT-1 (make-flight PASSENGER-1 FLIGHT-0))
(define FLIGHT-2 (make-flight PASSENGER-2 FLIGHT-1))

; Interesting! How many passengers does FLIGHT-0 have? FLIGHT-1? FLIGHT-2?
; Is FLIGHT-1 just a part of FLIGHT-2, or is it a full-fledge Flight in its own right?
; Both!

; Could you create a Flight with 79 passengers?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise:
; Now design the template:

(define (flight-temp f)
  (...
   (cond [(boolean? f) ...]
         [(flight? f) (... (passenger-temp (flight-passenger f)) ...
                           (flight-temp (flight-flight f)) ...)])))

; What's new here? We have the template calling itself! But is that so different
; from when we called a helper on some other type? The only difference here is that
; the other type is... us! Main question is: is this even allowed? Let's see!
; (Remember: a template is never actually evaluated, so we wouldn't know if it worked.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function num-passengers tfhat counts the number of passengers on a flight.

; num-passengers : ??? -> ???
; Returns how many passengers are on the flight
 
(check-expect (num-passengers FLIGHT-0) ?)
(check-expect (num-passengers FLIGHT-1) ?)
(check-expect (num-passengers FLIGHT-2) ?)
 
(define (num-passengers f)
  #; ; start with template:
  (...
   (cond [(boolean? f) ...]
         [(flight? f) (... (passenger-temp (flight-passenger f)) ...
                           (flight-temp (flight-flight f)) ...)]))
  ;<DO NOW><SCROLL-RM>
  







  
  (cond
    [(boolean? f) 0]
    [(flight? f) (add1 (num-passengers (flight-flight f)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The check-expects worked--so it must be allowed! This is called "recursion", and is
; a very powerful concept. Let's pause to think about what the recursive call means
; conceptually, and how it fits into the overall solution.
;<DISCUSS> Myopia and leap of faith

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ANOTHER DESIGN EXAMPLE

; What is a natural number? There is a natural (pun intended!) self-referential definition
; here: a natural number is either 0, or 1 added to some other natural number.

(define-struct nn [prev])
 
; A NaturalNumber (NN) is one of:
; - 0
; - (make-nn NN)
; Interpretation: A natural number (counting number) one greater than the existing NN
; referred to.

; Let's make some data examples:
;<DO NOW><SCROLL>








(define NN-0 0)                ; represents 0
(define NN-1 (make-nn NN-0))   ; represents 1
(define NN-2 (make-nn NN-1))   ; represents 2

; Now let's create the template:

(define (nn-temp nn)
  (...
   (cond
     [(number? nn) ...]
     [(nn? nn) (... (nn-temp (nn-prev nn)) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function nn->int that returns the integer that the NN represents.

; nn->int : NN -> Integer
; Returns the integer that the NN corresponds to
 
(check-expect (nn->int NN-0) 0)
(check-expect (nn->int NN-1) 1)
(check-expect (nn->int NN-2) 2)

(define (nn->int nn)
    #; ; start with template:
  (...
   (cond
     [(number? nn) ...]
     [(nn? nn) (... (nn-temp (nn-prev nn)) ...)]))
;<DO NOW><SCROLL-RM>







  
   (cond
     [(number? nn) 0]
     [(nn? nn) (add1 (nn->int (nn-prev nn)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function nn-even? that accepts a NN and returns whether or not it represents
; an even number.

; nn-even? : NN -> Boolean
; Determines whether or not the NN represents an even number
 
(check-expect (nn-even? NN-0) #true)
(check-expect (nn-even? NN-1) #false)
(check-expect (nn-even? NN-2) #true)

(define (nn-even? nn)
    #; ; start with template:
  (...
   (cond
     [(number? nn) ...]
     [(nn? nn) (... (nn-temp (nn-prev nn)) ...)]))
;<DO NOW><SCROLL-RM>







  
  (cond
    [(number? nn) #true]
    [(nn? nn) (not (nn-even? (nn-prev nn)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Summary:
; the data definition refers to itself, so the template refers to itself, so the
; code refers to itself. There is (intentional) parallelism between the three

; How are we certain the process will terminate?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WE'RE READY FOR JUPITER!

; Exercise:
; Come up with a data definition for a world program that will support an
; arbitrary number of moons.

(require 2htdp/image)
(require 2htdp/universe)
 

(define-struct moon [x vx])
 
; A Moon is a (make-moon Number Number)
; Representing the position and velocity of a moon.
;  - x is the x-position of the moon
;  - vx is the x-velocity of the moon
 
(define MOON-1 (make-moon 10 1))
(define MOON-2 (make-moon 300 -1))
 
(define (moon-temp m)
  (... (moon-x m) ... (moon-vx m) ...))
 
 
(define-struct system [moon system])
 
; A System is one of:
; - #false
; - (make-system Moon System)
; Representing the collection of moons we're trying
; to simulate (or #false for an empty system)
; - make-system
;  - moon is the current moon
;  - system is the rest of the moons
 
(define SYSTEM-0 #false)
(define SYSTEM-1 (make-system MOON-1 SYSTEM-0))
(define SYSTEM-2 (make-system MOON-2 SYSTEM-1))
 
(define (system-temp s)
  (...
   (cond [(boolean? s) ...]
         [(system? s) (... (moon-temp (system-moon s)) ...
                           (system-temp (system-system s)) ...)])))

; Let's see if we can re-implement our eclipse program to use a System instead of Moon.
; Let's start with our big-bang function; all that we need to do there is update the signature:

; eclipse : System -> System
; Runs an eclipse of moons
 
#;
(define (eclipse initial-s)
  (big-bang initial-s
    [to-draw draw-eclipse]
    [on-tick move-eclipse]))


; Now modify draw-eclipse to use our new System data definition.

; draw-eclipse : System -> Image
; Draw the moons on a scene with the sun
 
(define SIZE 400)
(define HALF (/ SIZE 2))
 
(define SUN (circle (/ SIZE 10) "solid" "yellow"))
(define MOON (circle (/ SIZE 10) "solid" "gray"))
(define SKY (square SIZE "solid" "pink"))
 
(check-expect (draw-eclipse SYSTEM-0)
              (place-image SUN
                           HALF HALF
                           SKY))
 
(check-expect (draw-eclipse SYSTEM-2)
              (place-image MOON
                           300 HALF
                           (place-image MOON
                                        10 HALF
                                        (place-image SUN
                                                     HALF HALF
                                                     SKY))))
 
(define (draw-eclipse s)
  #; ; start with template:
  (...
   (cond [(boolean? s) ...]
         [(system? s) (... (moon-temp (system-moon s)) ...
                           (system-temp (system-system s)) ...)]))
;<DO NOW><SCROLL-RM>








  (cond [(boolean? s) (place-image SUN HALF HALF SKY)]
        [(system? s) (draw-moon (system-moon s)
                                (draw-eclipse (system-system s)))]))
 
; draw-moon : Moon Image -> Image
; Draws a moon onto a background
 
(check-expect (draw-moon MOON-1 SKY)
              (place-image MOON (moon-x MOON-1) HALF SKY))
 
(define (draw-moon moon background)
  (place-image MOON
               (moon-x moon) HALF
               background))


; Now let's work on move-eclipse.

; move-eclipse : System -> System
; Moves moons for one tick
 
(check-expect (move-eclipse SYSTEM-0) SYSTEM-0)
 
(check-expect (move-eclipse SYSTEM-2)
              (make-system (make-moon 299 -1)
                           (make-system (make-moon 11 1)
                                        #false)))
 
(define (move-eclipse s)
  #; ; start with template:
  (...
   (cond [(boolean? s) ...]
         [(system? s) (... (moon-temp (system-moon s)) ...
                           (system-temp (system-system s)) ...)]))
;<DO NOW><SCROLL-RM>








  (cond [(boolean? s) #false]
        [(system? s) (make-system (move-moon (system-moon s))
                                  (move-eclipse (system-system s)))]))
 
; move-moon : Moon -> Moon
; Moves a single moon
 
(check-expect (move-moon MOON-1)
              (make-moon 11 1))
 
(check-expect (move-moon MOON-2)
              (make-moon 299 -1))
 
(define (move-moon m)
  (make-moon (+ (moon-x m) (moon-vx m))
             (moon-vx m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's use big-bang's on-key to have it add a new moon every time someone presses a key.

; eclipse : System -> System
; Runs an eclipse of moons
 
(define (eclipse initial-s)
  (big-bang initial-s
    [to-draw draw-eclipse]
    [on-tick move-eclipse]
    [on-key add-moon]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Design the function add-moon that adds a new moon when a key is pressed.

; add-moon : System KeyEvent -> System
; Adds a moon to the system
 
(define NEW-MOON-X 1)
(define NEW-MOON-VX 20)
 
(check-expect (add-moon SYSTEM-0 " ")
              (make-system (make-moon NEW-MOON-X NEW-MOON-VX)
                           #false))
 
(check-expect (add-moon SYSTEM-2 "right")
              (make-system (make-moon NEW-MOON-X NEW-MOON-VX)
                           SYSTEM-2))
 
#;
(define (add-moon s ke)
    #; ; start with template:
  (...
   (cond [(boolean? s) ...]
         [(system? s) (... (moon-temp (system-moon s)) ...
                           (system-temp (system-system s)) ...)]))
;<DO NOW><SCROLL-RM>









  (cond [(boolean? s) (make-system (make-moon NEW-MOON-X NEW-MOON-VX) s)]
        [(system? s) (make-system (make-moon NEW-MOON-X NEW-MOON-VX) s)]))

; Notice something? First, we ignore the KeyEvent - that's ok. Second, both
; clauses do the same thing - this makes sense, because we're always adding
; a moon (no matter whether the current system is empty or not).
; Thus we can simplify to, producing...
 
(define (add-moon s ke)
  (make-system (make-moon NEW-MOON-X NEW-MOON-VX) s))
