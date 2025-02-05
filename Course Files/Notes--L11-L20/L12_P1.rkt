;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L14_P1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; LISTS AND DESIGNING FUNCTIONS ON LISTS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Finish up many-moon simulation:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recap: Last time we built on union data and structures to create a data
; definition that can represent any number of items. Let's reviewe a
; couple of examples...

; A Flight is one of:
; - #false
; - (make-flight Passenger Flight)
; Interpretation: the passengers on a flight
;  - passenger is the current passenger
;  - flight is the remainder of the passengers
; or a flight with no passengers (#false)
 
; A System is one of:
; - #false
; - (make-system Moon System)
; Representing the collection of moons we're trying
; to simulate (or #false for an empty system)
; - make-system
;  - moon is the current moon
;  - system is the rest of the moons

; There are some fundamental commonalities there... Can we
; leverage that?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CONS

; What does our Flight represent? Essentially a list of Passengers.
; And System? A list of Moons.

; Having a list of things is a very common requirement, and it seems annoying
; to have to create a data definition every time we want to build a list of some
; other kind of thing.

; Let's look at the shorthand that BSL provides to make this easier.

; What makes a list a "list"? If has several salient properties:
; - It gathers a bunch of things into a collection you can refer to as an entity.
; - you can add something to it.
; - it has a sense of order among the elements

; A list is a very useful thing, and BSL provides you with two tools that
; allow you to create lists: cons and '().

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vocabulary	cons
; Grammar	(cons item list) where item is any value, and list is a list.
; Semantics	constructs a list given a value and an existing list

; What's the difference between this and our make-flight, or our make-system, or...

; - cons was designed to be a general construct from the ground up (compare to make-posn's)
; - cons cares deeply about its second argument; it will throw an error if you try to
; - give it something that's not a list; try to do (cons 1 2) or (cons '() 1) at some point.

; We now have the ability to add to a list, at the "head". Let's try to build one from scratch:

#;
(define my-list-1 (cons 1 (cons 2 (cons 3 ...))))

; ... you see the problem. We need:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vocabulary	'()
; Grammar	'()        (also, empty or null)
; Semantics	a list with no elements

; So now, we can do:
(define my-list-1 (cons 1 (cons 2 (cons 3 '() ))))
; We can also do:
(define my-empty-list '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cons is kind-of like a make-foo constructor. What would be the equivalent of selectors?

; Vocabulary	first, rest
; Grammar	(first cons-list), (rest cons-list)
; Semantics	allows you to access the first item in a list (value),
;		and the second item (another list)

; Note that you can't apply these to an empty list--it wouldn't make sense:
; It's very important to note that you'll get an error if you call (first empty) or (rest empty).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; What about predicates?

; Vocabulary	list?, cons?, empty?
; Grammar	(list? any), (cons? any), (empty? any)
; Semantics	returns whether a supplied value is a list; or a specific type

; Okay! So we can think of cons as a special case of a structure.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LISTS
; Let's see how we can use our newfound knowledge for a data definition:

; A ListOfNumbers (LoN) is one of
; - '()
; - (cons Number LoN)
; Interpretation: ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise	Create some data examples.

(define LON-0 '())
(define LON-1 (cons 3 LON-0))
(define LON-2 (cons 5 LON-1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise	Create the template.

(define (lon-temp lon)
  (...
   (cond
     [(empty? lon) ...]
     [(cons? lon) (... (first lon) ...
                       (lon-temp (rest lon)) ...)])))

; It's basically the same pattern we saw last time.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design a function sum-list that accepts a ListOfNumbers and returns the sum.


; sum-list : LoN -> Number
; adds up the list

(check-expect (sum-list LON-0) 0)
(check-expect (sum-list LON-1) 3)
(check-expect (sum-list LON-2) 8)

(define (sum-list lon)
  ; Start from template
  ;<DO NOW><SCROLL>







  
  (cond
    [(empty? lon) 0]
    [(cons? lon) (+ (first lon) (sum-list (rest lon)))]))

; Don't forget our two catchphrases:
; - Leap of faith!
; - Exercise myopia
