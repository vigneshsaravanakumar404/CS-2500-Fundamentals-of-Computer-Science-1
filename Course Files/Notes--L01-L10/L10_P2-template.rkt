;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L10_P2-template) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CREATING TEMPLATES

; There are four basic steps to create a template for a data definition.
; Let's design a data type called Foo:

; 1) Ask if Foo can take multiple forms. Look for "is one of".
;      If so, you need a cond.
; 2) If there are multiple cases, what predicates tell them apart?
;      These are the questions in your cond.
; 3) Can you "deconstruct" the data (i.e., take it apart if it's a structure)?
;      If so, add those selectors.
; 4) Have you hit another data definition?
;      If so, call its template.
;
; Following this process will all you to create a template for any
; data definition you've seen in this class.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Create a template for...

; A Silly is one of:
; - Number
; - (make-posn String Image)

;<DO NOW><SCROLL-RM>








(define (silly-temp s)
  (...
   (cond [(number? s) (... s ...)]
         [(posn? s) (... (posn-x s) ... (posn-y s) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Create a template for...

; A Bar is one of
; - "hello"
; - PositiveInteger

;<DO NOW><SCROLL-RM>








(define (bar-temp b)
  (...
   (cond [(string? b) ...]
         [(number? b) (... b ...)])))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct blah [ab cd])

; A Foo is a (make-blah Number Bar)
;<DO NOW><SCROLL-RM>








(define (foo-temp f)
  (... (blah-ab f) ...
       (bar-temp (blah-cd f)) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXERCISE: PILOTS
; Create a data definition for a flight, which has two pilots. Each pilot has a name and an age.

;<BRAINSTORM>








; (Likely, came up with flat structure)
; (define-struct flight [captain-name captain-age firstofficer-name firstofficer-age])
 
; A Flight is a (make-flight String Number String Number)
 
#;
(define (flight-temp f)
  (... (flight-captain-name f) ... (flight-captain-age f) ...
       (flight-firstofficer-name f) ... (flight-firstofficer-name f) ...))

; This creates a lot of duplication
; Let's try a NESTED STRUCTURE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct pilot [name age])
 
; A Pilot is a (make-pilot String Number)
; Representing the name and age of a pilot.
;  - name is the name of the pilot
;  - age is the age of the pilot in years
 
(define PILOT-1 (make-pilot "alice" 25))
(define PILOT-2 (make-pilot "bob" 35))
(define PILOT-3 (make-pilot "carol" 70))


(define-struct flight [captain firstofficer])
 
; A Flight is a (make-flight Pilot Pilot)
; representing a flight with the two pilots.
;  - captain is the pilot who is the captain of the flight
;  - firstofficer is the pilot who is the first officer of the flight
 
(define FLIGHT-1 (make-flight PILOT-1 PILOT-2))
(define FLIGHT-2 (make-flight PILOT-1 PILOT-3))
(define FLIGHT-3 (make-flight PILOT-2 PILOT-3))

; Now create the templates
; <DO NOW><SCROLL-RM>








(define (pilot-temp p)
  (... (pilot-name p) ...
       (pilot-age p) ...))
 
(define (flight-temp f)
  (... (pilot-temp (flight-captain f)) ...
       (pilot-temp (flight-firstofficer f)) ...))

; Why did flight-temp call pilot-temp? Because we've hit another data definition,
; we need to call its template.
; That's the general rule: when we hit a another data definition, we use its template.
; That means the nested template will become a helper function.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Now design a function is-flight-legal? that accepts a Flight and returns
; whether or not both pilots are 65 or younger (and are therefore legal to fly).

(define AGE-LIMIT 65)
 
; is-pilot-legal? : Pilot -> Boolean
; Determines whether the pilot is 65 or younger
 
(check-expect (is-pilot-legal? PILOT-1) #true)
(check-expect (is-pilot-legal? PILOT-2) #true)
(check-expect (is-pilot-legal? PILOT-3) #false)
 
(define (is-pilot-legal? pilot)
  (<= (pilot-age pilot) AGE-LIMIT))
 
; is-flight-legal? : Flight -> Boolean
; Returns whether or not both pilots are 65 or younger
 
(check-expect (is-flight-legal? FLIGHT-1) #true)
(check-expect (is-flight-legal? FLIGHT-2) #false)
(check-expect (is-flight-legal? FLIGHT-3) #false)
 
(define (is-flight-legal? flight)
  (and (is-pilot-legal? (flight-captain flight))
       (is-pilot-legal? (flight-firstofficer flight))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Now design a function is-alice-flying? that accepts a Flight and returns
; whether or not the pilot "alice" is assigned to this flight.

; is-pilot-alice? : Pilot -> Boolean
; Returns whether the pilot is "alice"
 
(check-expect (is-pilot-alice? PILOT-1) #true)
(check-expect (is-pilot-alice? PILOT-2) #false)
(check-expect (is-pilot-alice? PILOT-3) #false)
 
(define (is-pilot-alice? pilot)
  (string=? (pilot-name pilot) "alice"))
 
; is-alice-flying? : Flight -> Boolean
; Returns whether or not "Alice" is one of the pilots
 
(check-expect (is-alice-flying? FLIGHT-1) #true)
(check-expect (is-alice-flying? FLIGHT-2) #true)
(check-expect (is-alice-flying? FLIGHT-3) #false)
 
(define (is-alice-flying? flight)
  (or (is-pilot-alice? (flight-captain flight))
      (is-pilot-alice? (flight-firstofficer flight))))
