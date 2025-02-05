;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L10_P1-union) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Logistics
; - Will cover up to and including stuctures, i.e., up to L09 (Monday's lecture)
; - i.e., does not cover today's topics (unions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UNION DATA
;
; Recall the two cARS collision simulation from last week: what was missing?
; Our world state was two cars position and velocity components. There is no concept
; of a collided state. Even if we made the velocities 0, that is not the same as
; a singled mangled wreck.
; Our world state needs to accommodate both of these possibilities: two independent cars,
; OR one wrecked mass.
;
; We need a mechanism for representing this kind of multiple-possible-states definition.
; For example:
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; An NUIDorFalse is one of:
; - NaturalNumber
; - #false
; Interpretation: A student's NUID, or #false if student does not yet have an NUID assigned
 
(define NUIDORFALSE-f #false)
(define NUIDORFALSE-1 17)
(define NUIDORFALSE-2 192388)


; Note that this looks like an enumeration; the difference is: an enumeration lists
; particular values that the data could be, here we list particular classes (potentially
; mixed with values within a class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; What would our template look like for NUIDorFalse?
(define (nuidorfalse-temp norf)
  (...
   (cond
     [(number? norf) (... norf ...)]
     [(boolean? norf) ...])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the data to represent a Lyft you ordered, which can be either its (x,y) location
; or a status (such as "arrived").

; A PosnOrString (PorStr) is...
;<DO NOW><SCROLL>








; A PosnOrString (PorStr) is one of
; - (make-posn Number Number)
; - String
; Interpretation: location of the car or its status
 
(define PORSTR-1 (make-posn 15.4 13))
(define PORSTR-2 "arrived")
 
(define (porstr-temp pors)
  (...
   (cond
     [(posn? pors) (... (posn-x pors) ... (posn-y pors) ...)]
     [(string? pors) (... pors ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; However, the status field can be one of several pre-defined possibilities.
; Really should design a data type for that:

; A Status is one of:
; - "arrived"
; - "not assigned"
; Interpretation: a status of a car

(define S-ARRIVED "arrived")
(define S-NOTASSIGNED "not assigned")

(define (status-temp s)
  (...
    (cond 
      [(string=? s S-ARRIVED) ... s ...]
      [(string=? s S-NOTASSIGNED) ... s ...])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We should now go back and update our design for PosnOrString
; A PosnOrStatus (PorS) is one of
; - (make-posn Number Number)
; - Status
; Interpretation: location of the car or its status
 
(define PORS-1 (make-posn 15.4 13))
(define PORS-2 "arrived")

; Note change in template:
; Just like with structs, we call the template for Status in that case:
(define (pors-temp pors)
  (...
   (cond
     [(posn? pors) (... (posn-x pors) ... (posn-y pors) ...)]
     [(string? pors) (... (status-temp pors) ...)])))
