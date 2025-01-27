;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname P3-posn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Structured data

; Vocabulary	make-posn
; Grammar	(make-posn value value)
; Semantics	A "constructor": it creates a position that encapsulates the two values

(define ORIGIN (make-posn 0 0))

; Vocabulary	posn-x, posn-y
; Grammar	(posn-x posn), (posn-y posn)
; Semantics	"Selectors": returns the first (or second) value that was originally
;		created via make-posn
(define my-posn (make-posn 18 33))
(posn-x my-posn)
; but what of: (posn-x "Hello") ? Won't work--error. Can avoid with...

; Vocabulary	posn?
; Grammar	(posn? value)
; Semantics	A "predicate": determines whether or not the value is a make-posn

; NB: a make-posn is NOT a data definition itself: it is a mechanism
; for creating structured data types
; For example, a make-posn doesn't have to be with two numbers:
(define NAME (make-posn "Jane" "Doe"))

; Can even do:
(define STUDENT-INFO (make-posn "Jane" (make-posn "Doe" "sophomore")))
; ...but this is not recommended: stay tuned!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Building a data definition on top of structured data:

; A Position is a (make-posn Number Number)
; Interpretation:  Represents the x- and y-coordinates of an object
 
(define POSN-1 (make-posn 3 4))
(define POSN-2 (make-posn 0 0))
(define POSN-3 (make-posn 89.2 pi))

; NB: this is how we template structured data
(define (posn-temp p)
  (...
   (posn-x p) ...
   (posn-y p) ...))
 
; x-greater-than-y? : Position -> Boolean
; Determines whether or not the x-coord is greater than the y-coord
 
;<DO NOW>
(check-expect (x-greater-than-y? POSN-1) ???)
(check-expect (x-greater-than-y? POSN-2) ???)
(check-expect (x-greater-than-y? POSN-3) ???)
 
(define (x-greater-than-y? posn)
  ;<DO NOW>
  ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
; add-10-to-x : Position -> Position
; Adds 10 to the x-coordinate

;<DO NOW>
(check-expect (add-10-to-x POSN-1) ???)
(check-expect (add-10-to-x POSN-2) ???)
 
(define (add-10-to-x p)
  ;<DO NOW>
  ...)
