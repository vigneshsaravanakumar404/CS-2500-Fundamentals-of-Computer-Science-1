;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname P1-abst-funcs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Last week, we found similarities between data types like
; a System of Moons, and a Flight of Passengers, all created with
; unions involving self-referential data types that terminated in some
; base value (e.g., #false, or "").
; We used a data abstraction--the list--to reimplement these types in
; a common, reusable format; e.g.:
;
; A ListofNumbers is one of:
; - '()
; - (cons Number LoN)
; Interpretation: ...
;
; This allowed us to more simply define lists of things using a common infrastructure.
; We could define a ListofStrings, a ListofImages, even a list of structures, like
; a ListofMoons.
;
; Can this same principle be applied to function design?
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; First, create the data definition for a ListOfStrings.
; A ListOfStrings (LoS) is...
;<DO NOW>





(define LOS-0 '())
(define LOS-1 (cons "alice" LOS-0))
(define LOS-2 (cons "bob" LOS-1))
 
(define (los-temp los)
  ;<DO NOW>




  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Side A:
;   Design the function prefix-with-from, which accepts a ListOfStrings and
;   prefixes every string with "From: ".
;
; Side B:
;   Design the function prefix-with-to, which accepts a ListOfStrings and
;   prefixes every string with "To: ".

; prefix-with-from : LoS -> LoS
; Prefixes each of the strings in the list with "From: "
 
(check-expect (prefix-with-from LOS-0) '())
(check-expect (prefix-with-from LOS-2)
              (cons "From: bob"
                    (cons "From: alice" '())))
(define (prefix-with-from los)
  ;<DO NOW>
  )





; prefix-with-to : LoS -> LoS
; Prefixes each of the strings in the list with "To: "
 
(check-expect (prefix-with-to LOS-0) '())
(check-expect (prefix-with-to LOS-2)
              (cons "To: bob"
                    (cons "To: alice" '())))
 
(define (prefix-with-to los)
  ;<DO NOW>
  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; What do you notice about the two functions? What are the only differences?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ADDING A PARAMETER
; How we could avoid re-writing the same code twice? What if we abstracted
; this function by adding a parameter that represented the string we wanted
; to pre-pend?


; prefix-with : LoS String -> LoS
; Prefixes each of the strings in the list with the second string

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Exercise:
; Finish designing this function.

; prefix-with : LoS String -> LoS
; Prefixes each of the strings in the list with the second string
 
(check-expect (prefix-with LOS-0 "From: ") '())
(check-expect (prefix-with LOS-0 "To: ") '())
 
(check-expect (prefix-with LOS-2 "To: ")
              (cons "To: bob"
                    (cons "To: alice" '())))
 
(check-expect (prefix-with LOS-2 "From: ")
              (cons "From: bob"
                    (cons "From: alice" '())))
 
(define (prefix-with los prefix)
; HINT: in the recursive call, we had to pass in the parameter again. Why?
  ;<DO NOW>




  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We aren't done yet; haven't solved the original problem, which was to
; specifically design the functions prefix-with-from and prefix-with-to.
; Let's fulfill that now

; Exercise:
; Re-write prefix-with-from and prefix-with-to to use prefix-with.
; Note: the signatures, purpose statements, and tests do not need to change;
; in fact, they shouldn't!

(define (prefix-with-from los)
  (prefix-with los "From: "))

(define (prefix-with-to los)
  (prefix-with los "To: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The general approach for abstracting functions is:
; 
; Spot the differences between the functions
; Create an abstracted function where the differences are a parameter
; Re-write the original function to use the abstracted function.

