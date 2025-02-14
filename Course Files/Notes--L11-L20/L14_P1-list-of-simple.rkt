;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L15_P1-list-of-simple) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Let's review what we learned Monday:
; cons, '()
; first, rest
; cons?, empty?, list?

; Why do we need them? We don't... but they make life much more convenient!

; A list made up of self-referential cons cells is an ABSTRACTION
; (what's that?)

; We used it to create a "new-style" ListOfNumbers:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OLD:
#|
(define-struct numlist [number numlist])

; A ListOfNumbers (LoN) is one of
; - #false
; - (make-numlist Number LoN)
; Interpretation: ...

(define LON-0 #false)
(define LON-1 (make-numlist 3 LON-0))
(define LON-2 (make-numlist 5 LON-1))

(define (lon-temp lon)
  (...
   (cond [(boolean? lon) ...]
         [(numlist? lon) (... (numlist-number lon) ...
                              (lon-temp (numlist-numlist lon)) ...)])))
|#

; NEW:
; A ListOfNumbers (LoN) is one of
; - '()
; - (cons Number LoN)
; Interpretation: ...

(define LON-0 '())
(define LON-1 (cons 3 LON-0))
(define LON-2 (cons 5 LON-1))

(define (lon-temp lon)
  (...
   (cond [(empty? lon) ...]
         [(cons? lon) (... (first lon) ...
                           (lon-temp (rest lon)) ...)])))

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







  
  (cond [(empty? lon) 0]
        [(cons? lon) (+ (first lon)
                        (sum-list (rest lon)))]))

; Don't forget our two catchphrases:
; - Leap of faith!
; - Exercise myopia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Create the data definition for ListofStrings

; A ListofStrings (LoS) is...
;<DO NOW><SCROLL>








; A ListofStrings (LoS) is one of
; - '()
; - (cons String LoS)
; Interpretation: ...
 
(define LOS-0 '())
(define LOS-1 (cons "bob" LOS-0))
(define LOS-2 (cons "alice" LOS-1))
 
(define (los-temp los)
  (...
   (cond [(empty? los) ...]
         [(cons? los) (... (first los) ...
                           (los-temp (rest los)) ...)])))

; Very similar to ListOfNumbers: more on this later

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function num-strings that accepts a ListofStrings and
; returns its length.

; num-strings : ListOfStrings -> Number
; Returns the number of strings in the list
 
(check-expect (num-strings LOS-0) 0)
(check-expect (num-strings LOS-1) 1)
(check-expect (num-strings LOS-2) 2)
 
(define (num-strings los)
  ;<DO NOW><SCROLL-RM>







  
  (cond [(empty? los) 0]
        [(cons? los) (add1 (num-strings (rest los)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function count-chars that accepts a ListofStrings and
; returns the total number of characters in all Strings.


; count-chars : ListOfStrings -> Number
; Returns the number of characters in the list
 
(check-expect (count-chars LOS-0) 0)
(check-expect (count-chars LOS-1) 3)
(check-expect (count-chars LOS-2) 8)
 
(define (count-chars los)
  ;<DO NOW><SCROLL-RM>







  
  (cond [(empty? los) 0]
        [(cons? los) (+ (string-length (first los))
                        (count-chars (rest los)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function contains-alice? that accepts a ListofStrings and
; returns whether or not it contains the string "alice".


; contains-alice? : ListOfStrings -> Boolean
; Determines whether or not the list contains "alice"
 
(check-expect (contains-alice? LOS-0) #false)
(check-expect (contains-alice? LOS-1) #false)
(check-expect (contains-alice? LOS-2) #true)
 
(define (contains-alice? los)
  ;<DO NOW><SCROLL-RM>







  
  (cond [(empty? los) #false]
        [(cons? los) (or (string=? (first los) "alice")
                         (contains-alice? (rest los)))]))
