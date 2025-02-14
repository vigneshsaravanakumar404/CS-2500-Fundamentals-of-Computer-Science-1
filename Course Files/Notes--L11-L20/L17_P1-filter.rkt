;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname L18_P1-keep-if) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; FILTER

; Let's look at two other list-processing function abstractions
; ("list abstractions" for short).In past lectures, we've had occasion to:
;   (1) Filter a list (e.g., removing moons that are not visible)
;   (2) Collapse a list (summing numbers).

; Let's start with filtering strings...

; For old time's sake:

; A [List-of String] (LoS) is one of
; - '()
; - (cons String [List-of String])
; Interpretation: a list of strings!

(define STR-S "msg")
(define STR-SP "dear msg")
(define STR-L "really really long message")
(define STR-LP "dear person this is a really really long message that is polite")
 
(define LOS-0 '())
(define LOS-1 (cons STR-S LOS-0))
(define LOS-2 (cons STR-SP LOS-1))
(define LOS-3 (cons STR-L LOS-2))
(define LOS-4 (cons STR-LP LOS-3))

(define (los-temp los)
  (...
   (cond [(empty? los) ...]
         [(cons? los) (... (first los) ...
                           (los-temp (rest los)) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Group A:
;   Design short-msgs that accepts a list of strings and returns just those that are
;   shorter than 14 characters;
;
; Group B:
;   Design polite-msgs that accepts a list of strings and returns just those that
;   start with "dear".
;<DO NOW><SCROLL>








; short-msgs : [List-of String] -> [List-of String]
; Keeps just the strings that are < 14 characters
 
(define SHORT 14)
 
(check-expect (short-msgs LOS-0) LOS-0)
(check-expect (short-msgs LOS-1) LOS-1)
(check-expect (short-msgs LOS-2) LOS-2)
(check-expect (short-msgs LOS-3) LOS-2)
(check-expect (short-msgs LOS-4) LOS-2)
 
(define (short-msgs los)
  ; Start w/template:
  #;
  (define (los-temp los)
  (...
   (cond [(empty? los) ...]
         [(cons? los) (... (first los) ...
                           (los-temp (rest los)) ...)])))
  ;<DO NOW><SCROLL>







  
  (cond [(empty? los) '()]
        [(cons? los) (if (< (string-length (first los)) SHORT)
                         (cons (first los) (short-msgs (rest los)))
                         (short-msgs (rest los)))]))
 
; polite-msgs : [List-of String] -> [List-of String]
; Keeps just the strings that start with "dear"
 
(define POLITE "dear")
 
(check-expect (polite-msgs LOS-0) LOS-0)
(check-expect (polite-msgs LOS-1) LOS-0)
(check-expect (polite-msgs LOS-2) (cons STR-SP '()))
(check-expect (polite-msgs LOS-3) (cons STR-SP '()))
(check-expect (polite-msgs LOS-4) (cons STR-LP
                                        (cons STR-SP '())))
 
(define (polite-msgs los)
  ; Start w/template:
  #;
  (define (los-temp los)
  (...
   (cond [(empty? los) ...]
         [(cons? los) (... (first los) ...
                           (los-temp (rest los)) ...)])))
  ;<DO NOW><SCROLL>







  
  (cond [(empty? los) '()]
        [(cons? los) (if (and (>= (string-length (first los)) (string-length POLITE))
                              (string=? (substring (first los) 0 (string-length POLITE)) POLITE))
                         (cons (first los) (polite-msgs (rest los)))
                         (polite-msgs (rest los)))]))

; Recall the steps to abstraction: First highlight the differences - in this case,
; it's only the condition for the if, which is easier to see with helper functions...

(define (short-msgs/v2 los)
  (cond [(empty? los) '()]
        [(cons? los) (if (short? (first los))
                         (cons (first los) (short-msgs (rest los)))
                         (short-msgs (rest los)))]))

; short? : String -> Boolean
; Determines if a string is short
 
(check-expect (short? STR-SP) #true)
(check-expect (short? STR-L) #false)
 
(define (short? s)
  (< (string-length s) SHORT))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (polite-msgs los)
  (cond [(empty? los) '()]
        [(cons? los) (if (polite? (first los))
                         (cons (first los) (polite-msgs (rest los)))
                         (polite-msgs (rest los)))]))

; polite? : String -> Boolean
; Determines if a string starts with "dear"
 
(check-expect (polite? STR-S) #false)
(check-expect (polite? STR-LP) #true)
 
(define (polite? s)
  (and (>= (string-length s) (string-length POLITE))
       (string=? (substring s 0 (string-length POLITE)) POLITE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 2: parameterize the difference(s) as parameters

; So what would our abstracted function look like? Let's call it keep-if.

; keep-if : ??? <DO NOW>
; Keeps just the strings that the function returns true for
 
(check-expect (keep-if LOS-4 short?)
              (cons STR-SP (cons STR-S '())))
 
(check-expect (keep-if LOS-4 polite?)
              (cons STR-LP (cons STR-SP '())))
 
(define (keep-if lox p?)
  ;<DO NOW> What should we start from?







  
  (cond [(empty? lox) '()]
        [(cons? lox) (if (p? (first lox))
                         (cons (first lox) (keep-if (rest lox) p?))
                         (keep-if (rest lox) p?))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Step 3: re-write our original functions to use the abstracted one.
; Half the class do short-msgs, the other half do polite-msgs.
;<DO NOW><SCROLL>









; Note: this is just modified parts--the rest of the design--sig, purpose,
; tests remain exactly as-is

(define (short-msgs los)
  (keep-if los short?))

(define (polite-msgs los)
  (keep-if los polite?))


; I would claim this is more readable, more reusable, and much shorter!
