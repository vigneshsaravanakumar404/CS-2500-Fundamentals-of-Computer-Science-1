;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname L18_P4-exercises) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise: Consider the following code:

(define-struct staff [name ta?])
;; A Staff is a (make-staff String Boolean).
;; A (make-staff name ta?) represents a member of the CS2500 staff where:
;; - name is their name
;; - ta? is #true if they are a TA

(define STAFF-1 (make-staff "Ferdinand Vesely" #false))
(define STAFF-2 (make-staff "Francesca Lucchetti" #true))
(define STAFF-3 (make-staff "Yangtian Zi" #true))
(define STAFF-4 (make-staff "Leena Razzaq" #false))

; Design a function ta-names that produces the names of the TAs on staff.
; You must use list abstractions when possible.

;; ta-names :: [List-of Staff] -> [List-of String]
;; Produces the names of the TAs on staff

(check-expect (ta-names '()) '())
(check-expect (ta-names (list STAFF-1 STAFF-2 STAFF-3 STAFF-4)) 
              (list "Francesca Lucchetti" "Yangtian Zi"))

; Hints:
; - Does the function transform the list items in any way? E.g., add to them,
;   extract fields of a structure, append strings, etc.?
;   If so, it is likely a map. 
; - Does the function exclude some items?
;   If so, it is almost certainly a filter.
; <DO NOW>








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise: How would you make this better?

;; An [NE-List-of X] is one of:
;; - (cons X '())
;; - (cons X [NE-List-of X])
;;
;; Interpretation: A non-empty list of X.

(define (ne-list-template alist)
  (cond [(empty? (rest alist)) (... (first alist) ...)]
        [(cons? (rest alist)) (... (first alist) ...
                                   (ne-list-template (rest alist)) ...)]))

;; sum-all: [List-of Number] -> Number
(define (sum-all alist)
  (cond [(empty? (rest alist)) (first alist)]
        [(cons? (rest alist)) (+ (first alist) (sum-all (rest alist)))]))

;; prod-all: [List-of Number] -> Number
(define (prod-all alist)
  (cond [(empty? (rest alist)) (first alist)]
        [(cons? (rest alist)) (* (first alist) (prod-all (rest alist)))]))

