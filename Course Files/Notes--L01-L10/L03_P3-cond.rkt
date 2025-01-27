;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname P3_cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Booleans: true/#true/#t and false/#false/#f

; Exercise:
; Create the function gonna-get-an-A? that accepts your current grade
; and returns whether or not you are going to get an A.

; gonna-get-an-A? : Number -> Boolean
; Returns whether or not you're going to get an A, given your grade

(define (gonna-get-an-A? grade)
  (<= 90 grade))

; Now let's try it out...
(gonna-get-an-A? 10)
(gonna-get-an-A? 89)
(gonna-get-an-A? 95)


;<SKIP REST OF THIS E.G. FOR NOW>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Now, let's add examples
;;;
;;;; gonna-get-an-A? : Number -> Boolean
;;;; Returns whether or not you're going to get an A, given your grade
;;;; Examples:
;;;; (gonna-get-an-A? 10) should be false
;;;; (gonna-get-an-A? 89) should be false
;;;; (gonna-get-an-A? 95) should be true
;;;; (gonna-get-an-A? 90) should be true
;;; 
;;;#;
;;;(define (gonna-get-an-A? grade)
;;;  (< 90 grade))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; VOCAB: check-expect
;;;;; Grammar:
;;;;; Semantics:
;;;
;;;
;;;
;;;;; Convert above examples to check-expect's
;;;;; DO NOW
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;(check-expect (gonna-get-an-A? 10) #false)
;;;(check-expect (gonna-get-an-A? 89) #false)
;;;(check-expect (gonna-get-an-A? 95) #true)
;;;(check-expect (gonna-get-an-A? 90) #true)
;;;; Did it all work???


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Other predicates:
; For numerical data, BSL has several common predicates:
;   <, <=, >, >=, =, even?, odd?, ...
;
; For string data the most common predicate is
;   string=?
;
; Many data types have such equality tests, such as boolean=?.
;
; There are also many functions that determine whether or not
;   an item of data is of a particular type (e.g., string?).
;
; IMPORTANT:
; There are also standard Boolean operators: and, or, not
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Vocabulary:	cond
;Grammar:	cond takes a series of clauses...
;(cond
;  [test-1 answer-1]
;  [test-2 answer-2]
;  ...
;  [test-n/else answer-n])
;Semantics:	cond evaluates and returns the answer espression
;		for the first clause whose test is #true.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Write the function letter-grade which takes a numerical grade in
; the range 0-100 and returns the corresponding letter grade
;<DO NOW>
