;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L08_P1-true-false) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; PRACTICE 1: TRUE/FALSE TESTS

; Exercise:	Design a function passed? that determines if selecting the same response
; for all five questions on a True/False Test gets you a passing grade (at least 3-out-of-5).
; Each question has a prompt and a correct answer.

; passed? : TFTest Boolean -> Boolean

; Hmm... We'll need to do a data design for a test, and that means we need to design
; the component question type first...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step 0: Define the struct.
; Note: this is not part of the data design--it sets the stage for it

(define-struct tfq [prompt answer])

; Now that we have that, do all the steps of the standard data design recipe:

; A TFQuestion is...
;<DO NOW>



; Interpretation: represents a prompt and correct response to a true/false question

; Step 3: Examples
#;
(define TFQ-1 ...) ; an example w/the prompt "Northeastern has a campus in Hawaii", answer: #false
;<DO NOW><SCROLL>








(define TFQ-1 (make-tfq "Northeastern has a campus in Hawaii" #false))
(define TFQ-2 (make-tfq "Northeastern has a campus in London" #true))
(define TFQ-3 (make-tfq "Northeastern has a campus in Seattle" #true))
(define TFQ-4 (make-tfq "Northeastern has a campus in Texas" #false))
(define TFQ-5 (make-tfq "Northeastern has a campus in Egypt" #false))

; Step 4: template:
;<DO NOW><SCROLL>








(define (tfq-temp tfq)
  (... (tfq-prompt tfq) ...
       (tfq-answer tfq) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now that we have the data design for a single T/F question, let's design the
; test for holding 5 of these questions:

(define-struct tft [title date q1 q2 q3 q4 q5])
 
; A TFTest is... <DO NOW>
; representing a test with five true-false questions
 
(define TFT-1 (make-tft TFQ-1 TFQ-2 TFQ-3 TFQ-4 TFQ-5))
; Template?
;<DO NOW><SCROLL>








(define (tft-temp tft)
  (...
   (tft-title tft) ...
   (tft-date tft) ...
   (tfq-temp (tft-q1 tft)) ...
   (tfq-temp (tft-q2 tft)) ...
   (tfq-temp (tft-q3 tft)) ...
   (tfq-temp (tft-q4 tft)) ...
   (tfq-temp (tft-q5 tft)) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Finally, with all data designed, we can design our function

; passed? : TFTest Boolean -> Boolean
; Determines if answering the same answer for
; all questions results in passing (at least 3 out of 5)
 
(check-expect (passed? TFT-1 #true) #false)
(check-expect (passed? TFT-1 #false) #true)
 
(define (passed? tft response)
  ;<DO NOW><SCROLL>
  ; Let's create a helper "score" that works on a single question:







  (>=
   (+
    (score (tft-q1 tft) response)
    (score (tft-q2 tft) response)
    (score (tft-q3 tft) response)
    (score (tft-q4 tft) response)
    (score (tft-q5 tft) response))
   3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now, design the function score:

; score : TFQuestion Boolean -> [0, 1]
; Returns 1 (point) if the question was answered
; correctly

; <DO NOW><SCROLL>








(check-expect (score TFQ-1 #true) 0)
(check-expect (score TFQ-1 #false) 1)
 
(define (score tfq answer)
  (if
   (boolean=? (tfq-answer tfq) answer)
   1 0))
