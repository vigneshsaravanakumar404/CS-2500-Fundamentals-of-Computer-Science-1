;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L04_P1_cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Logistics:
; HW1 due Wed at 9:00pm EDT
; Make sure you have your Khoury acct set up *now*!
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Review:

;Vocabulary	cond
;Syntax	cond takes a series of clauses...
;(cond
;  [test-1 answer-1]
;  [test-2 answer-2]
;  ...
;  [test-n/else answer-n])
;Semantics	cond returns the first answer whose test is #true.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sign : Number -> String
; Returns "positive", "negative", or "zero" for the number
 
(define (sign num)
  (cond
  ; <DO NOW><SCROLL>







  
    [(< 0 num) "positive"]
    [(> 0 num) "negative"]
    [(zero? num) "zero"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Review Exercise:
; Create the function num->grade, which would accept your numeric grade
; and give back your letter grade using the standard breakdown.
; You should include the signature and purpose statement

; num->grade : Number -> String
; Returns the letter grade corresponding to your numeric grade

; <NEW> Let's add some examples of how it SHOULD work:
; (num->grade 0) should return "F"
; (num->grade 75) should return "C"
; (num->grade 90) should return "A"
 
(define (num->grade grade)
  ; DO NOW <SCROLL>







  
  (cond
    [(> grade 90) "A"]
    [(> grade 80) "B"]
    [(> grade 70) "C"]
    [(> grade 60) "D"]
    [else "F"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Note that the examples above must be comments (and therefore...?)
; Is there a way to actually execute these checks?

; VOCAB: check-expect
; Grammar: (check-expect expr-to-test expected-value)
; Semantics: Evaluates the expression and verifies that it is correct
; Note that all the check-expects are gathered and only executed AFTER
; all other lines in the file are evaluated. (So it can make forward
; references.)

;<UNCOMMENT AND RERUN>
;(check-expect (num->grade 0) "F")
;(check-expect (num->grade 75) "C")
;(check-expect (num->grade 90) "A")

; These should be inserted where we had the examples above.
; We will use these consistently from now on.

; Also note the "Halloween colors" above after Running

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; if-then
;
; (cond
;   [test-1 answer-1]
;   [else answer-2])
; is the same as...
; 
; (if test-1
;     answer-1
;     answer2)

;For example, let's produce the function num->pf that returns
; "Pass" or "Fail" using our nifty num->grade function
; (start with cond, convert to if)...

(check-expect (num->pf 0) "Fail")
(check-expect (num->pf 75) "Pass")
(check-expect (num->pf 95) "Pass")
 
(define (num->pf grade)
  (cond
    [(string=? "F" (num->grade grade)) "Fail"]
    [else "Pass"]))
 
(define (num->pf-v2 grade)
  (if (string=? "F" (num->grade grade))
      "Fail"
      "Pass"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's apply cond to making a more realistic eclipse
; Original version:
(require 2htdp/image)
(require 2htdp/universe)
 
(define SKY-WIDTH 300)
(define SKY-HEIGHT 200)
(define RADIUS 25)
 
(define SUN (circle RADIUS "solid" "yellow"))
(define MOON (circle RADIUS "solid" "gray"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))
;; NEW:
(define DARKSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "black"))
 
; draw-eclipse : Number -> Image
; Draw the moon at the given x-coordinate, on a scene with the sun
 
(define (draw-eclipse-v1 x-moon)
  (place-image MOON
               x-moon (/ SKY-HEIGHT 2)
               (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2) SKY)))

; <DO NOW>
#;(animate draw-eclipse-v1)

; Exercise:
; Let's make the sky dark when the moon passes over the face of the sun:
(define (draw-eclipse-v2 x-moon)
  ; DO NOW <SCROLL>







  ; How you might do it if you're a former Python programmer:
  #;
  (if (= x-moon (/ SKY-WIDTH 2))
      (place-image MOON
                   x-moon (/ SKY-HEIGHT 2)
                   (place-image SUN
                                (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                                DARKSKY ))
      (place-image MOON
                   x-moon (/ SKY-HEIGHT 2)
                   (place-image SUN
                                (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                                SKY)))
  ; ...but we prefer:
  
  (place-image MOON
               x-moon (/ SKY-HEIGHT 2)
               (place-image SUN
                            (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                            (if (= x-moon (/ SKY-WIDTH 2)) DARKSKY SKY))))
 
#;(animate draw-eclipse-v2)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Too "blinky"--can we make sky darker when partial eclipse?
;; NEW:
(define DIMSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "blue"))

(define (draw-eclipse-v3 x-moon)
  (place-image MOON
               x-moon (/ SKY-HEIGHT 2)
               (place-image SUN
                            (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                            (cond
                              [(= x-moon (/ SKY-WIDTH 2)) DARKSKY]
                              [(< (abs (- x-moon (/ SKY-WIDTH 2))) (* 2 RADIUS)) DIMSKY]
                              [else SKY]))))
 
#;(animate draw-eclipse-v3)
