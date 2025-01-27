;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname P2_design-recipe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; We want to follow a "recipe" to help us create well-designed, understandable,
; error-free programs.
;
; We followed one for designing functions:
; Step 1: Function signature
; Step 2: Purpose statement
; Step 3: Tests
; Step 4: Code
;
; This is our "design recipe" for functions.
; What about for data?

;
; Let's review a old program:
; Exercise: Design a function num->grade, which accepts your numeric grade
; and returns your letter grade.
;
; First step: a signature:

; num->grade : Number -> String

; That says it accepts a Number: any Number? What does a grade of 120 get?
; How about negative numbers? Imaginary??
; What about the String return value: is returning "Foo" okay? ""?
; Need more...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designing Data
;
; What is data? How is it different from information?
; information -> data : representation
; data -> information : interpretation
; We think in terms of information, but program with data

; So how do we design a data type?

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: data definition
; E.g.s:
; A NumericGrade is a...
; <DO NOW> <SCROLL>








; A NumericGrade is a real number in [0, 100]

; What about letter grades? Can we just say...

; A LetterGrade is a String

; Not good enough; better:

; A LetterGrade is one of:
; - "A"
; - "B"
; - "C"
; - "D"
; - "F"

; This is an _enumeration_: a finite set of discrete possible values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data design, Part 2: data interpretation
; A NumericGrade is a real number in [0, 100]
; Interpretation: A student's current numeric grade in CS 2500
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A LetterGrade is one of:
; - "A"
; - "B"
; - "C"
; - "D"
; - "F"
; Interpretation:  A student's letter grade in the US system

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data design, Part 3: Examples of the data type
; A NumericGrade is a real number in [0, 100]
; Interpretation: A student's current numeric grade in CS 2500

(define NUMGRADE-0 0)
(define NUMGRADE-75 75)
(define NUMGRADE-95 95)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A LetterGrade is one of:
; - "A"
; - "B"
; - "C"
; - "D"
; - "F"
; Interpretation:  A student's letter grade in the US system

(define GRADE-A "A")
(define GRADE-B "B")
(define GRADE-C "C")
(define GRADE-D "D")
(define GRADE-F "F")

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data design, Part 4: the template

; A NumericGrade is a real number in [0, 100]
; Interpretation: A student's current numeric grade in CS 2500
 
(define NUMGRADE-0 0)
(define NUMGRADE-75 75)
(define NUMGRADE-95 95)
 
; numericgrade-temp : NumericGrade -> ?
(define (numericgrade-temp ng)
  (... ng ...))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A LetterGrade is one of:
; - "A"
; - "B"
; - "C"
; - "D"
; - "F"
; Interpretation:  A student's letter grade in the US system
 
(define GRADE-A "A")
(define GRADE-B "B")
(define GRADE-C "C")
(define GRADE-D "D")
(define GRADE-F "F")
 
; lettergrade-temp : LetterGrade -> ?
(define (lettergrade-temp lg)
  (...
   (cond
     [(string=? lg GRADE-A) ...]
     [(string=? lg GRADE-B) ...]
     [(string=? lg GRADE-C) ...]
     [(string=? lg GRADE-D) ...]
     [(string=? lg GRADE-F) ...])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the data for temperature. (do not specify Celcius/Kelvin/etc – see what they come up with)
; <DO NOW> <SCROLL>








; A Temperature is a real number greater than or equal to -273.15 
; Interpretation: A temperature in Celcius
 
(define TEMP-95 95)
(define TEMP-87.2 87.2)
(define TEMP-15 15)
 
(define (temperature-temp temp)
  (... temp ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the data for an MBTA line.
; <DO NOW> <SCROLL>








; An MBTALine is one of
; - "Red"
; - "Blue"
; - "Orange"
; - "Green"
; Interpretation: One of the MBTA lines
 
(define MBTALINE-RED "Red")
(define MBTALINE-BLUE "Blue")
(define MBTALINE-ORANGE "Orange")
(define MBTALINE-GREEN "Green")
 
(define (mbtaline-temp line)
  (...
   (cond
     [(string=? line MBTALINE-RED) ...]
     [(string=? line MBTALINE-BLUE) ...]
     [(string=? line MBTALINE-ORANGE) ...]
     [(string=? line MBTALINE-GREEN) ...])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's now update our function design to work with our new data designs:

; ORIGINAL:
; num->grade : Number -> String
; Returns the letter grade corresponding to your numeric grade
 
(check-expect (num->grade 0) "F")
(check-expect (num->grade 75) "C")
(check-expect (num->grade 95) "A")
 
#;
(define (num->grade grade)
  (cond
    [(<= 90 grade) "A"]
    [(<= 80 grade) "B"]
    [(<= 70 grade) "C"]
    [(<= 60 grade) "D"]
    [else "F"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NEW AND IMPROVED:

; num->grade : NumericGrade -> LetterGrade
; Returns the letter grade corresponding to your numeric grade
 
(check-expect (num->grade NUMGRADE-0) GRADE-F)
(check-expect (num->grade NUMGRADE-75) GRADE-C)
(check-expect (num->grade NUMGRADE-95) GRADE-A)

(define (num->grade grade)
  (cond
    [(<= 90 grade) GRADE-A]
    [(<= 80 grade) GRADE-B]
    [(<= 70 grade) GRADE-C]
    [(<= 60 grade) GRADE-D]
    [else GRADE-F]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Note the parallels between the data and function design recipes:
; Data design:		Function design:
; ============		================
; Definition		Signature
; Interpretation	Purpose Statement
; Examples		Tests
; Template		Code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PRACTICE
;
; Exercise:
; Design the function grade->gpa that converts a letter grade to GPA points.
; We cannot even write the signature for this function until we've designed the data:

; A GPA is a real number in [0,4]
; Interpretation:  The GPA "points" in the U.S. system
 
(define GPA-0.1 0.1)
(define GPA-4.0 4.0)
 
(define (gpa-temp gpa)
  (... gpa ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NOW, we can design the function:

; grade->gpa : LetterGrade -> GPA
; Returns the GPA points for the given letter grade
 
(check-expect (grade->gpa GRADE-A) 4.0)
(check-expect (grade->gpa GRADE-C) 2.0)
(check-expect (grade->gpa GRADE-F) 0.0)

; Where should we start for writing the code? Hmm...
;
; ... the template!!

#;
(define (grade->gpa lg)
  (...
   (cond
     [(string=? lg GRADE-A) ...]
     [(string=? lg GRADE-B) ...]
     [(string=? lg GRADE-C) ...]
     [(string=? lg GRADE-D) ...]
     [(string=? lg GRADE-F) ...])))

; and then we adapt this template to our needs
; <DO NOW> <SCROLL>







(define (grade->gpa lg)
  (cond
    [(string=? lg GRADE-A) 4.0]
    [(string=? lg GRADE-B) 3.0]
    [(string=? lg GRADE-C) 2.0]
    [(string=? lg GRADE-D) 1.0]
    [(string=? lg GRADE-F) 0.0]))

; Now hit "Run".
; Note the appearance again of the "black bars", indicating untested code!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design a function convert that converts a Celcius temperature to a Farenheit one.

; First, do the data designs.

; A CelciusTemperature is...
;<DO NOW> <SCROLL>








; A CelciusTemperature is a real number greater than or equal to -273.15 
; Interpretation: A temperature in degrees celcius
 
(define CTEMP-95 95)
(define CTEMP-87.2 87.2)
(define CTEMP-15 15)
 
(define (celciustemperature-temp ct)
  (... ct ...))
 
 
; A FarenheitTemperature is a real number greater than or equal to -459.67 
; Interpretation: A temperature in degrees farenheit
 
(define FTEMP-95 95)
(define FTEMP-87.2 87.2)
(define FTEMP-15 15)
 
(define (farenheittemperature-temp ft)
  (... ft ...))


; convert : CelciusTemperature -> FarenheitTemperature
; Converts a celcius temperature to the equivalent farenheit one
 
(check-expect (convert 0) 32)
(check-expect (convert -40) -40)
(check-expect (convert 100) 212)
 
(define (convert ct)
  ; <DO NOW> <SCROLL>
  








  (+ (* (/ 9 5) ct) 32))
