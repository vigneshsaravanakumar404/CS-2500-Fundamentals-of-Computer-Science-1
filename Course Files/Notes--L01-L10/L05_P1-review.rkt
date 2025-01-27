;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname P1-review) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Design recipes, continued...

; From last time:
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
; Design the data for temperature.
; <DO NOW><SCROLL>








; A Temperature is a real number greater than or equal to -273.15 
; Interpretation: A temperature in Celcius
 
(define TEMP-95 95)
(define TEMP-87.2 87.2)
(define TEMP-15 15)
 
(define (temperature-temp temp)
  (... temp ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Design a function convert that converts a Celcius temperature to a Farenheit one.

; First, do the data designs.

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
  (+ (* (/ 9 5) ct) 32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; One more:

; Exercise:
; You are running a vehicle-rental shop that rents out bikes, scooters, and cars.
; Design the function num-spares to suggest the maximum number of wheels that could go
; on a particular vehicle.
;<DO NOW><SCROLL>








; A Vehicle is one of
; - "bike"
; - "scooter"
; - "car"
; Interpretation: A rental vehicle
 
(define VEHICLE-BIKE "bike")
(define VEHICLE-SCOOTER "scooter")
(define VEHICLE-CAR "car")
 
(define (vehicle-temp v)
  (...
   (cond
     [(string=? v VEHICLE-BIKE) ...]
     [(string=? v VEHICLE-SCOOTER) ...]
     [(string=? v VEHICLE-CAR) ...])))
 
; num-spares : Vehicle -> PosInteger
; Determines how many wheels a vehicle has
 
(check-expect (num-spares VEHICLE-BIKE) 2)
(check-expect (num-spares VEHICLE-SCOOTER) 2)
(check-expect (num-spares VEHICLE-CAR) 4)
 
(define (num-spares v)
  (cond
    [(string=? v VEHICLE-BIKE) 2]
    [(string=? v VEHICLE-SCOOTER) 2]
    [(string=? v VEHICLE-CAR) 4]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise
; The course project this year involves the City of Boston, and building maps
; and other interactive programs involving the neighborhoods. We can also talk
; about Counties; we are in Suffolk County currently; Cambridge and Somerville
; are in Middlesex County. Essex county is immediately North of the city, and
; there are eleven other counties that make up the state.
; (The city of Brookline is in an especially whacky situation--look it up!)

; We could define an enumerated datatype for MACounty, simplified to only include
; a few sample counties: for example, Suffolk, Middlesex, and Essex:

;; MACounty is one of:
;; - "Suffolk"
;; - "Middlesex"
;; - "Essex"

; <DO NOW>: Complete the definition by adding an interpretation, examples, and template.









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now that we have a datatype, we can design a predicate (a function that
; returns #true or #false). For example, if we consult the US Census Bureaus
; American Community Survey
; (https://data.census.gov/table/ACSEEO5Y2018.EEOALL7R?q=doctoral&g=040XX00US25$0500000&moe=false),
; we can see the percentage of people with doctorate degrees in these three counties:
; 6.9% for Suffolk, 9.2% for Middlesex, and 4.1% for Essex.

; Exercise: Write a function highly-educated? that takes a MACounty and returns
; a boolean if the county is highly educated, where you determine that by a
; threshold of 5% holding doctoral degrees.

; Side note: In HW & Lab, we will ask you to answer ethical questions related to
; the problems you are working on. For this problem, one that you might be asked
; is: is a 5% threshold for doctoral degrees really the measure of a highly
; educated county? Indeed, is any threshold of such degrees going to be informative
; enough? What other information might you want to include if you wanted your
; predicate to have a more accurate notion of "educatedness" for a neighborhood?
