;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname P1-local) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; First let's review local with a warmup problem.
; What is the value of the following expression?

(define X 1)
(define Y 3)
 
(define Z (local [(define X 10)
                  (define Y 20)]
            (local [(define X 30)]
              (+ X Y))))
(+ X Z)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Use build-list to design a function that computes the first n double-squares,
; or numbers that are twice as large as perfect squares.
; (This is the same exercise as yesterday, but this time, use local

; double-squares : Nat -> [List-of Nat]
; The first n double squares

(check-expect (double-squares 0) '())
(check-expect (double-squares 1) (list 0))
(check-expect (double-squares 4) (list 0 2 8 18))
 
(define (double-squares n)
  (local [; double-square : Nat -> Nat
          ; The nth double square
          ; if given 0, returns 0
          ; if given 2, returns 8
          ; if given 3, returns 18
          (define (double-square n)
            (* 2 (sqr n)))]
    (build-list n double-square)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DESIGN RECIPE FOR LOCAL
; Let's focus on when to use local and how to use local.
; Let's start with the design recipe for how to use it.

; Often, we use local because we want to use a list abstraction. Therefore, it will be
; useful to describe the design recipe in terms of a problem that accepts a list.

; Here's the basic design recipe for using local, framed around the following example:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design add-3-to-all, which accepts a [List-of Number] and adds 3 to every one.

; LOCAL Design Recipe, Step 1:
; Start with the first three steps of the design recipe for your outer function.


; add-3-to-all : [List-of Number] -> [List-of Number]
; Adds 3 to every number

(check-expect (add-3-to-all '()) '())
(check-expect (add-3-to-all (list 0)) (list 3))
(check-expect (add-3-to-all (list -7 2.3)) (list -4 5.3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LOCAL Design Recipe, Step 2:
; Write a "stub" function for your main function that uses a local

#;
(define (add-3-to-all lon)
  (local [???
          ???]
    (abstraction ???)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LOCAL Design Recipe, Step 3:
; Decide which list abstraction is appropriate for your problem:
; - Look at the signatures of the list abstractions, see which one fits.
; - Copy down its general signature, and then fill in the dots.

#;
;<DISCUSS> What list abstraction is best here?








(define (add-3-to-all lon)
  (local [???
          ???]
    ; (X Y) [X -> Y] [List-of X] -> [List-of Y]
    ; X is Number
    ; Y is Number
    ; [Number -> Number] [List-of Number] -> [List-of Number]
    (map ??? lon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LOCAL Design Recipe, Step 4:
; Design any functions you need inside the body of the local.
; Follow the full design recipe, except that any tests have to be written in English.

(define (add-3-to-all lon)
  (local [; add-3 : Number -> Number
          ; Adds 3 to a number
          ; Given 4, should return 7
          (define (add-3 n)
            (+ n 3))]
    ; (X Y) [X -> Y] [List-of X] -> [List-of Y]
    ; X is Number
    ; Y is Number
    ; [Number -> Number] [List-of Number] -> [List-of Number]
    (map add-3 lon)))

; You can erase the abstraction signature and X is ... comments once you're done.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; When should you use local? What does it buy you? How does it help?
; We'll discuss four reasons why you might want to use local :

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. HIDING HELPER FUNCTIONS
; Local allows us to embed, and therefore hide, one-off functions inside where they are needed,
; instead of cluttering up the global scope


; Exercise:
; Design the function shortest-string that accepts a [List-of String] and returns
; the (first) shortest one. If the list is empty, it should return false.


; A StringOrFalse (SoF) is one of:
; - String
; - #false
; Interpretation: A string, or #false if none exists
 
(define SOF-1 "hello")
(define SOF-2 "yo")
(define SOF-3 #false)
 
(define (sof-temp sof)
  (...
   (cond
     [(string? sof) (... sof ...)]
     [(boolean? sof) ...])))
 
; shortest-string : [List-of String] -> SoF
; Returns the (first) shortest string, or #false if given an empty list
 
(check-expect (shortest-string '()) #false)
(check-expect (shortest-string (list "a")) "a")
(check-expect (shortest-string (list "a" "b" "c")) "a")
(check-expect (shortest-string (list "abc" "b" "cde")) "b")
(check-expect (shortest-string (list "abc" "b" "cde" "f" "")) "")
 
(define (shortest-string los)
  (local [; String SoF -> SoF
          ; Combines the two, returning the shortest string
          ; if given "foo" and false, returns "foo"
          ; if given "foo" and "bar", returns "foo"
          ; if given "foo" and "b", returns "b"
          (define (combine-shortest str sof)
            (cond
              [(string? sof)
               (if (<= (string-length str)
                       (string-length sof))
                   str sof)]
              [(boolean? sof) str]))]
    (foldr combine-shortest #false los)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. USING CONTEXT

; Previously, the only way to pass information into function was through its parameters;
; now, local gives us a second way.
; with local, functions can be defined inside of other functions, meaning they have
; access to arguments passed into the outside function.

; Exercise:
; Design a function usd->eur that accepts a [List-of Number] representing
; prices in USD and a Number representing the USD-EUR exchange rate,
; and converts each amount to EUR.


; usd->eur : [List-of Number] Number -> [List-of Number]
; Coverts USD to EUR with the given exchange rate

(check-expect (usd->eur '() 2.0) '())
(check-expect (usd->eur (list 1 2 3) 2.0) (list 2.0 4.0 6.0))
(check-expect (usd->eur (list 1 2 3) 2.5) (list 2.5 5.0 7.5))
 
(define (usd->eur lon rate)
  (local [; convert : Number -> Number
          ; Converts the USD amount to EUR
          ; If rate is 7.0, given 1.0 would produce 7.0
          (define (convert n)
            (* n rate))]
 
    ; (X Y) [X -> Y] [List-of X] -> [List-of Y]
    ; X is Number
    ; Y is Number
    ; [Number -> Number] [List-of Number]
    (map convert lon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Consider the data definition below:

(define-struct traincar [seats wheels])
 
; A TrainCar is a (make-traincar Number Number)
; Interpretation: A train car, with the number of seats and wheels
 
(define TRAINCAR-1 (make-traincar 40 3))
(define TRAINCAR-2 (make-traincar 50 7))
(define TRAINCAR-3 (make-traincar 20 1))
 
; A Train is a [List-of TrainCar]
; Interpretation: A collection of cars that make up a train
 
(define TRAIN-0 '())
(define TRAIN-1 (cons TRAINCAR-1 TRAIN-0))
(define TRAIN-2 (cons TRAINCAR-2 TRAIN-1))
(define TRAIN-3 (cons TRAINCAR-3 TRAIN-2))

; Now, design the function num-bigger-cars that accepts a Train and a Number
; and counts the number of cars with at least the provided number of seats.
; Design two versions: one with a list abstraction and one without.

; num-big-cars : Train NatNum -> NatNum
; Returns the number of cars that at least as many seats as the provided Number
 
(check-expect (num-big-cars TRAIN-0 40) 0)
(check-expect (num-big-cars TRAIN-1 40) 1)
(check-expect (num-big-cars TRAIN-2 40) 2)
(check-expect (num-big-cars TRAIN-3 40) 2)
 
(define (num-big-cars t num)
  ;<DO NOW><SCROLL> helper:( big-traincar? TC num)







  
  (cond
    [(empty? t) 0]
    [(cons? t)
     (if (big-traincar? (first t) num)
         (add1 (num-big-cars (rest t) num))
         (num-big-cars (rest t) num))]))
 
; big-traincar? : TrainCar NatNum -> Boolean
; Returns whether or not the TrainCar has at least num seats
 
(check-expect (big-traincar? TRAINCAR-1 40) #true)
(check-expect (big-traincar? TRAINCAR-2 40) #true)
(check-expect (big-traincar? TRAINCAR-3 40) #false)
 
(define (big-traincar? tc num)
  (>= (traincar-seats tc) num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ... and now the abstraction-based design:
; Can we do it using filter ? (there is a function length)
;<DISCUSS>








; num-big-cars/abs : Train NatNum -> NatNum
; Returns the number of cars that at least as many seats as the provided Number
 
(check-expect (num-big-cars/abs TRAIN-0 40) 0)
(check-expect (num-big-cars/abs TRAIN-1 40) 1)
(check-expect (num-big-cars/abs TRAIN-2 40) 2)
(check-expect (num-big-cars/abs TRAIN-3 40) 2)
 
(define (num-big-cars/abs t num)
  (local [; big-traincar? : TrainCar -> Boolean
          ; Returns whether or not the TrainCar has at least num seats
          ; if num 40, (check-expect (bigger-traincar? TRAINCAR-1) #true)
          ; if num 40, (check-expect (bigger-traincar? TRAINCAR-2) #true)
          ; if num 40, (check-expect (bigger-traincar? TRAINCAR-3) #false)
          (define (big-traincar? tc)
            (>= (traincar-seats tc) num))]
    (length (filter big-traincar? t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. CLARIFYING CODE
; local can be used to make complex functions more understandable:

; Exercise:
; Design the function slope that accepts two Positions's and returns
; a Number representing the slope of the line defined by those two points.

; Again, let's try both without and with local.

; A Position is a (make-posn Real Real)
; Interpretation: a 2d position

(define POSITION-0_0 (make-posn 0 0))
(define POSITION-1_2 (make-posn 1 2))

(define (position-temp p)
  (... (posn-x p) ...
       (posn-y p) ...))

; slope : Position Position -> Number
; Calculates the slope of the line connecting the two points
 
(check-expect (slope POSITION-1_2 POSITION-0_0) 2.0)
 
(define (slope p1 p2)
  (/ (- (posn-y p2) (posn-y p1))
     (- (posn-x p2) (posn-x p1))))

; slope/better : Position Position -> Number
; Calculates the slope of the line connecting the two points
 
(check-expect (slope/better POSITION-1_2 POSITION-0_0) 2.0)
 
(define (slope/better p1 p2)
  (local [(define DELTA-Y (- (posn-y p2) (posn-y p1)))
          (define DELTA-X (- (posn-x p2) (posn-x p1)))]
    (/ DELTA-Y DELTA-X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 4. EFFICIENCY
; local can be used to remember the values of previous computations.

; Exercise:
; Design the function mymax that accepts a non-empty list of Numbers
; and returns the biggest.Do not use a list abstraction; do this one by hand.
; A [NEList-of X] is one of:
; - (cons X '())
; - (cons X [NEList-of X])
 
(define NELOX-1 (cons 1 '()))
(define NELOX-2 (cons 2 NELOX-1))
 
(define (nelox-temp nelox)
  (...
   (cond
     [(empty? (rest nelox))
      (... (first nelox) ...)]
     [(cons? (rest nelox))
      (...
       (first nelox) ...
       (nelox-temp  (rest nelox)) ...)])))
 
 
; mymax : [NEList-of Number] -> Number
; Finds the biggest number in the list
 
(check-expect (mymax (list 1)) 1)
(check-expect (mymax (list 1 4 3 5)) 5)
(check-expect (mymax (list 5 3 4 1)) 5)
 
(define (mymax nelon)
  (cond
    [(empty? (rest nelon)) (first nelon)]
    [(cons? (rest nelon))
     (if (> (first nelon) (mymax (rest nelon)))
         (first nelon)
         (mymax (rest nelon)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's mesure how this function scales:
(mymax (build-list 20 identity))

; Seemed slow. The ISL function time allows us to measure it more precisely
(time (mymax (build-list 20 identity)))
(time (mymax (build-list 21 identity)))
(time (mymax (build-list 22 identity)))
; Why so long to find the max of just 22 elements??
;<DISCUSS>








; Write a better version using local
;<DO NOW><SCROLL>








(define (mymax/v2 nelon)
  (cond
    [(empty? (rest nelon)) (first nelon)]
    [(cons? (rest nelon))
     (local [(define MYMAX (mymax/v2 (rest nelon)))]
       (if (> (first nelon) MYMAX)
           (first nelon)
           MYMAX))]))

; Let's see how well v2 runs:

(time (mymax/v2 (build-list 22 identity)))
(time (mymax/v2 (build-list 1000 identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MORE PRACTICE (optional)

; Exercise:
; Design a function num-perfect-squares that accepts a [List-of Number] and
; counts those numbers that are perfect squares.

; num-perfect-squares : [List-of Number] -> NatNum
; Returns the count of numbers are a perfect square

(check-expect (num-perfect-squares '()) 0)
(check-expect (num-perfect-squares (list 3 4 18)) 1)
(check-expect (num-perfect-squares (list 3 10 -1 18)) 0)
(check-expect (num-perfect-squares (list 4 9 16)) 3)
 
(define (num-perfect-squares lon)
  (local [; perfect-square-count : Number NatNum -> NatNum
          ; Increments the supplied count if the number is a perfect square
          ; Given 4.0 and 1 would produce 2
          ; Given 3.0 and 1 would produce 1
          (define (perfect-square-count n count)
            (if (integer? (sqrt n))
                (+ 1 count)
                count))]
 
    ; (X Y) [X Y -> Y] Y [List-of X] -> Y
    ; X is Number
    ; Y is NatNum
    ; [Number NatNum -> NatNum] NatNum [List-of Number]
    (foldr perfect-square-count 0 lon)))

; Another version:
(define (num-perfect-squares/filter lon)
  (local [; perfect-square? : Number -> Boolean
          ; Returns whether or not the number is a perfect square
          ; Given 4.0 would produce true
          ; Given 3.0 would produce false
          (define (perfect-square? n)
            (integer? (sqrt n)))]
 
    ; (X) [X -> Boolean] [List-of X] -> Boolean
    ; X is Number
    ; [Number -> Boolean] [List-of Number]
    (length (filter perfect-square? lon))))
