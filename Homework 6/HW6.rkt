;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

;;                             Exercise 1
;; ========================================================================

; SmallNatural is assumed to be defined as per https://piazza.com/class/m5h7r615vbn18k/post/131


(define LIST-1 (cons 1 (cons 2 (cons 3 '()))))
(define LIST-2 (cons 1 (cons 2 (cons 3 (cons 4 '())))))
(define LIST-3 (cons 43 (cons 70 (cons 12 '()))))


; keep-greater-than : [List-of SmallNatural] SmallNatural -> [List-of Number]
; Produces a list of numbers greater or equal to the given number
(check-expect (keep-greater-than LIST-1 2) (cons 2 (cons 3 '())))
(check-expect (keep-greater-than LIST-2 2) (cons 2 (cons 3 (cons 4 '()))))
(check-expect (keep-greater-than LIST-3 50) (cons 70 '()))

(define (keep-greater-than lon num)
  (cond
    [(empty? lon) '()]
    [(>= (first lon) num) (cons (first lon) (keep-greater-than (rest lon) num))]
    [else (keep-greater-than (rest lon) num)]))


; keep-less-than : [List-of SmallNatural] SmallNatural -> [List-of SmallNatural]
; Produces a list of numbers less than the given number
(check-expect (keep-less-than LIST-1 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than LIST-2 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than LIST-3 50) (cons 43 (cons 12 '())))

(define (keep-less-than lon num)
  (cond
    [(empty? lon) '()]
    [(<= (first lon) num) (cons (first lon) (keep-less-than (rest lon) num))]
    [else (keep-less-than (rest lon) num)]))


; keep-select: [List-of SmallNatural] SmallNatural SmallNatural -> [List-of SmallNatural]
; Produces a list of numbers that satisfy the given modifier
(check-expect (keep-select LIST-1 2 1000) (cons 2 (cons 3 '())))
(check-expect (keep-select LIST-2 0 2) (cons 1 (cons 2 '())))
(check-expect (keep-select LIST-3 70 70) (cons 70 '()))

(define (keep-select lon low high)
  (cond 
    [(empty? lon) '()]
    [(and (>= (first lon) low) (<= (first lon) high)) 
     (cons (first lon) (keep-select (rest lon) low high))]
    [else (keep-select (rest lon) low high)]))


; keep-greater-than/v2 : [List-of SmallNatural] SmallNatural -> [List-of SmallNatural]
; Produces a list of numbers greater or equal to the given number
(check-expect (keep-greater-than/v2 LIST-1 2) (cons 2 (cons 3 '())))
(check-expect (keep-greater-than/v2 LIST-2 2) (cons 2 (cons 3 (cons 4 '()))))
(check-expect (keep-greater-than/v2 LIST-3 50) (cons 70 '()))

(define (keep-greater-than/v2 lon num)
  (keep-select lon num 1000))

; keep-less-than/v2 : [List-of SmallNatural] SmallNatural -> [List-of SmallNatural]
; Produces a list of numbers less than the given number
(check-expect (keep-less-than/v2 LIST-1 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than/v2 LIST-2 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than/v2 LIST-3 50) (cons 43 (cons 12 '())))

(define (keep-less-than/v2 lon num)
  (keep-select lon 0 num))


;;                           Exercise 2
;; ========================================================================
(define BOOL-LIST-0 '())
(define BOOL-LIST-1 (cons #true (cons #false (cons #true '()))))
(define BOOL-LIST-2 (cons #false (cons #false (cons #true (cons #true '())))))
(define BOOL-LIST-3 (cons #true (cons #true (cons #false '()))))
(define BOOL-LIST-4 (cons #false
                          (cons #false
                                (cons #true
                                      (cons #true
                                            (cons #true
                                                  (cons #false
                                                        (cons #true '()))))))))
(define BOOL-LIST-5 (cons #false '()))

; count-trues : [List-of Boolean] -> NonNegInteger
; Produces the number of true values in the list
(check-expect (count-trues BOOL-LIST-1) 2)
(check-expect (count-trues BOOL-LIST-2) 2)
(check-expect (count-trues BOOL-LIST-3) 2)

(define (count-trues lob)
  (cond
    [(empty? lob) 0]
    [(first lob) (+ 1 (count-trues (rest lob)))]
    [else (count-trues (rest lob))]))


; nth-is-true? : [List-of Boolean] NonNegInteger -> Boolean
; Produces true if the nth element of the list is true counting from 0
(check-expect (nth-is-true? BOOL-LIST-1 0) #true)
(check-expect (nth-is-true? BOOL-LIST-1 1) #false)
(check-expect (nth-is-true? BOOL-LIST-1 2) #true)
(check-expect (nth-is-true? BOOL-LIST-2 0) #false)
(check-expect (nth-is-true? BOOL-LIST-2 1) #false)
(check-expect (nth-is-true? '() 0) #false)

(define (nth-is-true? lob n)
  (cond
    [(empty? lob) #false]
    [(= n 0) (first lob)]
    [else (nth-is-true? (rest lob) (- n 1))]))


; first-true : [List-of Boolean] -> Integer
; Produces the index of the first true element in the list and -1 if there is no true element
(check-expect (first-true BOOL-LIST-1) 0)
(check-expect (first-true BOOL-LIST-2) 2)
(check-expect (first-true BOOL-LIST-3) 0)
(check-expect (first-true (cons #false '())) -1)
(check-expect (first-true (cons #false (cons #false '()))) -1)
(check-expect (first-true (cons #false (cons #false (cons #true '())))) 2)
(check-expect (first-true (cons #false (cons #true (cons #false (cons #true '()))))) 1)
(check-expect (first-true (cons #false (cons #false (cons #false '())))) -1)
(check-expect (first-true (cons #false
                                (cons #false
                                      (cons #false (cons #false (cons #true '())))))) 4)


(define (first-true lob)
  (cond 
    [(empty? lob) -1]
    [(first lob) 0]
    [(= -1 (first-true (rest lob))) -1]
    [else (+ 1 (first-true (rest lob)))]))


; should-error? : [List-of Boolean] Boolean -> Boolean
; Produces true if the function should return an error
(check-expect (should-error? BOOL-LIST-1 #true) #true)
(check-expect (should-error? BOOL-LIST-1 #false) #false)
(check-expect (should-error? BOOL-LIST-2 #true) #false)

(define (should-error? lob pred)
  (or (and (first lob) pred) (and (not (first lob)) (not pred))))

; render-error : Boolean -> String
; Produces the error message
(check-expect (render-error #true) "The old value was not #false")
(check-expect (render-error #false) "The old value was not #true")
(check-expect (render-error #true) "The old value was not #false")

(define (render-error pred)
  (string-append "The old value was not " (boolean->string (not pred))))


; set-predicate : [List-of Boolean] NonNegInteger Boolean -> [List-of Boolean]
; n-th item in the list (counting from 0) set to the given boolean or error if it was the same or out 
; of bounds
(check-expect (set-predicate BOOL-LIST-1 1 #true)
              (cons #true  (cons #true (cons #true '()))))
(check-expect (set-predicate BOOL-LIST-2 1 #true)
              (cons #false (cons #true (cons #true (cons #true '())))))
(check-expect (set-predicate BOOL-LIST-1 0 #false)
              (cons #false (cons #false (cons #true '()))))
(check-expect (set-predicate BOOL-LIST-2 2 #false)
              (cons #false (cons #false (cons #false (cons #true '())))))
(check-error (set-predicate '() 10 #true) "IndexOutOfBoundsException")
(check-error (set-predicate '() 0 #true) "IndexOutOfBoundsException")
(check-error (set-predicate BOOL-LIST-1 1 #false) "The old value was not #true")
(check-error (set-predicate BOOL-LIST-1 2 #true) "The old value was not #false")
(check-error (set-predicate BOOL-LIST-2 0 #false) "The old value was not #true")

(define (set-predicate lob n pred)
  (cond
    [(empty? lob) (error "IndexOutOfBoundsException")]
    [(= n 0) (cond
               [(should-error? lob pred) (error (render-error pred))]
               [else (cons pred (rest lob))])]
    [else (cons (first lob) (set-predicate (rest lob) (- n 1) pred))]))


; set-true : [List-of Boolean] NonNegInteger -> [List-of Boolean]
; n-th item in the list (counting from 0) converted from #false
; to #true, error if the old value was not #false)
(check-expect (set-true BOOL-LIST-1 1) (cons #true (cons #true (cons #true '()))))
(check-expect (set-true BOOL-LIST-2 1) (cons #false (cons #true (cons #true (cons #true '())))))
(check-error (set-true '() 1) "IndexOutOfBoundsException")
(check-error (set-true '() 0) "IndexOutOfBoundsException")
(check-error (set-true BOOL-LIST-1 2) "The old value was not #false")
(check-error (set-true BOOL-LIST-2 2) "The old value was not #false")

(define (set-true lob n)
  (set-predicate lob n #true))


; set-false : [List-of Boolean] NonNegInteger -> [List-of Boolean]
; n-th item in the list (counting from 0) converted from #true to #false, error if
; the old value was not #true)
(check-expect (set-false BOOL-LIST-1 0) (cons #false (cons #false (cons #true '()))))
(check-expect (set-false BOOL-LIST-2 2) (cons #false (cons #false (cons #false (cons #true '())))))
(check-error (set-true '() 1) "IndexOutOfBoundsException")
(check-error (set-false BOOL-LIST-1 1) "The old value was not #true")
(check-error (set-false BOOL-LIST-2 0) "The old value was not #true")

(define (set-false lob n)
  (set-predicate lob n #false))


; create-rectangle : Boolean PosInteger -> Image
; Interpretation : Produces an image of a rectangle with the given list of booleans and proportion n:1
(check-expect (create-rectangle #true 1) (rectangle 20 20 "solid" "black"))
(check-expect (create-rectangle #false 1) (rectangle 20 20 "outline" "black"))
(check-expect (create-rectangle #true 4) (rectangle 80 20 "solid" "black"))

(define (create-rectangle b n)
  (rectangle (* n 20) 20 (cond
                           [b "solid"]
                           [else "outline"]) "black"))
             


; draw-map : [List-of Boolean] PosInteger -> Image
; Produces an image of a map with the given list of booleans and proportion n:1
(check-expect (draw-map BOOL-LIST-1 1)
              (beside (rectangle 20 20 "solid" "black")
                      (beside (rectangle 20 20 "outline" "black")
                              (rectangle 20 20 "solid" "black"))))
(check-expect (draw-map BOOL-LIST-2 1)
              (beside (rectangle 20 20 "outline" "black")
                      (beside (rectangle 20 20 "outline" "black")
                              (beside (rectangle 20 20 "solid" "black")
                                      (rectangle 20 20 "solid" "black")))))
(check-expect (draw-map BOOL-LIST-2 4)
              (beside (rectangle 80 20 "outline" "black")
                      (beside (rectangle 80 20 "outline" "black")
                              (beside (rectangle 80 20 "solid" "black")
                                      (rectangle 80 20 "solid" "black")))))
(check-expect (draw-map BOOL-LIST-3 1)
              (beside (rectangle 20 20 "solid" "black")
                      (beside (rectangle 20 20 "solid" "black")
                              (rectangle 20 20 "outline" "black"))))

(define (draw-map lob n)
  (cond
    [(empty? lob) empty-image]
    [else (beside (create-rectangle (first lob) n)
                  (draw-map (rest lob) n))]))

;;                           Exercise 3
;; ========================================================================
; count-total : [List-of Boolean] -> NonNegInteger
; Produces the total number of values in the list
(check-expect (count-total BOOL-LIST-1) 3)
(check-expect (count-total BOOL-LIST-2) 4)
(check-expect (count-total BOOL-LIST-3) 3)

(define (count-total lob)
  (cond
    [(empty? lob) 0]
    [else (+ 1 (count-total (rest lob)))]))


; render : [List-of Boolean] -> Image
; Produces an image of the [List-of Boolean] with the following properties:
; - number of true values
; - visualized as a rectangle
; - the message "[THIS SPACE FOR RENT]"
(check-expect (render BOOL-LIST-1)
              (above
               (text (string-append "True Count: "
                                    (number->string (count-trues BOOL-LIST-1))
                                    " Total Count: "
                                    (number->string (count-total BOOL-LIST-1))) 20 "black")
               (draw-map BOOL-LIST-1 4)
               (text "[THIS SPACE FOR RENT]" 20 "black")))
(check-expect (render BOOL-LIST-2)
              (above (text (string-append "True Count: "
                                          (number->string (count-trues BOOL-LIST-2))
                                          " Total Count: "
                                          (number->string (count-total BOOL-LIST-2))) 20 "black")
                     (draw-map BOOL-LIST-2 4)
                     (text "[THIS SPACE FOR RENT]" 20 "black")))
(check-expect (render BOOL-LIST-3)
              (above (text (string-append "True Count: "
                                          (number->string (count-trues BOOL-LIST-3))
                                          " Total Count: "
                                          (number->string (count-total BOOL-LIST-3))) 20 "black")
                     (draw-map BOOL-LIST-3 4)
                     (text "[THIS SPACE FOR RENT]" 20 "black")))


(define (render lob)
  (above (text (string-append "True Count: "
                              (number->string (count-trues lob))
                              " Total Count: "
                              (number->string (count-total lob))) 20 "black")
         (draw-map lob 4)
         (text "[THIS SPACE FOR RENT]" 20 "black")))


; key-expr : [List-of Boolean] KeyEvent -> [List-of Boolean]
; Checks if the "Q" key is pressed and if so, exits the program
(check-expect (key-expr BOOL-LIST-1 "Q") (stop-with BOOL-LIST-1))
(check-expect (key-expr BOOL-LIST-2 "Q") (stop-with BOOL-LIST-2))
(check-expect (key-expr BOOL-LIST-3 "Q") (stop-with BOOL-LIST-3))
(check-expect (key-expr BOOL-LIST-3 "w") BOOL-LIST-3)

(define (key-expr ws ke)
  (cond
    [(key=? ke "Q") (stop-with ws)]
    [else ws]))

(equal? (key-expr BOOL-LIST-1 "Q") BOOL-LIST-1)
(equal? (key-expr BOOL-LIST-3 "w") BOOL-LIST-3)


; compute-shift : [List-of Boolean] -> NonNegInteger
; Produces the shift value for the mouse fliping of rectangles
(define (compute-shift ws)
  (quotient (max (- (image-width (render ws)) (image-width (draw-map ws 4))) 0) 2))


; mouse-expr : [List-of Boolean] Integer Integer MouseEvent -> [List-of Boolean]
; Flips the value of the clicked rectangle
(check-expect (mouse-expr BOOL-LIST-1 50 50 "button-down") BOOL-LIST-1)  
(check-expect (mouse-expr BOOL-LIST-1 50 20 "button-down") BOOL-LIST-1)  
(check-expect (mouse-expr BOOL-LIST-1 160 30 "move") BOOL-LIST-1)        
(check-expect (mouse-expr BOOL-LIST-2 160 30 "button-down") 
              (set-predicate BOOL-LIST-2 (quotient 160 80) #false))
(check-expect (mouse-expr BOOL-LIST-1 160 100 "button-down") BOOL-LIST-1)
(check-expect (mouse-expr BOOL-LIST-5 0 30 "button-down") BOOL-LIST-5)
(check-expect (mouse-expr '() 5 30 "button-down") '()) 



(define (mouse-expr ws x y me)
  (cond
    [(> y 41) ws]
    [(< y 22) ws]
    [(< x (compute-shift ws)) ws]
    [(> x (+ (compute-shift ws) (image-width (draw-map ws 4)))) ws]
    [(mouse=? me "button-down")
     (set-predicate ws (quotient (- x (compute-shift ws)) 80)
                    (not (nth-is-true? ws (quotient (- x (compute-shift ws)) 80))))]
    [else ws]))

; bit-bucket : [List-of Boolean] -> [List-of Boolean]
; Interpretation: Produces a visual representation of the list
(define (bit-bucket lob)
  (big-bang lob
    [to-draw render]
    [on-key key-expr]
    [on-mouse mouse-expr]
    [close-on-stop #true]))

;;; (bit-bucket '())
;;; (bit-bucket BOOL-LIST-4)
;;; (bit-bucket BOOL-LIST-5)