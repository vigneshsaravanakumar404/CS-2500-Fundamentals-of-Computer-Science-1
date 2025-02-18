;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;                             Exercise 1
;; ========================================================================
(define LIST-1 (cons 1 (cons 2 (cons 3 '()))))
(define LIST-2 (cons 1 (cons 2 (cons 3 (cons 4 '())))))
(define LIST-3 (cons 43 (cons 70 (cons 12 '()))))


; keep-greater-than : [List-of Number] Number -> [List-of Number]
; Produces a list of numbers greater or equal to the given number
(check-expect (keep-greater-than LIST-1 2) (cons 2 (cons 3 '())))
(check-expect (keep-greater-than LIST-2 2) (cons 2 (cons 3 (cons 4 '()))))
(check-expect (keep-greater-than LIST-3 50) (cons 70 '()))

(define (keep-greater-than lon num)
  (cond
    [(empty? lon) '()]
    [(>= (first lon) num) (cons (first lon) (keep-greater-than (rest lon) num))]
    [else (keep-greater-than (rest lon) num)]))


; keep-less-than : [List-of Number] Number -> [List-of Number]
; Produces a list of numbers less than the given number
(check-expect (keep-less-than LIST-1 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than LIST-2 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than LIST-3 50) (cons 43 (cons 12 '())))

(define (keep-less-than lon num)
  (cond
    [(empty? lon) '()]
    [(<= (first lon) num) (cons (first lon) (keep-less-than (rest lon) num))]
    [else (keep-less-than (rest lon) num)]))


; keep-select: [List-of Number] Number Number Nuber -> [List-of Number]
; Produces a list of numbers that satisfy the given modifiers
(check-expect (keep-select (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))) 3 1 -1) (cons 3 (cons 4 (cons 5 '()))))  ;; num >= x  (3 >= x)
(check-expect (keep-select (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))) 3 1 0) (cons 4 (cons 5 '())))   ;; num > x   (3 > x)
(check-expect (keep-select (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))) 3 -1 -1) (cons 1 (cons 2 (cons 3 '())))) ;; num <= x  (3 <= x)
(check-expect (keep-select (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))) 3 -1 0) (cons 1 (cons 2 '())))  ;; num < x   (3 < x)
;;; (check-expect (keep-select (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))) 3 0 0) (cons 3 '())) ;; num = x (3 = x)

; /fix/ make 0 work
(define (keep-select lon num flip comp-type)
  (cond
    [(empty? lon) '()]
    [(> (+ (* -1 flip num) (* flip (first lon))) comp-type)
     (cons (first lon) (keep-select (rest lon) num flip comp-type))]
    [else (keep-select (rest lon) num flip comp-type)]))




; N/FIX/ Did i keep the right parts of the design recipe?
(check-expect (keep-greater-than/v2 LIST-1 2) (cons 2 (cons 3 '())))
(check-expect (keep-greater-than/v2 LIST-2 2) (cons 2 (cons 3 (cons 4 '()))))
(check-expect (keep-greater-than/v2 LIST-3 50) (cons 70 '()))

(define (keep-greater-than/v2 lon num)
  (keep-select lon num 1 -1))

(check-expect (keep-less-than/v2 LIST-1 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than/v2 LIST-2 2) (cons 1 (cons 2 '())))
(check-expect (keep-less-than/v2 LIST-3 50) (cons 43 (cons 12 '())))

(define (keep-less-than/v2 lon num)
  (keep-select lon num -1 -1))


;;                           Exercise 2
;; ========================================================================
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


; nth-is-true? : [List-of Boolean] Number -> Boolean
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


; first-true-helper : [List-of Boolean] Number -> Number
; Produces the index of the first true element in the list and -1 if there is no true element
(check-expect (first-true-helper BOOL-LIST-1 0) 0)
(check-expect (first-true-helper BOOL-LIST-2 0) 2)
(check-expect (first-true-helper BOOL-LIST-3 0) 0)
(check-expect (first-true-helper (cons #false '()) 0) -1)

(define (first-true-helper lob n)
  (cond
    [(empty? lob) -1]
    [(first lob) n]
    [else (first-true-helper (rest lob) (+ n 1))]))

; first-true : [List-of Boolean] -> Number
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
  (first-true-helper lob 0))

#| Doing it without a Helper function
(define (first-true lob)
  (cond 
    [(empty? lob) -1]
    [(first lob) 0]
    [(= -1 (first-true (rest lob))) -1]
    [else (+ 1 (first-true (rest lob)))])) |#


;; /FIX/ What does limit unnecessary recursion mean?
; set-predicate : [List-of Boolean] Number Boolean -> [List-of Boolean]
; n-th item in the list (counting from 0) set to the given boolean or
; error if the old value was the same
(check-expect (set-predicate BOOL-LIST-1 1 #true)
              (cons #true  (cons #true (cons #true '()))))
(check-expect (set-predicate BOOL-LIST-2 1 #true)
              (cons #false (cons #true (cons #true (cons #true '())))))
(check-expect (set-predicate BOOL-LIST-1 0 #false)
              (cons #false (cons #false (cons #true '()))))
(check-expect (set-predicate BOOL-LIST-2 2 #false)
              (cons #false (cons #false (cons #false (cons #true '())))))
(check-expect (set-predicate '() 10 #true) '())
(check-error (set-predicate BOOL-LIST-1 1 #false) "The old value was not #true")
(check-error (set-predicate BOOL-LIST-1 2 #true) "The old value was not #false")
(check-error (set-predicate BOOL-LIST-2 0 #false) "The old value was not #true")

(define (set-predicate lob n pred)
  (cond
    [(empty? lob) '()]
    [(= n 0) (cond
               [(or (and (first lob) pred) (and (not (first lob)) (not pred)))
                (error (string-append "The old value was not " (boolean->string (not pred))))]
               [else (cons pred (rest lob))])]
    [else (cons (first lob) (set-predicate (rest lob) (- n 1) pred))]))


; set-true : [List-of Boolean] Number -> [List-of Boolean]
; n-th item in the list (counting from 0) converted from #false
; to #true, error if the old value was not #false)
(check-expect (set-true BOOL-LIST-1 1) (cons #true (cons #true (cons #true '()))))
(check-expect (set-true BOOL-LIST-2 1) (cons #false (cons #true (cons #true (cons #true '())))))
(check-error (set-true BOOL-LIST-1 2) "The old value was not #false")
(check-error (set-true BOOL-LIST-2 2) "The old value was not #false")

(define (set-true lob n)
  (set-predicate lob n #true))


; set-false : [List-of Boolean] Number -> [List-of Boolean]
; n-th item in the list (counting from 0) converted from #true to #false, error if
; the old value was not #true)
(check-expect (set-false BOOL-LIST-1 0) (cons #false (cons #false (cons #true '()))))
(check-expect (set-false BOOL-LIST-2 2) (cons #false (cons #false (cons #false (cons #true '())))))
(check-error (set-false BOOL-LIST-1 1) "The old value was not #true")
(check-error (set-false BOOL-LIST-2 0) "The old value was not #true")

(define (set-false lob n)
  (set-predicate lob n #false))



; draw-map : [List-of Boolean] Number -> Image
; Produces an image of a map with the given list of booleans and proportion n:1
(check-expect (draw-map BOOL-LIST-1 1)
              (beside (rectangle 20 20 "solid" "black")
                      (beside (rectangle 19 19 "outline" "black")
                              (rectangle 20 20 "solid" "black"))))
(check-expect (draw-map BOOL-LIST-2 1)
              (beside (rectangle 19 19 "outline" "black")
                      (beside (rectangle 19 19 "outline" "black")
                              (beside (rectangle 20 20 "solid" "black")
                                      (rectangle 20 20 "solid" "black")))))
(check-expect (draw-map BOOL-LIST-2 4)
              (beside (rectangle 79 19 "outline" "black")
                      (beside (rectangle 79 19 "outline" "black")
                              (beside (rectangle 80 20 "solid" "black")
                                      (rectangle 80 20 "solid" "black")))))
(check-expect (draw-map BOOL-LIST-3 1)
              (beside (rectangle 20 20 "solid" "black")
                      (beside (rectangle 20 20 "solid" "black")
                              (rectangle 19 19 "outline" "black"))))

(define (draw-map lob n)
  (cond
    [(empty? lob) empty-image]
    [else (beside (rectangle (cond
                               [(first lob) (* n 20)]
                               [else (+ -1 (* n 20))])
                             (cond
                               [(first lob) 20]
                               [else 19])
                             (cond
                               [(first lob) "solid"]
                               [else "outline"]) "black")
                  (draw-map (rest lob) n))]))

;;                           Exercise 3
;; ========================================================================

;; /FIX/ What does truly exit mean?
;; /FIX/ Where do big-bang helper functions go?


; count-true : [List-of Boolean] -> Number
; Produces the number of true values in the list
(check-expect (count-true BOOL-LIST-1) 2)
(check-expect (count-true BOOL-LIST-2) 2)
(check-expect (count-true BOOL-LIST-3) 2)

(define (count-true lob)
  (cond
    [(empty? lob) 0]
    [(first lob) (+ 1 (count-true (rest lob)))]
    [else (count-true (rest lob))]))

; count-total : [List-of Boolean] -> Number
; Produces the total number of values in the list
(check-expect (count-total BOOL-LIST-1) 3)
(check-expect (count-total BOOL-LIST-2) 4)
(check-expect (count-total BOOL-LIST-3) 3)

(define (count-total lob)
  (cond
    [(empty? lob) 0]
    [else (+ 1 (count-total (rest lob)))]))


; render : World State -> Image
; Produces an image of the world state with the following properties:
; - number of true values
; - visualized as a rectangle
; - the message "[THIS SPACE FOR RENT]"
(check-expect (render BOOL-LIST-1)
              (above
               (text (string-append "True Count: "
                                    (number->string (count-true BOOL-LIST-1))
                                    " Total Count: "
                                    (number->string (count-total BOOL-LIST-1))) 20 "black")
               (draw-map BOOL-LIST-1 4)
               (text "[THIS SPACE FOR RENT]" 20 "black")))
(check-expect (render BOOL-LIST-2)
              (above (text (string-append "True Count: "
                                          (number->string (count-true BOOL-LIST-2))
                                          " Total Count: "
                                          (number->string (count-total BOOL-LIST-2))) 20 "black")
                     (draw-map BOOL-LIST-2 4)
                     (text "[THIS SPACE FOR RENT]" 20 "black")))
(check-expect (render BOOL-LIST-3)
              (above (text (string-append "True Count: "
                                          (number->string (count-true BOOL-LIST-3))
                                          " Total Count: "
                                          (number->string (count-total BOOL-LIST-3))) 20 "black")
                     (draw-map BOOL-LIST-3 4)
                     (text "[THIS SPACE FOR RENT]" 20 "black")))
(define (render BOOL-LIST-4)
  (above (text (string-append "True Count: "
                              (number->string (count-true BOOL-LIST-4))
                              " Total Count: "
                              (number->string (count-total BOOL-LIST-4))) 20 "black")
         (draw-map BOOL-LIST-4 4)
         (text "[THIS SPACE FOR RENT]" 20 "black")))


; key-expr : World State KeyEvent -> World State
; Checks if the "q" key is pressed and if so, exits the program
(check-expect (key-expr BOOL-LIST-1 "q") (stop-with BOOL-LIST-1))
(check-expect (key-expr BOOL-LIST-2 "q") (stop-with BOOL-LIST-2))
(check-expect (key-expr BOOL-LIST-3 "q") (stop-with BOOL-LIST-3))
(check-expect (key-expr BOOL-LIST-3 "w") BOOL-LIST-3)

(define (key-expr ws ke)
  (cond
    [(key=? ke "q") (stop-with ws)]
    [else ws]))

; mouse-expr : World State Integer Integer MouseEvent -> World State
; Flips the value of the clicked rectangle
(check-expect (mouse-expr BOOL-LIST-1 50 50 "button-down") BOOL-LIST-1)  
(check-expect (mouse-expr BOOL-LIST-1 50 20 "button-down") BOOL-LIST-1)  
(check-expect (mouse-expr BOOL-LIST-1 160 30 "move") BOOL-LIST-1)        
(check-expect (mouse-expr BOOL-LIST-2 160 30 "button-down") 
              (set-predicate BOOL-LIST-2 (quotient 160 80) #false)) 
(check-expect (mouse-expr BOOL-LIST-3 80 30 "button-down") 
              (set-predicate BOOL-LIST-3 (quotient 80 80) #false))

(define (mouse-expr ws x y me)
  (cond
    [(> y 41) ws]
    [(< y 22) ws]
    [(mouse=? me "button-down") (set-predicate ws (quotient x 80)
                                               (not (nth-is-true? ws (quotient x 80))))]
    [else ws]))


; bit-bucket : [List-of Number] -> World State
; Produces a world state that is a bit-bucket
(define (bit-bucket lob)
  (big-bang lob
    (to-draw render)
    (on-key key-expr)
    (on-mouse mouse-expr)
    (close-on-stop #true)))