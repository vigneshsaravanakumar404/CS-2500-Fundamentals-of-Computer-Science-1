#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

(define LIST-1 (cons 1 (cons 2 (cons 3 '()))))
(define LIST-2 (cons 1 (cons 2 (cons 3 (cons 4 '())))))
(define LIST-3 (cons 43 (cons 70 (cons 12 '()))))
;; ===============================================================================================
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

; count-trues : [List-of Boolean] -> Number
; Produces the number of true values in the list
(check-expect (count-trues BOOL-LIST-1) 2)
(check-expect (count-trues BOOL-LIST-2) 2)
(check-expect (count-trues BOOL-LIST-3) 2)

(define (count-trues lob)
  (cond
    [(empty? lob) 0]
    [(first lob) (+ 1 (count-trues (rest lob)))]
    [else (count-trues (rest lob))]))


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


; set-predicate : [List-of Boolean] Number Boolean -> [List-of Boolean]
; n-th item in the list (counting from 0) set to the given boolean or error if it was the same
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
    [(empty? lob) (cond
                    [(>= n 0) (error "IndexOutOfBoundsException")]
                    [else '()])]
    [(= n 0) (cond
               [(should-error? lob pred) (error (render-error pred))]
               [else (cons pred (rest lob))])]
    [else (cons (first lob) (set-predicate (rest lob) (- n 1) pred))]))


; set-true : [List-of Boolean] Number -> [List-of Boolean]
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


; set-false : [List-of Boolean] Number -> [List-of Boolean]
; n-th item in the list (counting from 0) converted from #true to #false, error if
; the old value was not #true)
(check-expect (set-false BOOL-LIST-1 0) (cons #false (cons #false (cons #true '()))))
(check-expect (set-false BOOL-LIST-2 2) (cons #false (cons #false (cons #false (cons #true '())))))
(check-error (set-true '() 1) "IndexOutOfBoundsException")
(check-error (set-false BOOL-LIST-1 1) "The old value was not #true")
(check-error (set-false BOOL-LIST-2 0) "The old value was not #true")

(define (set-false lob n)
  (set-predicate lob n #false))


; create-rectangle : Boolean Number -> Image
; Interpretation : Produces an image of a rectangle with the given list of booleans and proportion n:1
(check-expect (create-rectangle #true 1) (rectangle 20 20 "solid" "black"))
(check-expect (create-rectangle #false 1) (rectangle 19 19 "outline" "black"))
(check-expect (create-rectangle #true 4) (rectangle 80 20 "solid" "black"))

(define (create-rectangle b n)
  (rectangle (cond
               [b (* n 20)]
               [else (+ -1 (* n 20))])
             (cond
               [b 20]
               [else 19])
             (cond
               [b "solid"]
               [else "outline"]) "black"))


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
    [else (beside (create-rectangle (first lob) n)
                  (draw-map (rest lob) n))]))