;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L16_P2-abst-funcs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; More advanced functional abstraction
;
; A ListOfNumbers (LoN) is one of
; - '()
; - (cons Number LoN)
; Interpretation: ...
 
(define LON-0 '())
(define LON-1 (cons 9 LON-0))
(define LON-2 (cons 4 LON-1))
 
(define (lon-temp lon)
  (...
   (cond [(empty? lon) ...]
         [(cons? lon) (... (first lon) ...
                           (lon-temp (rest lon)) ...)])))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Side A:
;   Write sqrt-of-all that accepts a ListofNumbers and computes the
;   square root of all of them.
;
; Side B:
;   Write sqr-of-all that accepts a ListofNumbers and computes the
;   square of all of them.
 
; sqrt-of-all : LoN -> LoN
; Takes the square root of all numbers
 
(check-expect (sqrt-of-all LON-0) '())
(check-expect (sqrt-of-all LON-2)
              (cons 2 (cons 3 '())))
 
#;
(define (sqrt-of-all lon)
  ;<DO NOW>

    


  )

; sqr-of-all : LoN -> LoN
; Squares all the numbers
 
(check-expect (sqr-of-all LON-0) '())
(check-expect (sqr-of-all LON-2)
              (cons 16 (cons 81 '())))
 
#;
(define (sqr-of-all lon)
  ;<DO NOW>

    


  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Again, let's examine the two functions and see where they are the same,
; and where they differ. Then, let's apply our abstraction process.
;
; Spot the differences between the functions
; Create an abstracted function where the differences are a parameter
; Re-write the original function to use the abstracted function.

;<BRAINSTORM>
; Hmm... what is very different here from our previous exercise??
; Let's try what we can anyway.
;<DO NOW><SCROLL>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; do-to-all : LoN ??? -> LoN
; Applies the second argument to each of the numbers
; But what should the second parameter be??
; <BRAINSTORM><SCROLL>





 
(check-expect (do-to-all LON-0 add1) '())
(check-expect (do-to-all LON-2 add1) (cons 5 (cons 10 '())))
(check-expect (do-to-all LON-2 sqr) (cons 16 (cons 81 '())))
(check-expect (do-to-all LON-2 sqrt) (cons 2 (cons 3 '())))

#;
(define (do-to-all ...)
  ; Start w/template:
  (...
   (cond [(empty? lon) ...]
         [(cons? lon) (... (first lon) ...
                           (do-to-all (rest lon)) ...)])))
;<BRAINSTORM><SCROLL>




(define (do-to-all lon func-to-use)
  (cond [(empty? lon) '()]
        [(cons? lon) (cons (func-to-use (first lon))
                           (do-to-all (rest lon) func-to-use))]))



; So, we're trying to pass a function as a parameter. Is that even allowed??
; Let's try...
;<RUN PROGRAM>
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GRADUATION!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now, we can pass functions as parameters to function calls!

; What is the signature of do-to-all? How can we express what f has to be? Well, it's a function,
; so it accepts something and returns something. What does it have to accept? Just a Number.
; What does it have to return? For now, another Number. Thus...

; do-to-all : LoN Function-that-accepts-a-Number-and-returns-a-Number -> LoN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Now we have our abstracted do-to-all. Let's re-write sqrt-of-all and sqr-of-all using do-to-all.
;<DO NOW><SCROLL>
(define (sqrt-of-all lon)
  (do-to-all lon sqrt))

(define (sqr-of-all lon)
  (do-to-all lon sqr))

