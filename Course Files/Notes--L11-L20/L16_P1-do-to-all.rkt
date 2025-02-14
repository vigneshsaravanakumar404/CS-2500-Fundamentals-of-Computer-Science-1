;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L15_P3-abst-funcs-b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Congratulations! But what can we do with this new superpower?

; Recall our data definition for Position and ListOfPosns

; A Position is a (make-posn Number Number)
; representing a 2D position
 
(define POSN-0 (make-posn 0 0))
(define POSN-1 (make-posn 3 4))
 
(define (posn-temp p)
  (... (posn-x p) ...
       (posn-y p) ...))
 
; A ListOfPosns (LoP) is one of:
; - '()
; - (cons Position LoP)
; representing a list of Positions
 
(define LOP-0 '())
(define LOP-1 (cons POSN-0 '()))
(define LOP-2 (cons POSN-1 LOP-1))
 
(define (lop-temp lop)
  (...
   (cond [(empty? lop) ...]
         [(cons? lop) (... (posn-temp (first lop)) ...
                           (lop-temp (rest lop)) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Side A:
;   Write manhattan-dist-all that accepts a ListofPosns and computes the
;   Manhattan distance of all of them.
;
; Side B:
;   Write euclidean-dist-all that accepts a ListOfPosns and computes the
;   Euclidean distance of all of them.

; We are providing helper functions manhattan-dist and euclidean-dist

;<DO NOW><SCROLL>





; manhattan-dist-all : LoP -> LoN
; Compute the manhattan distance to the origin of all positions
 
(check-expect (manhattan-dist-all LOP-0) LOP-0)
(check-expect (manhattan-dist-all LOP-2)
              (cons 7 (cons 0 '())))
 
(define (manhattan-dist-all lop)
  ;<DO NOW> Helper: manhattan-dist<DONE>




  )
 
; manhattan-dist : Position -> Number
; Computes the manhattan distance to the origin
 
(check-expect (manhattan-dist POSN-0) 0)
(check-expect (manhattan-dist POSN-1) 7)
 
(define (manhattan-dist p)
  (+ (abs (posn-x p))
     (abs (posn-y p))))


; euclidean-dist-to-all : LoP -> LoN
; Compute the Euclidean distance to the origin of all positions
 
(check-expect (euclidean-dist-to-all LOP-0) LOP-0)
(check-expect (euclidean-dist-to-all LOP-2)
              (cons 5 (cons 0 '())))
 
(define (euclidean-dist-to-all lop)
  ;<DO NOW> Helper: euclidean-dist<DONE>




  )
 
; euclidean-dist : Position -> Number
; Computes the Euclidean distance to the origin
 
(check-expect (euclidean-dist POSN-0) 0)
(check-expect (euclidean-dist POSN-1) 5)
 
(define (euclidean-dist p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Now abstract those. Where are the differences?
; Similar to previous exercise, can we leverage effort there? REALLY leverage??
;<BRAINSTORM>






; We can reuse do-to-all!!!
; But does do-to-all care about what data type is in the list?

; do-to-all : LoN Function-that-accepts-a-Number-and-returns-a-Number -> LoN
; Applies the second argument to each of the numbers
 
(define (do-to-all lon f)
  (cond [(empty? lon) '()]
        [(cons? lon) (cons (f (first lon))
                           (do-to-all (rest lon) f))]))

; Our signature says it does, but let's try it anyway.
; Rewrite those two in terms of do-to-all:
;<DO NOW><SCROLL>







(define (manhattan-dist-all lop)
  (do-to-all lop manhattan-dist))

(define (euclidean-dist-to-all lop)
  (do-to-all lop euclidean-dist))

; Note: signatures for helpers are:
; euclidean-dist : Position -> Number
; euclidean-dist : Position -> Number

; This suggests that our previous signature for do-to-all might be a bit too restrictive.
; Think about that for next time.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Optional Exercises:
;
; Exercise: abstract the following two functions

;; beside-all : ListOfImages -> Image
(define (beside-all images)
    (cond
    [(empty? images) empty-image]
    [(cons? images) (beside (first images) (beside-all (rest images)))]))

;; overlay-all : ListOfImages -> Image
(define (overlay-all images)
    (cond
    [(empty? images) empty-image]
    [(cons? images) (overlay (first images) (overlay-all (rest images)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's re-examine the signature for do-to-all: it applies another
; function, passed in as a parameter, to every element of a list.
; We initially threw up the signature:

; do-to-all : ListOfNumbers ??? -> ListOfNumbers

; What should ??? be? Well, when we created the function, we were abstracting the two
; similar functions sqr-of-all and srqt-of-all. Those functions wanted to apply sqr
; or sqrt to each number in the list; in other words, do-to-all was being passed a function
; that would take a number, and process it and return a number. Here's a first pass at
; a better signature:

; do-to-all : ListOfNumbers Function-that-accepts-a-Number-and-returns-a-Number -> ListOfNumbers

; Is there a better notation? How about borrowing from the style of the entire signature itself?

; do-to-all : ListOfNumbers [Number -> Number] -> ListOfNumbers
;<QUESTION>: why the square brackets??

; Better. But is this the best data definition? Let's take a look at the function:

(define (do-to-all lon f)
  (cond [(empty? lon) '()]
        [(cons? lon) (cons (f (first lon))
                           (do-to-all (rest lon) f))]))

; Is there anything in this function that requires that lon is a ListOfNumbers? Well,
; f does. However, that comes from outside do-to-all. Whatever f takes as an argument,
; is what the elements in lon should be.
; What about the return value? Does it have to be a ListofNumbers? In fact, does it have
; to be the same kind of list as the input list?
; If not, how do we express what it must be?
;<BRAINSTORM><SCROLL>






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A DIGRESSION OF CONVENIENCE

; Before we can resolve the bigger problem at hand, we need a more general way
; to talk about, and refer to, various types of lists (i.e., lists containing
; different types of things). Instead of referring to ListofNumbers and ListofPositions,
; Let's use the references  [List-of Number] and [List-of Position], or more generally,
; [List-of X], where X is some particular type. But what is a [List-of X]?
; Let's look at two of our past list data definitions:
; We already noticed that all of our ListofSomething's are 90.7% similar:

; A ListOfNumber is one of:
; - '()
; - (cons Number ListOfNumber)
 
; A ListOfPosition is one of:
; - '()
; - (cons Position ListOfPosition)

; The only differences between these is the type of the indiviual elements of the list.
; Let's abstract these data definitions:

; A [List-of X] is one of:
; - '()
; - (cons X [List-of X])

; We can then "invoke" this abstract data definition, by merely referring to
; a [List-of Number], or a [List-of String], or a [List-of-Position], or any other type!
; (Notice we use brackets to notate we are using an abstracted data definition.]

; We now no longer need to separate data design for every kind of list.
; No more data definition, interpretation, examples, or templates--yay!!
; (...Unless we explicitly ask you to, in which case... yeah.
;  ...Or it's useful, like examples to reuse in tests, etc..
;  ...And we will still be constantly referring back to list templates...
;  So, maybe delay throwing all that stuff away for a bit??)

; Note: this feature doesn't take effect until future assignments, so you can't use
; it for the current assignments :-(

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Note the difference between [List-of X] and ...

; A BadList is one of
; - '()
; - (cons Any BadList)

; A [List-of X] can only contain values of type X, whereas a BadList can mix-and-match
; different values. This is rarely what we want, and so, would be usually wrong (but not always...).
; (Note that BadList here is would be the same as [List-of Any].)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; As another example, consider a non-empty list...

; A [NEList-of X] is one of:
; - (cons X '())
; - (cons X [NEList-of X])

; Here are some examples of a non-empty list of numbers:

(define NELON-1 (cons 1 '()))
(define NELON-2 (cons 2 NELON-1))

; What about the template?

(define (nelon-temp nelon)
  (...
   (cond [(empty? (rest nelon)) (... (first nelon) ...)]
         [(cons? (rest nelon)) (... (first nelon) ...
                                    (nelon-temp (rest nelon)) ...)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Going back to perfecting the signature for do-to-all :
; How about:

; do-to-all : [List-of Any] [Any -> Any] -> [List-of Any]
;<DISCUSS>






; Better version:

; do-to-all : [List-of X] [X -> Y] -> [List-of Y]

; What are some issues with this?
;<DISCUSS>






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We also need to indicate that X and Y are data definitions!
; When you go to use do-to-all, you need to have a particular X and Y in mind.

; Best version:

; do-to-all : (X Y) [List-of X] [X -> Y] -> [List-of Y]

