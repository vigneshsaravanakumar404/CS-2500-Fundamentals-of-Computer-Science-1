;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname L19_P1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; In the previous lecture, we saw two ISL-provided abstractions that were familiar: map and filter.
; We also created collapse as well – does ISL have an analog for that, too?
; Also, are there other common function patterns that ISL provides abstractions for?

; To complete the list, let's create a simple game.

; WORLDOFFORTUNE (apologies to Pat Sajak and Vanna White)

; We're going to heavily take advantage of ISL-provided abstractions to build a
; Wheel of Fortune game. It's a TV game show, in which a secret phrase is
; shown as  sequence of hidden blocks, and the players repeatedly guess letters,
; revealing correct ones, until they can guess the phrase.
; 
; To keep our version simple, we will use a single word, and the program ends
; when all of the letters have been guessed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 2htdp/image)
(require 2htdp/universe)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Let's start by creating the data definition. We need to store two things:
; what the character is, and whether or not it has been guessed. We will then
; join these up in a list. There are a number of ways to do this.
;<BRAINSTORM>







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Here's one:

(define-struct guessed [char])
(define-struct unguessed [char])

; A Game1String is one of:
; - (make-guessed 1String)
; - (make-unguessed 1String)
; A 1String in the word, either it has been guessed already or it is still hidden
 
(define G1S-1 (make-guessed "a"))
(define G1S-2 (make-unguessed "a"))
 
(define (g1s-temp g1s)
  (...
   (cond [(guessed? g1s) (... (guessed-char g1s) ...)]
         [(unguessed? g1s) (... (unguessed-char g1s) ...)])))
 
; A WorldOfFortune (WoF) is a [List-of Game1String]
; The current state of the game
 
(define WOF-0 (list (make-unguessed "h")
                    (make-unguessed "e")
                    (make-unguessed "l")
                    (make-unguessed "l")
                    (make-unguessed "o")))
 
(define WOF-1 (list (make-unguessed "h")
                    (make-guessed "e")
                    (make-unguessed "l")
                    (make-unguessed "l")
                    (make-unguessed "o")))
 
(define WOF-2 (list (make-unguessed "h")
                    (make-guessed "e")
                    (make-guessed "l")
                    (make-guessed "l")
                    (make-unguessed "o")))
 
(define WOF-3 (list (make-guessed "h")
                    (make-guessed "e")
                    (make-guessed "l")
                    (make-guessed "l")
                    (make-unguessed "o")))
 
(define WOF-4 (list (make-guessed "h")
                    (make-guessed "e")
                    (make-guessed "l")
                    (make-guessed "l")
                    (make-guessed "o")))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Next, design our main function guessing-game. It should take a string as the word
; to be guessed.
;
; How could we convert this to a WoF?
; Hint: explode takes a string and produces a list of 1Strings, and I think we can convert
; it the rest of the way with abstractions that we've seen. 

; guessing-game : String -> WoF
; Run the world of fortune game on s
 
(define (guessing-game s)
  (big-bang (<some-list-abstr> ... (explode s))
    ;<DO NOW>
    [to-draw draw-wof]
    [stop-when all-guessed? draw-wof]
    [on-key handle-key]))




; This creates 3 new functions for our wishlist:
; - draw-wof
; - all-guessed?
; - handle-key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's design all-guessed? first

; all-guessed? : WoF -> Boolean
; Have all the characters been guessed?
 
(check-expect (all-guessed? WOF-0) #false)
(check-expect (all-guessed? WOF-1) #false)
(check-expect (all-guessed? WOF-4) #true)
 
(define (all-guessed?/v1 wof)
  ;<DO NOW><SCROLL> Start from template:
  #;
  (cond [(empty? wof) ...]
        [(cons? wof) (... (first wof) ...
                          (all-guessed?/v1 (rest wof)))])







  
  (cond [(empty? wof) #true]
        [(cons? wof) (and (guessed? (first wof))
                          (all-guessed?/v1 (rest wof)))])))

; This looks like a very useful pattern: determining if a given function returns #true for
; every element in the list.
; In this case, whether all things are a make-guessed structure using the guessed? function.

; Let's design a abstract function that follows this pattern.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design true-for-all? which tests if a predicate is true for all elements in a list

; true-for-all? : ??? <DO NOW>
; Returns whether the predicate is true for all elements of the list
 
(check-expect (true-for-all? (list 1 2 3) positive?) #true)
(check-expect (true-for-all? (list) positive?) #true)
(check-expect (true-for-all? (list 1 2 -3) positive?) #false)
(check-expect (true-for-all? (list "1" 1 "one") string?) #false)
(check-expect (true-for-all? (list "1" "one") string?) #true)
 
(define (true-for-all? lox p?)
  ;<DO NOW><SCROLL> Start from template:
  #;
  (cond [(empty? lox) ...]
        [(cons? lox) (... (first lox) ...
                          (true-for-all? (rest lox) p?))])







  
  (cond [(empty? lox) #true]
        [(cons? lox) (and (p? (first lox))
                          (true-for-all? (rest lox) p?))]))

; This should look similar to designs we've seen before: it's essentially a map of
; a predicate over a list, and then a collapse of the result using and.
; It's useful enough that it's already provided by ISL as andmap.

; (X) [X -> Boolean] [List-of X] -> Boolean
; determines whether p holds for every item on lx
; (andmap p (list x-1 ... x-n)) == (and (p x-1) ... (p x-n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Rewrite all-guessed? using andmap:

 (define (all-guessed? wof)
   ;<DO NOW>



   
   ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Analogous to andmap, ISL provides ormap, which returns whether or not a
; given function is #true for at least one element of a list.

; Let's write it as true-for-any? :

; true-for-any? : (X) [List-of X] [X -> Boolean] -> Boolean
; Returns whether the predicate is true for any element of the list
 
(check-expect (true-for-any? (list 1 2 3) positive?) #true)
(check-expect (true-for-any? (list) positive?) #false)
(check-expect (true-for-any? (list -1 2 -3) positive?) #true)
(check-expect (true-for-any? (list -1 -2 -3) positive?) #false)
(check-expect (true-for-any? (list 1 "one" 1.0) string?) #true)
(check-expect (true-for-any? (list 1 1.0) string?) #false)
 
(define (true-for-any? lox p?)
  (cond [(empty? lox) #false]
        [(cons? lox) (or (p? (first lox))
                         (true-for-any? (rest lox) p?))]))

; Again, this is essentially a map and then an or; ISL calls it ormap.

; (X) [X -> Boolean] [List-of X] -> Boolean
; determines whether p holds for at least one item on lx
; (ormap p (list x-1 ... x-n)) == (or (p x-1) ... (p x-n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FOLDR AND FOLDL

; The next function in our WheelOFortune wishlist is draw-wof.
; It needs to take a [List-of Game1String] and collapse it into an image.

; Recall collapse from the previous lecture:

; collapse : (X Y) [List-of X] [X Y -> Y] Y -> Y
; Iteratively applies the function to the elements and returns the result
 
(define (collapse lox op base)
  (cond [(empty? lox) base]
        [(cons? lox) (op (first lox)
                         (collapse (rest lox) op base))]))

; ISL provides exactly this function as foldr:

; (X Y) [X Y -> Y] Y [List-of X] -> Y
; applies f from right to left to each item in lx and b
; (foldr f b (list x-1 ... x-n)) == (f x-1 ... (f x-n b))

; Here's a simple example:

; concatenate : [List-of String] -> String
; puts all the strings together

(check-expect (concatenate (list)) "")
(check-expect (concatenate (list "a " "b" "c")) "a bc")
 
(define (concatenate los)
  (foldr string-append "" los))

; What would following do:
(foldr cons '() (list "a " "b" "c"))
;<DISCUSS>




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; foldl is the same as foldr but processes the list in reverse.

; (X Y) [X Y -> Y] Y [List-of X] -> Y
; applies f from left to right to each item in lx and b
; (foldl f b (list x-1 ... x-n)) == (f x-n ... (f x-1 b))

; An example:

; concatenate-backwards : [List-of String] -> String
; puts all the strings together in the opposite order they appear

(check-expect (concatenate-backwards (list)) "")
(check-expect (concatenate-backwards (list "a " "b" "c")) "cba ")
 
(define (concatenate-backwards los)
  (foldl string-append "" los))


; What would following do:
(foldl cons '() (list "a " "b" "c"))
;<DISCUSS>




; Note: foldr/foldl has some subtleties. Try:
(foldr + 0 (list 1 2 3 4))
(foldl + 0 (list 1 2 3 4))
(foldr - 0 (list 1 2 3 4))
(foldl - 0 (list 1 2 3 4))
;<DISCUSS>







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's apply foldr to implementing draw-wof:

; draw-wof : WoF -> Image
; Draws the current game state
 
(define IMAGE-TEXT-SIZE 35)
(define IMAGE-COLOR "red")
 
(check-expect (draw-wof WOF-0) (text "?????" IMAGE-TEXT-SIZE IMAGE-COLOR))
(check-expect (draw-wof WOF-1) (text "?e???" IMAGE-TEXT-SIZE IMAGE-COLOR))
 
(define (draw-wof wof)
  (text (get-rendered-string wof) IMAGE-TEXT-SIZE IMAGE-COLOR))
 
; get-rendered-string : WoF -> String
; Returns the WoF as a string to display
 
(check-expect (get-rendered-string WOF-0) "?????")
(check-expect (get-rendered-string WOF-1) "?e???")
 
(define (get-rendered-string wof)
  #;<DO NOW><SCROLL>
  (foldr <func???> <base???> <list???>)
  ;<BRAINSTORM> Maybe use another list abstr. in tandem?
  ; Helper: get-rendered-g1s







  (foldr string-append "" (map get-rendered-g1s wof)))
 
; get-rendered-g1s : Game1String -> String
; Returns the character that should be rendered
 
(check-expect (get-rendered-g1s G1S-1) "a")
(check-expect (get-rendered-g1s G1S-2) "?")
 
(define (get-rendered-g1s g1s)
  (cond [(guessed? g1s) (guessed-char g1s)]
        [(unguessed? g1s) "?"]))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Last function on wishlist: design handle-key.

; handle-key : WoF KeyEvent -> WoF
; Clear out all instances of WoF
 
(check-expect (handle-key WOF-0 "a") WOF-0)
(check-expect (handle-key WOF-0 "e") WOF-1)
(check-expect (handle-key WOF-1 "l") WOF-2)
(check-expect (handle-key WOF-2 "h") WOF-3)
(check-expect (handle-key WOF-3 "o") WOF-4)

; Let's see if we can apply one of our know list abstractions.
; Which seems to be the best fit?
;<BRAINSTORM>








#;
(define (handle-key wof ke)
  (map guess-ke wof))

; guess-ke : Game1String -> Game1String
; Converts any make-unguessed Game1String to the corresponding make-guessed
; if that character matches the guess
;<BRAINSTORM>








; Hmm... Can *almost* do it... but we fall short because we need the KeyEvent
; for the mapped function to be able to do its work!
; For now, do things the old-fashioned way
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (handle-key wof ke)
  (cond [(empty? wof) wof]
        [(cons? wof) (cons (guess-ke (first wof) ke)
                           (handle-key (rest wof) ke))]))
 
; guess-ke : Game1String KeyEvent -> Game1String
; Returns the new Game1String given the guess
 
(check-expect (guess-ke (make-guessed "e") "f") (make-guessed "e"))
(check-expect (guess-ke (make-guessed "e") "e") (make-guessed "e"))
(check-expect (guess-ke (make-unguessed "e") "f") (make-unguessed "e"))
(check-expect (guess-ke (make-unguessed "e") "e") (make-guessed "e"))
 
(define (guess-ke g1s ke)
  (cond [(guessed? g1s) g1s]
        [(unguessed? g1s) (if (string=? ke (unguessed-char g1s))
                              (make-guessed (unguessed-char g1s))
                              g1s)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; It's unsatisfying that we couldn't use a list abstraction for handle-key --
; we were so close! We'll try again later, when we have more tools at our disposal.

