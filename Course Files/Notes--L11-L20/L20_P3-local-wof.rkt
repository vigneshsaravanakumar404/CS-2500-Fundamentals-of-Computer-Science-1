;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname P3-local-wof) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Can local help us write a better handle-key ?

; Original versions of handle-key and its helper guess-ke :

; handle-key : WoF KeyEvent -> WoF
; Clear out all instances of WoF
 
(check-expect (handle-key WOF-0 "a") WOF-0)
(check-expect (handle-key WOF-0 "e") WOF-1)
(check-expect (handle-key WOF-1 "l") WOF-2)
(check-expect (handle-key WOF-2 "h") WOF-3)
(check-expect (handle-key WOF-3 "o") WOF-4)

(define (handle-key/v1 wof ke)
  (cond
    [(empty? wof) wof]
    [(cons? wof)
     (cons (guess-ke (first wof) ke)
           (handle-key (rest wof) ke))]))
 
; guess-ke : Game1String KeyEvent -> Game1String
; Returns the new Game1String given the guess
 
(check-expect (guess-ke (make-guessed "e") "f") (make-guessed "e"))
(check-expect (guess-ke (make-guessed "e") "e") (make-guessed "e"))
(check-expect (guess-ke (make-unguessed "e") "f") (make-unguessed "e"))
(check-expect (guess-ke (make-unguessed "e") "e") (make-guessed "e"))
 
(define (guess-ke g1s ke)
  (cond
    [(guessed? g1s) g1s]
    [(unguessed? g1s)
     (if (string=? ke (unguessed-char g1s))
         (make-guessed (unguessed-char g1s))
         g1s)]))
;<DISCUSS>

(define (handle-key wof ke)
  ;<DO NOW><SCROLL>







  
  (local [; guess-ke : Game1String -> Game1String
          ; Reveals g1s if it was hidden and matches the key
          (define (guess-ke g1s)
            (cond
              [(guessed? g1s) g1s]
              [(unguessed? g1s)
               (if (string=? ke (unguessed-char g1s))
                   (make-guessed (unguessed-char g1s))
                   g1s)]))]
    (map guess-ke wof)))

; Try running it!
#;
(do-it! 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 2htdp/image)
(require 2htdp/universe)
 
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
   (cond
     [(guessed? g1s) (... (guessed-char g1s) ...)]
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
 
 
; guessing-game : String -> WoF
; Run the world of fortune game on s
 
(define (guessing-game s)
  (big-bang (map make-unguessed (explode s))
    [to-draw draw-wof]
    [stop-when all-guessed? draw-wof]
    [on-key handle-key]))
 
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
  (foldr string-append "" (map get-rendered-g1s wof)))
 
; get-rendered-g1s : Game1String -> String
; Returns the character that should be rendered
 
(check-expect (get-rendered-g1s G1S-1) "a")
(check-expect (get-rendered-g1s G1S-2) "?")
 
(define (get-rendered-g1s g1s)
  (cond [(guessed? g1s) (guessed-char g1s)]
        [(unguessed? g1s) "?"]))
 
 
; all-guessed? : WoF -> Boolean
; Have all the characters been guessed?
 
(check-expect (all-guessed? WOF-0) #false)
(check-expect (all-guessed? WOF-1) #false)
(check-expect (all-guessed? WOF-4) #true)
 
(define (all-guessed? wof)
  (andmap guessed? wof))

(define (all-guessed?/v1 wof)
  (...
   (cond
     [(empty? wof) #true]
     [(cons? wof)
      (and (guessed? (first wof))
           (all-guessed? (rest wof)))])))
 

; handle-key design was move up to top for easier perusal

(define (do-it! x)
  (guessing-game "supercalifragilisticexpialidocious"))

