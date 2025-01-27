;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname L07_P2-wp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise:
; Design a world program word-processor that renders the characters that the user
; types in black text on a yellow background.

(require 2htdp/image)
(require 2htdp/universe)

; word-processor : String -> String
; Runs a simplified word processor
 
(define (word-processor initial-word)
  (big-bang initial-word
    ; What event handlers will we need? Also write out their signatures
    ;<DO NOW><SCROLL>







    



    [to-draw draw-word]  ; String -> Image
    [on-key key-wp/bs]))    ; String KeyEvent -> String

; draw-word : String -> Image
; Draws the current word

(define BACKGROUND (empty-scene 600 100 "yellow"))
(define WORD-SIZE 20)
(define WORD-COLOR "black")
 
#;
(check-expect (draw-word "hello")
              ;??? <DO NOW><SCROLL>




              
              )

(check-expect (draw-word "hello")
              (overlay/align "left" "middle" (text "hello" WORD-SIZE WORD-COLOR) BACKGROUND))
 
(check-expect (draw-word "a")
              (overlay/align "left" "middle" (text "a" WORD-SIZE WORD-COLOR) BACKGROUND))
 
(define (draw-word s)
  (overlay/align "left" "middle" (text s WORD-SIZE WORD-COLOR) BACKGROUND))
 
; key-wp : String KeyEvent -> String
; Adds key to the end of s

; Add check-expects
#;(check-expect (key-wp "" "e") ???)
#;(check-expect (key-wp "e" "l") ???)
 
(define (key-wp ws key)
  ;<DO NOW><SCROLL>







  
  (string-append ws key))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise: how would we add backspace handling?

; key-wp/bs : String KeyEvent -> String
; Adds key to the end of s, or removes the last letter if backspace is entered
 
(check-expect (key-wp/bs "e" "") "e")
(check-expect (key-wp/bs "l" "e") "le")
(check-expect (key-wp/bs "le" "\b") "l")
 
(define (key-wp/bs s key)
  ;(string-append ws key))  ; ORIG
  ;<DO NOW><SCROLL>








  (cond
    [(string=? key "\b") (substring s 0 (sub1 (string-length s)))]
    [else (string-append s key)]))








; <UNCOMMENT>
#;(check-expect (key-wp/bs "" "\b") "")
  
#;
(define (key-wp/bs s key)
  (cond
    [(string=? key "\b") (cond
                           [(string=? s "") s]
                           [else (substring s 0 (sub1 (string-length s)))])]
    [else (string-append s key)]))

; Too complicated a function--let' simplify by modularizing...
;<DO NOW><SCROLL>







 
#;
(define (key-wp/bs s key)
  (cond
    [(string=? key "\b") (remove-last s)]
    [else (string-append s key)]))
 
; remove-last : String -> String
; Removes the last letter of the supplied string
 
(check-expect (remove-last "") "")
(check-expect (remove-last "apple") "appl")
 
(define (remove-last s)
  (cond
    [(string=? s "") s]
    [else (substring s 0 (sub1 (string-length s)))]))
