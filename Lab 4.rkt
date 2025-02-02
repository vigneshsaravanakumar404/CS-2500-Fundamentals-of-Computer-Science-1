;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; =============================================
(define-struct ws [text cursor-pos])

; A ws is a (world-state text cursor-pos)
; Interpretation: The current string and the cursor position
;  - text is a String 
;  - cursor-pos is a Number

; make-ws : String Number -> ws
; ws? : Any -> Boolean
; ws-text : ws -> String
; ws-cursor-pos : ws -> Number

(define WS-1 (make-ws "Hello World" 11))
(define WS-2 (make-ws "Hello Worl" 10))
(define WS-3 (make-ws "Hello Wor" 9))

(define (ws-temp t)
  (... (text-temp (ws-text t)) ...
       (cursor-pos-temp (ws-cursor-pos t)) ...))
;; =============================================


;; =============================================
; word-processor : String -> Boolean
; Runs a simplified word processor, returns #true if all went okay

(define WS-4 (make-ws "Hello Wor" 9))

(define (word-processor initial-state)
  (big-bang initial-state
    [to-draw draw-wp]
    [on-key key-wp]))
;; =============================================



;; =============================================
;; to-draw event handler:

(define BACKGROUND (empty-scene 600 100 "yellow"))
(define WORD-SIZE 16)
(define WORD-COLOR "black")
(define HIGHLIGHT-COLOR "red")

;; current-space? : WorldState -> Boolean
; returns true if the current cursor position is a space

(check-expect (current-space? (make-ws " 2" 0)) #true)
(check-expect (current-space? (make-ws "" 0)) #false)
(check-expect (current-space? (make-ws " 2" 1)) #false)
(check-expect (current-space? (make-ws "  " 2)) #false)
(check-expect (current-space? (make-ws "a2" 1)) #false)

(define (current-space? ws)
  (cond
    [(zero? (string-length (ws-text ws))) #false]
    [(= (ws-cursor-pos ws) (string-length (ws-text ws))) #false]
    [(string=? " " (substring (ws-text ws) (within-range ws) (add1 (ws-cursor-pos ws)))) #true]
    [else #false]))

; draw-wp : String -> Image
; Draws the current text buffer
(check-expect (draw-wp (make-ws "Hello World" 5))
              (overlay/align
               "left"
               "middle"
               (text
                "Hello_World"
                WORD-SIZE WORD-COLOR) BACKGROUND))
(check-expect (draw-wp (make-ws "Hello World" 11))
              (overlay/align
               "left"
               "middle"
               (text
                "Hello World#"
                WORD-SIZE WORD-COLOR) BACKGROUND))
(check-expect (draw-wp (make-ws "Hello World" 4))
              (overlay/align
               "left"
               "middle"
               (beside
                (text "Hell"
                      WORD-SIZE WORD-COLOR)
                (text "o"
                      WORD-SIZE HIGHLIGHT-COLOR)
                (text " World"
                      WORD-SIZE WORD-COLOR))
               BACKGROUND))

(define (draw-wp ws)
  (cond
    [(current-space? ws)
     (overlay/align
      "left"
      "middle"
      (text
       (string-append
        (substring (ws-text ws) 0 (ws-cursor-pos ws))
        "_"
        (if (= (ws-cursor-pos ws) (string-length (ws-text ws)))
            ""
            (substring (ws-text ws) (add1 (ws-cursor-pos ws)))))
       WORD-SIZE WORD-COLOR) BACKGROUND)]
    [(= (ws-cursor-pos ws) (string-length (ws-text ws)))
     (overlay/align
      "left"
      "middle"
      (text
       (string-append
        (ws-text ws)
        "#")
        WORD-SIZE WORD-COLOR) BACKGROUND)]
    [else
     (overlay/align
      "left"
      "middle"
      (beside
       (text (substring (ws-text ws) 0 (ws-cursor-pos ws))
             WORD-SIZE WORD-COLOR)
       (text (substring (ws-text ws) (ws-cursor-pos ws) (add1 (ws-cursor-pos ws)))
             WORD-SIZE HIGHLIGHT-COLOR)
       (text (substring (ws-text ws) (add1 (ws-cursor-pos ws)))
             WORD-SIZE WORD-COLOR))
      BACKGROUND)]))
;; =============================================



;; =============================================
;; on-key event handler:




; key-wp : String KeyEvent -> String
; Adds key to the end of s, or removes the last letter if backspace is entered
(check-expect (key-wp (make-ws "A" 1) "a") (make-ws "Aa" 2))
(check-expect (key-wp (make-ws "Ab" 1) "a") (make-ws "Aab" 2))
(check-expect (key-wp (make-ws "A" 1) "\b") (make-ws "" 0))


(define (key-wp ws key)
  (cond [(string=? key "\b") (remove-current ws)]
        [(string=? key "left") (make-ws (ws-text ws)
                                        (within-range (make-ws (ws-text ws) (- (ws-cursor-pos ws) 1))))]
        [(string=? key "right") (make-ws (ws-text ws)
                                         (within-range (make-ws (ws-text ws) (+ 1 (ws-cursor-pos ws)))))]
        [else (make-ws (insert-letter ws key) (+ 1 (ws-cursor-pos ws)))]))


;; Helper function for key-wp:

;; within-range : Number ws -> Number
;; Ensures the cursor position is within the string
(define (within-range ws)
  (cond
    [(> (ws-cursor-pos ws) (+ 0 (string-length (ws-text ws)))) (+ 0 (string-length (ws-text ws)))]
    [(< (ws-cursor-pos ws) 0) 0]
    [else (ws-cursor-pos ws)]))

;; insert-letter : ws String -> String
;; Inserts the letter at the cursor position
(define (insert-letter ws key)
  (string-append (substring (ws-text ws) 0 (within-range ws))
                 key
                 (cond
                   [(<= (ws-cursor-pos ws) (string-length(ws-text ws)))
                        (substring (ws-text ws) (ws-cursor-pos ws) (string-length (ws-text ws)))]
                   [else ""])))

; remove-current : WorldState -> WorldState
; Removes the letter at the current cursor position in the supplied string

(check-expect (remove-current (make-ws "pasta" 2)) (make-ws "psta" 1))
(check-expect (remove-current (make-ws "pasta" 0)) (make-ws "pasta" 0))
(check-expect (remove-current (make-ws "pasta" 5)) (make-ws "past" 4))
(check-expect (remove-current (make-ws "" 0)) (make-ws "" 0))

(define (remove-current ws)
  (cond [(or (zero? (ws-cursor-pos ws)) (string=? (ws-text ws) "")) ws]
        [else (make-ws
               (string-append
                (substring (ws-text ws) 0 (sub1 (ws-cursor-pos ws)))
                (substring (ws-text ws) (ws-cursor-pos ws)))
               (sub1 (ws-cursor-pos ws)))]))

(word-processor WS-4)
;; =============================================


;; Fix cursor on the right