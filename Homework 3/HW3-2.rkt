;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW3-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; =========================================================================
;; a.
; An Hour is a natural number 0 to 23
; Interpretation: An hour of the day
(define HOUR-1 12)
(define HOUR-2 0)
(define HOUR-3 23)

(define (hour-temp h)
  (... h ...))


; A Minute is a natural number 0 to 59
; Interpretation: A minute of the hour
(define MINUTE-1 30)
(define MINUTE-2 0)
(define MINUTE-3 59)

(define (minute-temp m)
  (... m ...))
;; =========================================================================


;; =========================================================================
;; b/c.
(define-struct time [hour minute])

; A Time is a (make-time Hour Minute)
; Interpretation: A time of day
;  - Hour is the hour of the day between 0 and 23
;  - Minute is the minute of the hour between 0 and 59

; make-time : Hour Minute -> Time
; time? : Any -> Boolean
; time-hour : Time -> Hour
; time-minute : Time -> Minute

(define TIME-1 (make-time HOUR-1 MINUTE-1))
(define TIME-2 (make-time HOUR-2 MINUTE-2))
(define TIME-3 (make-time HOUR-3 MINUTE-3))

(define (time-temp t)
  (... (hour-temp (time-hour t)) ...
       (minute-temp (time-minute t)) ...))
;; =========================================================================


;; =========================================================================
;; d.

;; tick-tock : Time -> Time
;; Given a Time, returns the next minute. 59th minute returns 0th minute and increments hour.
;; 23rd hour returns 0th hour.
(check-expect (tick-tock (make-time HOUR-1 MINUTE-1))
              (make-time 12 31))
(check-expect (tick-tock (make-time 23 59))
              (make-time 00 00))
(check-expect (tick-tock (make-time 23 00))
              (make-time 23 01))
(check-expect (tick-tock (make-time 00 00))
              (make-time 00 01))
(check-expect (tick-tock (make-time 00 59))
              (make-time 01 00))


(define (tick-tock t)
  (make-time (modulo (+ (quotient (+ 1 (time-minute t)) 60) (time-hour t)) 24)
             (modulo (+ 1 (time-minute t)) 60)))
;; =========================================================================


;; =========================================================================
;; e.
;; pad-zeros : String -> String
;; pads string with 0 so always has 2 digits
(define (pad-zeros s)
  (cond
    [(< (string-length s) 2) (string-append "0" s)]
    [else s]))

;; hour-formatting : Number -> Number
;; given an hour returns the formatted hour
(define (hour-formatting h)
  (cond
    [(= (modulo h 12) 0) 12]
    [else (modulo h 12)]))

;; time->text : Time -> String
;; Given a Time, returns a string representation of the time in the format "HH:MM AM/PM"
(check-expect (time->text (make-time 00 28)) "12:28 AM")
(check-expect (time->text (make-time 12 28)) "12:28 PM")
(check-expect (time->text (make-time 23 59)) "11:59 PM")
(check-expect (time->text (make-time 11 59)) "11:59 AM")

(define (time->text t)
  (string-append (pad-zeros (number->string (hour-formatting (time-hour t))))
                 ":"
                 (pad-zeros (number->string (time-minute t)))
                 (cond
                   [(< (time-hour t) 12) " AM"]
                   [else " PM"])))
;; =========================================================================


;; =========================================================================
;; f.
;; render : Time -> Image
;; Given a Time, returns an image that displays the time in the format "HH:MM AM/PM"
(define (render t)
  (text (time->text t) 50 "black"))

;; digital-clock : Time -> Time
;; Given a Time, returns a digital clock that ticks every second
(define (digital-clock t)
  (big-bang t
    (on-tick tick-tock 60)
    (to-draw render)))

(digital-clock (make-time 10 51))
;; =========================================================================