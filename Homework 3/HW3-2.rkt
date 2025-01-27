(require 2htdp/universe)
(require 2htdp/image)

; =========================================================================
;; a.
(define-struct hour [hour])

; An Hour is a (make-hour Number)
; Interpretation: An hour of the day
;  - hour is the hour of the day between 0 and 23

; make-hour : Number -> Hour
; hour? : Any -> Boolean
; hour-hour : hour -> Number

(define HOUR-1 (make-hour 12))
(define HOUR-2 (make-hour 0))
(define HOUR-3 (make-hour 23))

(define (hour-temp h)
  (... (hour-hour h) ...))


(define-struct minute [minute])

; A Minute is a (make-minute Number)
; Interpretation: A minute of the hour
;  - minute is the minute of the hour between 0 and 59

; make-minute : Number -> Minute
; minute? : Any -> Boolean
; minute-minute : Minute -> Number

(define MINUTE-1 (make-minute 30))
(define MINUTE-2 (make-minute 0))
(define MINUTE-3 (make-minute 59))

(define (minute-temp m)
  (... (minute-minute m) ...))
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
(check-expect (tick-tock (make-time (make-hour 12) (make-minute 30))) (make-time (make-hour 12) (make-minute 31)))
(check-expect (tick-tock (make-time (make-hour 23) (make-minute 59))) (make-time (make-hour 00) (make-minute 00)))
(check-expect (tick-tock (make-time (make-hour 23) (make-minute 00))) (make-time (make-hour 23) (make-minute 01)))
(check-expect (tick-tock (make-time (make-hour 00) (make-minute 00))) (make-time (make-hour 00) (make-minute 01)))
(check-expect (tick-tock (make-time (make-hour 00) (make-minute 59))) (make-time (make-hour 01) (make-minute 00)))


(define (tick-tock t)
  (make-time (make-hour (modulo (+ (quotient (+ 1 (minute-minute (time-minute t))) 60) (hour-hour (time-hour t))) 24))
             (make-minute (modulo (+ 1 (minute-minute (time-minute t))) 60))))
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
(check-expect (time->text (make-time (make-hour 00) (make-minute 28))) "12:28 AM")
(check-expect (time->text (make-time (make-hour 12) (make-minute 28))) "12:28 PM")
(check-expect (time->text (make-time (make-hour 23) (make-minute 59))) "11:59 PM")
(check-expect (time->text (make-time (make-hour 11) (make-minute 59))) "11:59 AM")

(define (time->text t)
  (string-append (pad-zeros (number->string (hour-formatting (hour-hour (time-hour t)))))
                 ":"
                 (pad-zeros (number->string (minute-minute (time-minute t))))
                 (cond
                   [(< (hour-hour (time-hour t)) 12) " AM"]
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

(digital-clock (make-time (make-hour 23) (make-minute 59)))
;; =========================================================================