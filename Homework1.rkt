#lang racket

(require 2htdp/image)


;; Exercise 8
(define (string-insert string position)
  (string-append (substring string 0 position) "-" (substring string position (string-length string)))
  )


;; Exercise 9
;; Define colors
(define light-blue (make-color 173 216 230))
(define dark-brown (make-color 139 69 19))
(define light-brown (make-color 183 111 87))
(define dark-green (make-color 34 139 34))
(define gold (make-color 255 215 0))
(define orange (make-color 255 165 0))
(define light-purple (make-color 160 32 240))

;; Define constants
(define bg_width 200) ;; RESIZE THE IMAGE HERE
(define circle_center_x (/ bg_width 6.66666667))
(define circle_center_y (/ bg_width 6.66666667))
(define circle_radius (/ bg_width 13.3333333))
(define ellipse_height (/ bg_width 3.38983051))
(define ellipse_width (/ bg_width 5.71428571))
(define ellipse_center_x (/ bg_width 4.04040404))
(define ellipse_center_y (/ bg_width 1.95121951))
(define ground_center_x (/ bg_width 2))
(define ground_center_y (/ bg_width 1.11111111))
(define ground_height (/ bg_width 5))
(define stump_width (/ bg_width 15.3846154))
(define stump_height (/ bg_width 6.25))
(define stump_center_y (/ bg_width 1.38888889))
(define stump_center_x (/ bg_width 4.04040404))
(define house_base_width (/ bg_width 4.25531915))
(define house_base_height (/ bg_width 4.16666667))
(define house_base_x (/ bg_width 1.33779264))
(define house_base_y (/ bg_width 1.46148148))
(define roof_l1 (/ bg_width 3.92156863))
(define roof_l2 (/ bg_width 3.38983051))
(define roof_x (/ bg_width 1.33779264))
(define roof_y (/ bg_width 2.13045685))
(define outer_door_x (/ bg_width 1.3400335))
(define outer_door_y (/ bg_width 1.34680135))
(define outer_door_width (/ bg_width 22.2222222))
(define outer_door_height (/ bg_width 8.69565217))
(define window_inner_width (/ bg_width 28.5714286))
(define window_outer_width (+ window_inner_width (/ bg_width 200)))
(define window_inner_y (/ bg_width 1.58730159))
(define window_inner_x_1 (/ bg_width 1.42857143))
(define window_inner_x_2 (/ bg_width 1.2539185))


;; Define shapes
(define c (circle circle_radius "solid" "yellow"))
(define bg (rectangle bg_width bg_width "solid" light-blue))
(define leaves (ellipse ellipse_width ellipse_height "solid" dark-green))
(define ground (rectangle bg_width ground_height "solid" dark-brown))
(define stump (rectangle stump_width stump_height "solid" light-brown))
(define house_base (rectangle house_base_width house_base_height "solid" gold))
(define roof (rotate 180 (triangle/sss roof_l1 roof_l1 roof_l2 "solid" "red")))
(define outer_door (rectangle outer_door_width outer_door_height "solid" light-purple))
(define window_1 (rectangle window_inner_width window_inner_width "solid" "white"))
(define window_2 (rectangle window_outer_width window_outer_width "solid" orange))

;; Place Objects
(define final-scene
  (place-image leaves ellipse_center_x ellipse_center_y
   (place-image ground ground_center_x ground_center_y
    (place-image stump stump_center_x stump_center_y
     (place-image c circle_center_x circle_center_y
      (place-image roof roof_x roof_y
       (place-image outer_door outer_door_x outer_door_y
        (place-image window_1 window_inner_x_1 window_inner_y
         (place-image window_1 window_inner_x_2 window_inner_y
          (place-image window_2 window_inner_x_1 window_inner_y
           (place-image window_2 window_inner_x_2 window_inner_y
            (place-image house_base house_base_x house_base_y
         bg))))))))))))

;; Call Functions
(string-insert "HelloWorld" 5)
final-scene
