;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10-world-prog) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following will be PROVIDED:
(require 2htdp/image)
(require 2htdp/universe)

; The world state type:
(define-struct drive-state [hbs on? msg])
; A DriveState is a (make-drive-state HBS Boolean String)
; Representing the current bitmap, and an indication that big-bang is active

; drive-sim : [List-of Boolean] -> [List-of Boolean]
; A world program that consumes an initial flat map of the free and used layout of
; a drive, presents a UI that allows the user to modify the values, and produces
; the resulting storage map when the user exits.
(define (drive-sim lob)
  (flatten-hbs (drive-state-hbs (big-bang (make-drive-state (initialize-hbs lob) #true "[INIT]")
                                  [to-draw draw-ui]
                                  [on-mouse handle-mouse]
                                  [on-key handle-key]
                                  [stop-when bb-over?]))))

; flatten-hbs : HBS -> [List-of Boolean]
; "Unconverts" an HBS into a flat list of free blocks
(check-expect (flatten-hbs HBS-1) BITMAP-1)
(check-expect (flatten-hbs HBS-2) BITMAP-2)
(define (flatten-hbs hbs)
  (local [; flatten-hbs/a : hbs is-set
          ; flattens a bitmap tree
          ; ACCUMULATOR: whether an ancestor node was true (i.e., whole subtree is true)
          (define (flatten-hbs/a hbs is-set)
            (cond [(boolean? hbs) '()]
                  [(hbs? hbs) (local [(define tree-is-set (or is-set (hbs-bit hbs)))]
                                (if (boolean? (hbs-left hbs))
                                    (list tree-is-set)
                                    (append (flatten-hbs/a (hbs-left hbs) tree-is-set)
                                            (flatten-hbs/a (hbs-right hbs) tree-is-set))))]))]
    (flatten-hbs/a hbs #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BOX-HEIGHT 20)
(define TEXT-SIZE 16)
(define DISP-TOP (image-height (text "TEXT" TEXT-SIZE "black")))

; draw-ui : DriveState -> Image
; STUB: add check-expects
(define (draw-ui bb)
  (above/align "left" (text (list-stats (drive-state-hbs bb)) TEXT-SIZE "black")
               (draw-map (drive-state-hbs bb))
               (text (drive-state-msg bb) TEXT-SIZE "black")))

; list-stats : [List-of Boolean] -> String
; Produces a summary of the bitmap state
; STUB: add check-expects
(define (list-stats hbs)
  (string-append (number->string (blocks-remaining hbs)) " of "
                 (number->string (expt 2 (hbs-height hbs)))))

; draw-map : HBS -> Image
; Renders an image of the HBS as a stacked, aligned set of bitmaps, one for each chunk size
; STUB: add check-expects
(define (draw-map hbs)
  (local [(define levels (add1 (hbs-height hbs)))
          (define (add-map-above lob chunksize img)
            (above/align "left" (draw-1map lob chunksize) img))]
    (foldr add-map-above
           empty-image
           (hbs->lolob hbs)
           (build-list levels (λ (i) (expt 2 (- levels i 1)))))))

(define OLDHBS-1 (list (list #f)
                       (list #f #t)
                       (list #t #f #f #f)
                       (list #f #f #t #f #f #f #f #f)
                       (list #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f)))
(define OLDHBS-2 (list (list #f)
                       (list #f #f)
                       (list #t #f #f #f)
                       (list #f #f #f #f #f #f #f #f)
                       (list #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f)))

(check-expect (hbs->lolob HBS-1) OLDHBS-1)
(check-expect (hbs->lolob HBS-2) OLDHBS-2)
(define (hbs->lolob hbs)
  (cond [(boolean? hbs) '()]
        [(hbs? hbs) (cons (list (hbs-bit hbs))
                          (map append (hbs->lolob (hbs-left hbs)) (hbs->lolob (hbs-right hbs))))]))
    

; draw-1map : [List-of Boolean] PosInt -> Image
; displays the list as a series of contiguous rectangles of the requested width:height ratio.
;; render the #true values as black filled boxes, and #false values as black-outlined white boxes.
; STUB: add check-expects
(define (draw-1map lob n)
  (cond [(empty? lob) (overlay (text (number->string n) TEXT-SIZE "white")
                               (rectangle (* 4 BOX-HEIGHT) BOX-HEIGHT "solid" "black")
                               )]
        [(cons? lob) (beside (if (first lob)
                                 (overlay (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "outline" "black")
                                          (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "solid" "black"))
                                 (overlay (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "outline" "black")
                                          (rectangle (* n BOX-HEIGHT) BOX-HEIGHT "solid" "white")))
                             (draw-1map (rest lob) n))]))

; hbs-height : HBS -> Int
; Determines the height of the HBS, which is a perfect binary tree, so just measure the
; leftmost leaf's depth
(check-expect (hbs-height HBS-1) 4)
(check-expect (hbs-height (hbs-left HBS-1)) 3)
(check-expect (hbs-height (make-hbs #t #f #f)) 0)

(define (hbs-height hbs)
  (cond [(boolean? hbs) -1]
        [(hbs? hbs) (add1 (hbs-height (hbs-left hbs)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; handle-mouse : DriveState -> DriveState
; allows a user to click on one of the rectangles in the bitmap to free up the chunk,
; or click on the allocation button at the end of the row to allocate a chunk of that size
; STUB: add check-expects
(define (handle-mouse bb x y b)
  (if (and (drive-state-on? bb)
           (string=? b "button-up")
           (> y DISP-TOP))
      (alloc-or-free (drive-state-hbs bb)
                     (quotient (- y DISP-TOP) BOX-HEIGHT)
                     (quotient x BOX-HEIGHT))
      bb))

(define (alloc-or-free hbs row col)
  (local [(define height (hbs-height hbs))
          (define chunksize (expt 2 (- height row)))]
    (cond [(> row height) (make-drive-state hbs #true "(click on map)")]
          [(< col (expt 2 height))
           (local [(define blk (- col (remainder col chunksize)))]
           (make-drive-state (free-chunk hbs chunksize blk)
                             #true
                             (string-append "Freed " (number->string chunksize)
                                            "-chunk at "
                                            (number->string blk))))]
          [else (local [(define result (alloc-chunk hbs chunksize))]
                  (make-drive-state (hbs-alloc-hbs result)
                                    #true
                                    (string-append "Alloc: "
                                                   (number->string (hbs-alloc-block result)))))])))

; handle-key : DriveState -> DriveState
; exits big-bang when the user hits the "Q" key
; STUB: add check-expects
(define (handle-key bb ke)
  (if (or (string=? ke "Q") (string=? ke "q"))
      (make-drive-state (drive-state-hbs bb) #false "[DONE]")
      bb))

; bb-over? : DriveState -> Boolean
; Checks whether the simulation has quit
; STUB: add check-expects
(define (bb-over? bb)
  (not (drive-state-on? bb)))
