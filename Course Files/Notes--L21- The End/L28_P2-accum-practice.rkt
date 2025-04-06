;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname L28_P2-accum-practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; PRACTICE WITH ACCUMULATORS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ACCUMULATING TREES
; Design a function similar to the one that took a list of relative distances
; between cities and computed the list of absolute distances, except now working
; on a tree of cities

; The data definitions:

(define-struct btnode [data left right])
 
; A BinaryTree (BT) is one of:
; - #false
; - (make-btnode Number BT BT)
; Interpretation: A binary tree of numbers
 
(define BT-0 #false)
(define BT-1 (make-btnode 5 BT-0 BT-0))
(define BT-2L (make-btnode 7 BT-1 BT-0))
(define BT-2R (make-btnode 6 BT-0 BT-1))
(define BT-3 (make-btnode 1 BT-2L BT-2R))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; First three steps of the design recipe:

; sum-bt : BT -> BT
; Creates a BT of the sum of all
; data on the way to that node
 
(check-expect (sum-bt BT-0) BT-0)

(check-expect (sum-bt BT-3)
              (make-btnode 1
                           (make-btnode 8
                                        (make-btnode 13 BT-0 BT-0)
                                        BT-0)
                           (make-btnode 7
                                        BT-0
                                        (make-btnode 12 BT-0 BT-0))))
 
; Exercise:
; Design the code

(define (sum-bt bt)
  (local [; sum-bt/a : BT Nat -> BT
          ; Creates a BT of the sum of all
          ; data on the way to the supplied node
          ; Accumulator: ???
          (define (sum-bt/a btree acc)
            #;
            (cond
              [(boolean? btree) ...]
              [(btnode? btree) (... (btnode-data btree) ...
                                    (sum-bt/a (btnode-left btree) <ACC>)
                                    (sum-bt/a (btnode-right btree) <ACC>)))])







            
            #;
            (cond
              [(boolean? btree) ...]
              [(btnode? btree) (make-btnode (+ (btnode-data btree) acc)
                                            (sum-bt/a (btnode-left btree) (+ (btnode-data btree) acc))
                                            (sum-bt/a (btnode-right btree) (+ (btnode-data btree) acc))))])
            ; Can we optimize further?
            ;<DO NOW>







            
            (cond
              [(boolean? btree) btree]
              [(btnode? btree) (local [(define SUM (+ (btnode-data btree) acc))]
                                 (make-btnode SUM
                                              (sum-bt/a (btnode-left btree) SUM)
                                              (sum-bt/a (btnode-right btree) SUM)))])
            )]
    (sum-bt/a bt 0)))

