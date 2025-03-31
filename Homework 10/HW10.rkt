;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW9-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; ======================================= Provided Data Designs ======================================
(define-struct hbs [bit left right])
; A HierarchicalBitmapSet (HBS) is one of:
; - #false
; - (make-hbs Boolean HBS HBS)
; representing a bit at some level of a hierarchical tree of bits,
; or a sentinel value representing an empty tree/subtree
(define (hbs-temp hbs)
  (...
   (cond [(boolean? hbs) ...]
         [(hbs? hbs) (... (hbs-bit hbs) ...
                          (hbs-temp (hbs-left hbs)) ...
                          (hbs-temp (hbs-right hbs)) ...)])))


(define-struct hbs-alloc [block hbs])
; A HBSAllocResult is a (make-hbs-alloc Integer HBS)
; Represents the result of an allocation call, where:
; - block is the starting block number for the chunk that was allocated;
;   or -1 if no space available
; - hbs is the updated HBS after the allocation
(define HBSAR-1 (make-hbs-alloc 1
                                (make-hbs #f
                                          (make-hbs #f
                                                    (make-hbs #t #f #f)
                                                    (make-hbs #f #f #f))
                                          (make-hbs #t
                                                    (make-hbs #f #f #f)
                                                    (make-hbs #f #f #f)))))
; ===================================== End Provided Functions =======================================

(define HBS-1 (make-hbs #t #f #f))
;    1
;   / \
;  0   0

(define HBS-2 (make-hbs #t (make-hbs #f #f #f) (make-hbs #f #f #f)))
;        1
;      /   \
;    0       0
;   / \     / \
;  0   0   0   0

(define HBS-3 (make-hbs #t (make-hbs #f (make-hbs #f #f #f) (make-hbs #f #f #f)) 
                           (make-hbs #f (make-hbs #f #f #f) (make-hbs #f #f #f))))
;                1
;              /   \
;            /       \
;          /           \
;        0               0
;      /   \           /   \
;    0       0       0       0
;   / \     / \     / \     / \
;  0   0   0   0   0   0   0   0

(define HBS-4 (make-hbs #f (make-hbs #f HBS-2 (make-hbs #f (make-hbs #t #f #f) (make-hbs #f #f #t))) 
                           HBS-3))
;                                0
;                            /       \
;                         /             \                   
;                      /                   \
;                   /                         \            
;                0                               1
;              /   \                           /   \
;            /       \                       /       \
;          /           \                   /           \
;        1               0               0               0
;      /   \           /   \           /   \           /   \
;    0       0       1       0       0       0       0       0
;   / \     / \     / \     / \     / \     / \     / \     / \
;  0   0   0   0   0   0   0   1   0   0   0   0   0   0   0   0


; Exercise 1a
; blocks-remaining : HBS -> NonNegInteger
; to produce the number of blocks remaining in the HBS
(check-expect (blocks-remaining HBS-1) 2)
(check-expect (blocks-remaining HBS-2) 4)
(check-expect (blocks-remaining HBS-3) 8)
(check-expect (blocks-remaining HBS-4) 15)

(define (blocks-remaining HBS)
    (local [
        (define (blocks-remaining-helper HBS weight)
            (cond
                [(and (hbs? HBS) (hbs-bit HBS)) weight]
                [(hbs? HBS) (+ (blocks-remaining-helper (hbs-left HBS) (/ weight 2))
                               (blocks-remaining-helper (hbs-right HBS) (/ weight 2)))]
                [(and #t HBS) weight]
                [else 0]))]
    (blocks-remaining-helper HBS (expt 2 (depth HBS)))))

; depth : HBS -> NonNegInteger
; to produce the depth of the HBS
(check-expect (depth HBS-1) 1)
(check-expect (depth HBS-2) 2)
(check-expect (depth HBS-3) 3)
(check-expect (depth HBS-4) 4)

(define (depth HBS)
    (local [(define (depth-helper HBS depth)
                (cond
                    [(hbs? HBS) (depth-helper (hbs-left HBS) (+ depth 1))]
                    [else depth]))]
        (depth-helper HBS 0)))

; Exercise 1b
; find-chunk : HBS NonNegInteger -> Integer
; to produce the first free chunk of given size in the hbs
;;; (check-expect (find-chunk HBS-1 1) 0)
;;; (check-expect (find-chunk HBS-1 2) 0)
;;; (check-expect (find-chunk HBS-1 4) -1)
;;; (check-expect (find-chunk HBS-4 8) 8)
