;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Lab 9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Exercise 1
; alternate : [list-of-X] [list-of-Y] -> [list-of-(union X Y)]
; to produce a list that alternates elements from l1 and l2
(check-expect (alternate '() '()) '())
(check-expect (alternate '() (list "a" "b")) (list "a" "b"))
(check-expect (alternate (list 1 2 3) '()) (list 1 2 3))
(check-expect (alternate (list 1 2) (list "a" "b" "c"))
              (list 1 "a" 2 "b" "c"))

(define (alternate l1 l2)
  (local [(define (alternate-helper l1 l2 i)
            (cond
              [(empty? l1) l2]
              [(empty? l2) l1]
              [(even? i) (cons (first l1) (alternate-helper (rest l1) l2 (+ i 1)))]
              [else (cons (first l2) (alternate-helper l1 (rest l2) (+ i 1)))]))]
    (alternate-helper l1 l2 0)))


; Exercise 2
; cross-product : [list-of-X] [list-of-Y] -> [list-of-[X Y]]
; to produce the cross-product of l1 and l2
(check-expect (cross-product '() '()) '())
(check-expect (cross-product '() (list "a" "b")) '())
(check-expect (cross-product (list 1 2 3) '()) '())
(check-expect (cross-product (list "a" "b" "c") (list "a" "b" "c"))
              (list (list "a" "a") (list "a" "b") (list "a" "c")
                    (list "b" "a") (list "b" "b") (list "b" "c")
                    (list "c" "a") (list "c" "b") (list "c" "c")))
(check-expect (cross-product (list 1 2) (list "a" "b" "c"))
              (list (list 1 "a") (list 1 "b") (list 1 "c")
                    (list 2 "a") (list 2 "b") (list 2 "c")))

(define (cross-product l1 l2)
  (local [(define (recurse-l1 l1 l2)
            (cond
              [(empty? l1) empty]
              [else (append (recurse-l2 (first l1) l2)
                            (recurse-l1 (rest l1) l2))]))
          (define (recurse-l2 element l2)
            (cond
              [(empty? l2) empty]
              [else (cons (list element (first l2))
                          (recurse-l2 element (rest l2)))]))]
    (recurse-l1 l1 l2)))
    

; Exercise 3
#|


;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname L22_P2-hw8-hints) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


Starting with the following layout for the allocation of disk space:

+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| 0 | 0 | 1 | 1 | 0 | 0 | 0 | 0 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Part A: Fill in the following bitmap to represent what the minimal normal form
HBS would be; being a normal form, there is only one correct answer.
(Note: it would probably be easier to just fill in the 1/#t's,
treating all empty boxes as 0/#f)

+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|               0               |               1               | Level-3 (8-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|       0       |       0       |       0       |       0       | Level-2 (4-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   0   |   1   |   0   |   0   |   0   |   0   |   0   |   0   | Level-1 (2-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | Level-0 (1-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Part B: Now, what would be the returned value (the starting block for the
chunk you allocate) if the user requests a 2-chunk?
Answer (block#): _2___

Fill in the updated HBS:
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|               0               |               1               | Level-3 (8-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|       0       |       0       |       0       |       0       | Level-2 (4-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   0   |   0   |   0   |   0   |   0   |   0   |   0   |   0   | Level-1 (2-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | Level-0 (1-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Part C: What would be the returned value if the user then requests
another 2-chunk?
Answer (block#): __8__

Fill in the updated HBS:
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|               0               |               0               | Level-3 (8-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|       0       |       0       |       0       |       1       | Level-2 (4-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   0   |   0   |   0   |   0   |   0   |   1   |   0   |   0   | Level-1 (2-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | Level-0 (1-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


======================================================================

Exercise 4:
Part A:
Start with the following HBS:
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|               0               |               0               | Level-3 (8-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|       1       |       0       |       0       |       1       | Level-2 (4-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   0   |   0   |   1   |   1   |   0   |   1   |   0   |   0   | Level-1 (2-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | Level-0 (1-chunks)
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

The user calls free-chunk to free up a chunk of size 2 at block# 6.
First, simply free the chunk by marking the correct single bit at the correct
level. Outline the steps involved.

First traverse down to the chunk size 2 level then free the block representing block 6. 


Part B:
As discussed in lecture today, the above setting of a freed bit will create
a new HBS that is correctly minimal, but not fully normalized. So that's
what you would do next.
Let's assume the function that normalizes the HBS is called "normalize-hbs",
and that it takes two parameters: the HBS that needs normalizing, and the
level to fix down to, specified as a chunk size. Outline the steps involved.
Remember: you should use standard structureal recursion.

We have provided the first few steps for you:
1. Call normalize-hbs with the args "the entire HBS" (so, HBS-at-Level-3)
   and chunksize=2, since that's where the chunk was freed, and so a #t
   was added.
2. Checks chunksize arg against level's chunksize (2 vs. 8), recurses
    by calling normalize-hbs with HBS-at-level-2

3.    Checks chunksize arg against level's chunksize (2 vs. 4), recurses

4.      Checks chunksize arg against level's chunksize (2 vs. 2); this
        is base case; interpretation: this is level at which #t was added,
        but since recursion acts like the hbs arg is in fact the entire
        HBS, top level can have pairs of #t in normal form, so just returns
5.    After return from (normalize-hbs Level-1 ...), back to
      (normalize-hbs Level-2 ...) it does ...

6.After returning from (normalize-hbs Level-1 ...), back to (normalize-hbs Level-2 ...), it checks if the current level's chunksize matches the expected chunksize. If it does, it proceeds to combine the normalized chunks at this level.

7. If the chunksize at Level-2 matches the expected chunksize, it merges the normalized chunks into a single chunk, ensuring that the HBS at this level is fully normalized.

8. The function then returns to (normalize-hbs Level-3 ...), where it checks if the chunksize at Level-3 matches the expected chunksize.

9. If the chunksize at Level-3 matches the expected chunksize, it merges the normalized chunks at this level, ensuring that the HBS at this level is fully normalized.

10. The function continues this process recursively, moving up each level and merging normalized chunks until the entire HBS is fully normalized.

11. Finally, the function returns the fully normalized HBS, ensuring that all levels are correctly normalized according to the specified chunksize.



Part B: Time permitting, start converting above pseudocode into real
ISL code.
|# 