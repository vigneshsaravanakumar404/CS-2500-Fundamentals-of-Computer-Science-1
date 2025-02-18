; Exercise 1

(define-struct runner [name time])

; A Runner is a (make-runner String PosNumber)
; Interpretation: represents a runner with his fastest time
; - name is a String containing the runner's first name
; - time is a PosNumber containing the relay time
(define TIM (make-runner "Tim" 10))
(define BOB (make-runner "Bob" 20))
(define DYLAN (make-runner "Dylan" 23))
(define TIM2 (make-runner "Tim" 9))
(define BOB2 (make-runner "Bob" 19))
(define DYLAN2 (make-runner "Dylan" 22))
(define TIM3 (make-runner "Tim" 8))
(define BOB3 (make-runner "Bob" 18))
(define DYLAN3 (make-runner "Dylan" 21))
(define TIM4 (make-runner "Tim" 7))
(define BOB4 (make-runner "Bob" 17))
(define DYLAN4 (make-runner "Dylan" 20))
(define (runner-temp r)
  (... (runner-name r) ...
       (runner-time r) ...))

(define-struct rrt [runner rrt])

; An Rrt is one of:
; - TIM
; - (make-rrt Runner Rrt)
(define RRT-0 TIM)
(define RRT-1 (make-rrt BOB RRT-0))
(define RRT-2 (make-rrt DYLAN RRT-1))
(define RRT-01 TIM2)
(define RRT-11 (make-rrt BOB2 RRT-01))
(define RRT-21 (make-rrt DYLAN2 RRT-11))
(define RRT-02 TIM3)
(define RRT-12 (make-rrt BOB3 RRT-02))
(define RRT-22 (make-rrt DYLAN3 RRT-12))
(define RRT-03 TIM4)
(define RRT-13 (make-rrt BOB4 RRT-03))
(define RRT-23 (make-rrt DYLAN4 RRT-13))
(define (rrt-temp r)
  (...
   (cond
     [(runner? r) (runner-temp r)]
     [(rrt? r) (rrt-temp r)])...))

; Exercise 2

; total-time : Rrt -> PosNumber
; computes an Rrt's total race time
(check-expect (total-time RRT-0) 10)
(check-expect (total-time RRT-1) 30)
(check-expect (total-time RRT-2) 53)
(define (total-time r)
  (cond
    [(runner? r) (runner-time r)]
    [(rrt? r) (+ (runner-time (rrt-runner r)) (total-time (rrt-rrt r)))]))

; Exercise 3

; rescew-clock : Rrt -> Rrt
; rescews the clock
(check-expect (rescew-clock RRT-2) RRT-21)
(check-expect (rescew-clock RRT-21) RRT-22)
(check-expect (rescew-clock RRT-22) RRT-23)
(define (rescew-clock r)
  (cond
    [(runner? r) (make-runner (runner-name r) (sub1 (runner-time r)))]
    [(rrt? r) (make-rrt (rescew-clock (rrt-runner r)) (rescew-clock (rrt-rrt r)))]))

; Exercise 4
; cull-team : Rrt PosNumber -> Rrt
; Culls all team members who don't meet standards
(check-expect (cull-team RRT-2 11) RRT-0)
(check-expect (cull-team RRT-2 1) RRT-0)
(check-expect (cull-team RRT-2 25) RRT-2)
(define (cull-team r n)
  (cond
    [(runner? r) r]
    [(rrt? r)
     (if
      (<= (runner-time (rrt-runner r)) n)
      (make-rrt (rrt-runner r) (cull-team (rrt-rrt r) n))
      (cull-team (rrt-rrt r) n))]))

; Exercise 5

; A ListofStrings (LoS) is one of
; - '()
; - (cons String LoS)
; Interpretation: Represents a list of string
 
(define LOS-0 '())
(define LOS-1 (cons "bob" LOS-0))
(define LOS-2 (cons "alice" LOS-1))
(define (los-temp los)
  (...
   (cond [(empty? los) ...]
         [(cons? los) (... (first los) ...
                           (los-temp (rest los)) ...)])))

; contains-string? : LoS String -> Boolean
; Checks if list contains string
(check-expect (contains-string? LOS-0 "bob") #false)
(check-expect (contains-string? LOS-2 "bob") #true)
(check-expect (contains-string? LOS-2 "dylan") #false)
(define (contains-string? l s)
  (cond
    [(empty? l) #false]
    [(cons? l) (if (string=? (first l) s) #true (contains-string? (rest l) s))]))


; Exercise 6

; xor : Boolean Boolean -> Boolean
; Performs the xor operator
(define (xor p q)
  (and (not (and p q)) (or p q)))

; A ListofBooleans (LoB) is one of
; - '()
; - (cons String LoB)
; Interpretation: Represents a list of strings
 
(define LOB-1 (cons #true
                    (cons #true
                          (cons #true
                                (cons #true
                                      (cons #true (cons #true (cons #true (cons #true '())))))))))
(define LOB-2 (cons #false
                    (cons #false
                          (cons #true
                                (cons #false
                                      (cons #true (cons #true (cons #true (cons #true '())))))))))
(define LOB-3 '())
(define (lob-temp lob)
  (...
   (cond [(empty? lob) ...]
         [(cons? lob) (... (first lob) ...
                           (los-temp (rest lob)) ...)])))

; odd-true? : LoB -> Boolean
; checks if there is an odd number of #true
(check-expect (odd-true? LOB-1) #false)
(check-expect (odd-true? LOB-2) #true)
(check-expect (odd-true? LOB-3) #false)
(define (odd-true? l)
  (cond
    [(empty? l) #false]
    [(cons? l) (xor (first l) (odd-true? (rest l)))]))

; Exercise 7
; A Pet is one of:
; - "cat"
; - "dog"
; - "snake"
; - #f
; and represents a pet that someone owns;  #f represents any other kind of pet
 
(define pet-c "cat")
(define pet-d "dog")
(define pet-s "snake")
(define pet-o #f)
 
; pet-template : Pet -> ???
(define (pet-template p)
  (... (cond [(and (string? p) (string=? p "cat"))   ...]
             [(and (string? p) (string=? p "dog"))   ...]
             [(and (string? p) (string=? p "snake")) ...]
             [(boolean? p)                           ...])))


; A ListofPets (LoP) is one of
; - '()
; - (cons Pet LoP)
; Interpretation: Represents a list of pets
 
(define LOP-0 '())
(define LOP-1 (cons pet-c LOS-0))
(define LOP-2 (cons pet-d LOS-1))
(define (lop-temp lop)
  (...
   (cond [(empty? lop) ...]
         [(cons? lop) (... (first lop) ...
                           (los-temp (rest lop)) ...)])))

; Exercise 8
; all-noises : LoP -> LoS
(check-expect (all-noises LOP-0) '())
(check-expect (all-noises LOP-1) (cons "meow" '()))
(check-expect (all-noises LOP-2) (cons "bark" (cons "meow" '())))
(define (all-noises lp)
  (cond
    [(empty? lp) '()]
    [(cons? lp)
     (cond
       [(and (string? (first lp)) (string=? (first lp) "cat"))   (cons "meow" (all-noises (rest lp)))]
       [(and (string? (first lp)) (string=? (first lp) "dog"))   (cons "bark" (all-noises (rest lp)))]
       [(and (string? (first lp)) (string=? (first lp) "snake")) (cons "hiss" (all-noises (rest lp)))]
       [else (cons "unknown" (all-noises (rest lp)))])]))1