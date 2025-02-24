;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW7-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define-struct user [username friends rating])
(define-struct proctor [username friends community rank])
 
; A SiteMember is one of:
; - (make-user String [List-of String] NatNum)
; - (make-proctor String [List-of String] String NatNum)
; and represents either:
; - a regular user's login username, friends' usernames, and community rating
; - a site supervisor's login user name, friends' usernames, community they manage,
;   and their supervisory rank
 
(define SM-USER-1
  (make-user "fundies1"
             (list "jo6n")
             2))
 
(define SM-USER-2
  (make-user "jo6n"
             (list "fundies1" "adam12")
             4))
 
(define SM-PROCTOR-1
  (make-proctor "agent86"
                (list "adam12" "agent99")
                "Control Agents"
                99))
 
(define SM-PROCTOR-2
  (make-proctor "adam12"
                (list "agent86" "fundies1")
                "CHIPs"
                12))
 
; sitemember-temp : SiteMember -> ?
(define (sitemember-temp sm)
  (... (cond [(user? sm)    (... (user-username sm)
                                 (los-temp (user-friends sm))
                                 (user-rating sm) ...)]
             [(proctor? sm) (... (proctor-username sm)
                                 (los-temp (proctor-friends sm))
                                 (proctor-community sm)
                                 (proctor-rank sm) ...)])))

; ============================================================================================
(define SM-PROCTOR-3
    (make-proctor "Benjamin Koopferstock"
                  (list "agent86" "fundies1" "fundies1" "fundies1" "fundies1")
                  "CHIPs"
                  12))


; a.
; only-proctors : [List-of SiteMember] -> [List-of Proctor]
; Interpretation: To return a list of proctors from a list of site members.
(check-expect (only-proctors (list SM-USER-1 SM-USER-2 SM-PROCTOR-1 SM-PROCTOR-2))
              (list SM-PROCTOR-1 SM-PROCTOR-2))
(check-expect (only-proctors (list SM-USER-1 SM-PROCTOR-1 SM-PROCTOR-2))
              (list SM-PROCTOR-1 SM-PROCTOR-2))
(check-expect (only-proctors (list SM-USER-1 SM-PROCTOR-2))
              (list SM-PROCTOR-2))           
(check-expect (only-proctors empty) empty)

(define (only-proctors l)
    (filter proctor? l))


; b.
; all-friends : [List-of SiteMember] -> [List-of String]
; Interpretation: To return a list of all friends of all site members.
(check-expect (all-friends (list SM-USER-1 SM-USER-2 SM-PROCTOR-1 SM-PROCTOR-2))
              (list "agent86" "fundies1" "adam12" "agent99" "fundies1" "adam12" "jo6n"))
(check-expect (all-friends (list SM-USER-2 SM-PROCTOR-1 SM-PROCTOR-2))
              (list "agent86" "fundies1" "adam12" "agent99" "fundies1" "adam12"))
(check-expect (all-friends (list SM-USER-2 SM-PROCTOR-2))
              (list "agent86" "fundies1" "fundies1" "adam12"))
(check-expect (all-friends empty) empty)

(define (all-friends l)
    (foldl append empty (map choose-friends l)))

; choose-friends : SiteMember -> Pred
; Interpretation: Return a predicate that is true for the given site member.
(check-expect (choose-friends SM-USER-1) (list "jo6n"))
(check-expect (choose-friends SM-PROCTOR-1) (list "adam12" "agent99"))
(check-expect (choose-friends SM-PROCTOR-2) (list "agent86" "fundies1"))
(check-expect (choose-friends SM-PROCTOR-3) (list "agent86" "fundies1" "fundies1" "fundies1" "fundies1"))
(check-expect (choose-friends SM-USER-2) (list "fundies1" "adam12"))

(define (choose-friends sm)
  (cond [(user? sm) (user-friends sm)]
        [(proctor? sm) (proctor-friends sm)]))


; c.
; num-users : [List-of SiteMember] -> NatNum
; Interpretation: To return the number of users in the given list of site members.
(check-expect (num-users (list SM-USER-1 SM-USER-2 SM-PROCTOR-1 SM-PROCTOR-2)) 2)
(check-expect (num-users (list SM-USER-1 SM-PROCTOR-1 SM-PROCTOR-2)) 1)
(check-expect (num-users empty) 0)

(define (num-users l)
    (length (filter user? l)))


; d. 
; popular? : SiteMember -> Boolean
; Interpretation: If at least one sitemember has more than 1 friend
(check-expect (popular? (list SM-USER-1 SM-USER-2 SM-PROCTOR-1 SM-PROCTOR-2)) #true)
(check-expect (popular? (list SM-USER-1)) #false)

(define (popular? l)
    (foldl one-of #false l))

; one-of : [List-of SiteMember] Boolean -> Boolean
; Interpretation: To return true if at least one site member has more than 1 friend.
(define (one-of l c)
    (or (> (length (choose-friends l)) 1) c))


; e.
; most-popular : [List-of SiteMember] -> SiteMember
; Interpretation: To return the most popular site member from the given list.
(check-expect (most-popular (list SM-USER-1 SM-USER-2 SM-PROCTOR-1 SM-PROCTOR-2 SM-PROCTOR-3)) 5)
(check-expect (most-popular empty) 0)

(define (most-popular l)
    (foldl most-friends 0 l))

; friend-count : [List-of SiteMember] -> Number
; Interpretation: To return the number of friends of all site members in the given list.
(define (most-friends l m)
    (max m (length (choose-friends l))))


; f.
; highest-rating : [List-of SiteMember] -> Number
; Interpretation: To return the highest rating of all site members in the given list.
(check-expect (highest-rating (list SM-USER-1 SM-USER-2 SM-PROCTOR-1 SM-PROCTOR-2)) 99)
(check-expect (highest-rating empty) 0)

(define (highest-rating l)
    (foldl most-rating 0 l))

; choose-rating : SiteMember -> Number
; Interpretation: To return the rating of the given site member.
(define (choose-rating sm)
  (cond [(user? sm) (user-rating sm)]
        [(proctor? sm) (proctor-rank sm)]))

; most-rating : [List-of SiteMember] -> Number
; Interpretation: To return the highest rating of all site members in the given list.
(define (most-rating l m)
    (max m (choose-rating l)))