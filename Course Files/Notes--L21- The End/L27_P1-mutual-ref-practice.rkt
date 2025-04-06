;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname P1-mutual-ref-practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; PRACTICE WITH MUTUALLY REFERENTIAL DATA
;
; ANNOUNCEMENTS
; HW questions?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Org charts
;
;                      Joseph E. Aoun
;                       (President)
;                            |
;                         Cabinet                                   
;                            |
;        +-------------------+-------------------+
;    Karl Reid      Kenneth W. Henderson   David Madigan
;  (Chief Inclusion    (Chancellor)          (Provost)
;      Officer)                                 |
;                     +-------------------------+---------+
;                     |                                   |
;               Administration                      Academic Deans
;                     |                                   |
;                     |                  +----------------+---------------+
;                     |                  |                |               | 
;               Thomas Sheahan    Elizabeth Mynatt  Carmen Sceppa     Uta Poiger
;            (Exec Vice Provost)      (Khoury)         (Bouve)     (Social Sciences &
;                                                                     Humanities)
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the data OrgMember, where people have a name and a title,
; as well as any number of groups of direct reports, each with a label.
; 
;<BRAINSTORM>








(define-struct person [name title reports])
(define-struct group [name members])

; An OrgMember is a (make-person String String [List-of Group])
; Interpretation: a person in an org chart with their name, title, and reports

; [>>> NB: In the lecture videos, this data def was originally named "OrgChart"]

; A Group is a (make-group String [List-of OrgMember])
; Interpretation: a group of reports with its name and member(s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Now let's define some examples

(define ORG-WALK
  (make-person "Thinker" "Leader"
               (list (make-group "Followers" '()))))

; Write the defs for ORG-PROVOST and ORG-PREZ
;<DO NOW><SCROLL>








(define ORG-PROVOST
  (make-person "David Madigan" "Provost"
               (list (make-group "Administration"
                                 (list (make-person "Thomas Sheahan" "Exec Vice Provost" '())))
                     (make-group "Academic Deans"
                                 (list (make-person "Elizabeth Mynatt" "Khoury" '())
                                       (make-person "Carmen Sceppa" "Bouve" '())
                                       (make-person "Uta Poiger" "CSSH" '()))))))

(define ORG-PREZ
  (make-person "Joseph E. Aoun" "President"
               (list (make-group "Cabinet"
                                 (list (make-person "Karl Reid" "Chief Inclusion Officer" '())
                                       (make-person "Kenneth W. Henderson" "Chancellor" '())
                                       ORG-PROVOST)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Create the templates. How many should we define?
;<DO NOW><SCROLL>








(define (org-temp org)
  (... (person-name org) ...
       (person-title org) ...
       (log-temp (person-reports org)) ...))

(define (log-temp log)
  (...
   (cond [(empty? log) ...]
         [(cons? log) (... (group-temp (first log)) ...
                           (log-temp (rest log)) ...)])))

(define (group-temp group)
  (... (group-name group) ...
       (loo-temp (group-members group)) ...))
               
(define (loo-temp loo)
  (...
   (cond [(empty? loo) ...]
         [(cons? loo) (... (org-temp (first loo)) ...
                           (loo-temp (rest loo)) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function num-peeps that counts how many people are
; in an org chart.

; num-peeps : OrgMember -> Nat
; counts people in an org chart

(check-expect (num-peeps ORG-WALK) 1)
(check-expect (num-peeps ORG-PROVOST) ???)
(check-expect (num-peeps ORG-PREZ) ???)

(define (num-peeps org)
  ;<DO NOW><SCROLL> Start w/appropriate template from above
  ; Use list abstractions where appropriate







  
  (local [; num-peeps/group : Group -> Nat
          ; counts people in a group
          (define (num-peeps/group group)
            (foldr + 0
                   (map num-peeps (group-members group))))]
    (add1 (foldr + 0
                 (map num-peeps/group (person-reports org))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function full-title that takes an org chart and the name of an
; organization, and produces an org chart that adds the "(organization name)"
; to the title for each person.

; full-title : OrgMember String -> OrgMember
; adds an organization name to the title of all people in the org chart

(check-expect
 (full-title ORG-WALK "Jokes")
 (make-person "Thinker"
              "Leader (Jokes)"
              (list (make-group "Followers" '()))))


(check-expect
 (full-title ORG-PREZ "NU")
 (make-person "Joseph E. Aoun"
              "President (NU)"
              (list (make-group "Cabinet"
                                (list (make-person "Karl Reid"
                                                   "Chief Inclusion Officer (NU)"
                                                   '())
                                      (make-person "Kenneth W. Henderson"
                                                   "Chancellor (NU)"
                                                   '())
                                      (make-person "David Madigan"
                                                   "Provost (NU)"
                                                   (list (make-group "Administration"
                                                                     (list (make-person "Thomas Sheahan"
                                                                                        "Exec Vice Provost (NU)"
                                                                                        '())))
                                                         (make-group "Academic Deans"
                                                                     (list (make-person "Elizabeth Mynatt"
                                                                                        "Khoury (NU)"
                                                                                        '())
                                                                           (make-person "Carmen Sceppa"
                                                                                        "Bouve (NU)"
                                                                                        '())
                                                                           (make-person "Uta Poiger"
                                                                                        "CSSH (NU)"
                                                                                        '()))))))))))

#;
(define (full-title org org-name)
  ;<DO NOW><SCROLL> helpers: (full-title/org org), (full-title/group group), (fix-title str)
  (local [; full-title/org : OrgMember -> OrgMember
          ; fixes the title for an org
          (define (full-title/org org)
            (... (person-name org) ...
                 (person-title org) ...
                 (log-temp (person-reports org)) ...)) ; helper: abst, maybe? :-)

          ; fix-title : String -> String
          ; adds org-name to the title
          (define (fix-title t)
            (string-append t " (" org-name ")"))

          ; full-title/group : Group -> Group
          ; fixes the title for a group
          (define (full-title/group group)
            (... (group-name group) ...
                 (loo-temp (group-members group)) ...)) ; helper: abst, maybe? :-)

          ; ... and any other helpers required
          ]
    (full-title/org org)))
;<SCROLL>







(define (full-title org org-name)
  ;<DO NOW><SCROLL> helpers: (full-title/org org), (full-title/group group), (fix-title str)
  (local [; full-title/org : OrgMember -> OrgMember
          ; fixes the title for an org
          (define (full-title/org org)
            (make-person (person-name org)
                         (fix-title (person-title org))
                         (map full-title/group (person-reports org))))

          ; fix-title : String -> String
          ; adds org-name to the title
          (define (fix-title t)
            (string-append t " (" org-name ")"))

          ; full-title/group : Group -> Group
          ; fixes the title for a group
          (define (full-title/group group)
            (make-group (group-name group)
                        (map full-title/org (group-members group))))]
    (full-title/org org)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function prune that takes an org chart and a list of strings,
; and produces an org chart that only contains group names that are in the
; supplied list.

; prune : OrgMember [List-of String] -> OrgMember
; keeps only those groups whose names are included in the supplied list

(check-expect (prune ORG-WALK '()) (make-person "Thinker" "Leader" '()))
(check-expect (prune ORG-WALK (list "Followers" "Administration" "Cabinet")) ORG-WALK)

(check-expect (prune ORG-PREZ (list "Followers" "Administration" "Cabinet"))
              (make-person "Joseph E. Aoun"
                           "President"
                           (list (make-group "Cabinet"
                                             (list (make-person "Karl Reid"
                                                                "Chief Inclusion Officer"
                                                                '())
                                                   (make-person "Kenneth W. Henderson"
                                                                "Chancellor"
                                                                '())
                                                   (make-person "David Madigan"
                                                                "Provost"
                                                                (list (make-group "Administration"
                                                                                  (list (make-person "Thomas Sheahan"
                                                                                                     "Exec Vice Provost"
                                                                                                     '()))))))))))

(define (prune org groups)
  (local [; keep-group? : Group -> Boolean
          ; is the group name in the supplied list?
          (define (keep-group? group)
            (ormap (λ (g) (string=? g (group-name group)))
                   groups))

          ; prune/org : OrgMember -> OrgMember
          ; prunes an org chart
          (define (prune/org org)
            (make-person (person-name org)
                         (person-title org)
                         (map prune/group
                              (filter keep-group? (person-reports org)))))

          ; prune/group : Group -> Group
          ; prunes a group
          (define (prune/group group)
            (make-group (group-name group)
                        (map prune/org (group-members group))))]
    (prune/org org)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise:
; Design the function title-finder that takes an org chart and produces a
; function which, when supplied a title, determines whether a person with
; that tile exists within the org chart.

; title-finder : OrgMember -> [String -> Boolean]
; produces a function that determines whether a title
; exists in the supplied organization

(check-expect ((title-finder ORG-WALK) "President") #f)

(check-expect ((title-finder ORG-PREZ) "President") #t)

(check-expect ((title-finder ORG-PREZ) "Khoury") #t)

(check-expect ((title-finder ORG-PROVOST) "President") #f)

(check-expect ((title-finder ORG-PROVOST) "Khoury") #t)

(define (title-finder org)
  ;<DO NOW><SCROLL>







  
  (λ (t)
    (local [; title-finder/org : OrgMember -> Boolean
            ; does the title exist in the org?
            (define (title-finder/org org)
              (or (string=? t (person-title org))
                  (ormap title-finder/group (person-reports org))))

            ; title-finder/group : Group -> Boolean
            ; does the title exist in the group?
            (define (title-finder/group group)
              (ormap title-finder/org (group-members group)))]
      (title-finder/org org))))
    
