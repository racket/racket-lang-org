#lang at-exp racket/base

;; More detailed information about the venue where a given RCon is held

(require racket/match
         racket/string
         txexpr
         (prefix-in gregor: gregor)
         "lib.rkt"
         "commonelements.rkt")

(provide page)

;; ------------------------------------------------------------

(define (speaker #:person? [person? #t]
                 #:url [url #f]
                 #:affiliation [affiliation #f]
                 . x)
  (when (and person? (not (non-empty-string? url)))
    (error "Every person needs a URL"))
  (define span-kids
    (cond [(not person?)
           x]
          [(not (non-empty-string? url))
           (error "Every person needs a URL")]
          [else
           (define name (apply string-append x))
           (define attrs
             (append (list (list 'href url)
                           (list 'title name))
                     (cond [(non-empty-string? affiliation)
                            (list (list 'class "h-card"))]
                           [else
                            (list (list 'class "unaffiliated"))])))
           (cond [(non-empty-string? affiliation)
                  (list (txexpr* 'a attrs
                                 (bold name)
                                 " ("
                                 (txexpr* 'span
                                          (list (list 'class "p-org"))
                                          affiliation)
                                 ")"))]
                 [else
                  (list (txexpr* 'a attrs (bold name)))])]))
  (txexpr 'span
          (list (list 'class "speaker-a"))
          span-kids))
(define (lecture #:when when
                 #:who who
                 #:link [l #f]
                 #:what [what ""]
                 #:more [more ""]
                 #:even-more [even-more ""]
                 #:bio [bio #f]
                 #:first? [first? #f])
  ((if first? first-speech speech) when
                                   who
                                   (if l
                                       (live-link "" (a #:href l "talk video"))
                                       "")
                                   what
                                   more
                                   even-more
                                   (or bio "")))

(define (hallway when)
 (lecture #:when when #:who @speaker[#:person? #f]{@activity{Hallway}}))

(define (doors-open when)
  (lecture #:when when #:who @speaker[#:person? #f]{@activity{Doors Open}}
           #:first? #t))

(define (social #:when when #:where [where ""] #:more [more ""])
  (lecture #:when when #:who @speaker[#:person? #f]{@activity{Evening Social}}
           #:what where
           #:more more))

(define (coffee when)
 (lecture #:when when #:who @speaker[#:person? #f]{@activity{Coffee}}))

(define (break when)
 (lecture #:when when #:who @speaker[#:person? #f]{@activity{Break}}))

(define (lunch when)
 (lecture #:when when #:who @speaker[#:person? #f]{@activity{Lunch}}))

(define (keynote when #:who who #:what what #:more more #:link [link #f]
                 #:desc [desc "Keynote"] #:bio [bio #f])
  (lecture #:when when #:who @speaker[#:person? #f]{@activity{@desc}}
           #:what (keynote-speaker who) #:link link #:more what
           #:even-more more #:bio bio))

(define (bio . contents)
 (apply bio-div @bio-label{Bio: } contents))

(define (q content)
  `(q () ,content))

(define (at-where name addr)
  `(div ()
        (div ,name)
        (div ,addr)))

(define saturday (gregor:date 2026 10 3))
(define sunday (gregor:date 2026 10 4))
(define location "Oakland, California, USA")

(define (meta #:itemprop [itemprop #f]
              content)
  (define elem (txexpr* 'meta (list (list 'content content))))
  (cond [(non-empty-string? itemprop)
         (attr-set elem 'itemprop itemprop)]
        [else elem]))

(define slot-number 0)
(define (talk-time dtime)
 (set! slot-number (add1 slot-number))
 (local-require racket/string gregor)
 (match-define (list day times) (string-split dtime ","))
 (define d (match day
             ["Saturday" saturday]
             ["Sunday"   sunday]))
 (define t (parse-time times " h:mmaa"))
 (define tz (with-timezone (on-date t d) "America/New_York"))
 (define m (adjust-timezone tz "Etc/UTC"))
 (talk-time-div
  `(span ([data-slot-time ,(moment->iso8601 m)])
    ,(~t tz "EEEE, h:mma zz"))))

(define nb-yes-lunch
  @nb{Lunch is provided.})

(define nb-no-breakfast
  @nb{Breakfast won’t be served, so please eat before coming to the event.})

(define nb-yes-breakfast
  @nb{Light breakfast served.})

;; ------------------------------------------------------------

(define page
  (html #:lang "en"
   (head
    (head-meta #:http-equiv "content-type" #:content "text/html; charset=utf-8")
    (link #:href fonts-url
          #:rel "stylesheet")
    (style (cdata #f #f (classes->string)))
    (style (cdata #f #f "a { text-decoration: none; } li { text-align: left; } "))
    @title{(sixteenth RacketCon) Local Information})
   (body
     #:class "main h-event"
     (content
      (banner
       @subtitle{@a[#:href "index.html"]{RacketCon 2026} Local Information})

(column
       (section
        @sectionHeader{Location}

        (column
         @specific-location{Oakstop, Broadway Gallery Suite}
         @specific-location{1721 Broadway suite #201}
         @specific-location{Oakland, CA 94612}
         @specific-location{That's right, at the 18th Street BART (rapid transit) stop.}))

      (section
       @sectionHeader{Hotel}
       (column
        @vpara{To be confirmed...}))
      
      #; 
      (section
       @sectionHeader{Saturday Social}
       (column
        @vpara{The Saturday social event will take place /at some time/ at
                   @a[#:href ""]{/some location/} /some more info about location/}))
      #;
      (section
       @sectionHeader{Getting There}

       (column

        @vpara{@a[#:href "umb-navigation.pdf"]{/Possible navigation summary in PDF/}}

        @fromplace{From /the recommended hotel/}

        @vpara{Walk (/duration/):}

        @ul{
            @li{Step 1}
            @li{Step 2}
            @li{Step N}
            }

        @vpara{or /here is how to take an alternative route (e.g bus)/}

        @ul{
            @li{Step 1 (duration)}
            @li{Step 2 (duration)}
            @li{Step n (duration)}
            }


        @fromplace{From /the nearest Airport/}

        @ul{
            @li{Do this}
            @li{Do that}
            }

        @vpara{Then /more info on how to get there/}        

        @fromplace{From South Station}

        @ul{
            @li{Step 1}
            @li{Step 2}
            @li{Step 3}
           }

       @fromplace{Parking}

       @vpara{The closest parking garage to Oakstop is ...}

       @vpara{If that is full, parking is also available in ...}

       @vpara{See (link to local parking info) for rates and locations of all parking garages.}

       @fromplace{See Also}

       @vpara{Even more links with local information on how to get to Oakstop}
       ))

      (section
       @sectionHeader{Wi-Fi}
       (column
       #;
       @vpara{Attendees with university credentials should be able to access the
                            internet using eduroam.}

       @vpara{For all attendees, there will be network access via the venue WiFi network.}
       #;
       @vpara{Alternately, the @tt{UMB-Guest} network may also be used (@a[#:href "https://www.umb.edu/campus_center/services/wireless_access"]{details here}).})))))))
