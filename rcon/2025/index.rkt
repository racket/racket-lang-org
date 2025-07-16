#lang at-exp racket/base

(require racket/match
         racket/string
         xml
         txexpr
         (prefix-in gregor: gregor)
         "lib.rkt")

(define-div main
  [font-family "'Montserrat', sans-serif"]
  [background "blue"]
  [color "black"]
  [margin "0px"])

(define-div content
  ,@centered
  [background "white"]
  [margin-left "10ex"]
  [margin-right "10ex"]
  [padding-top "2ex"]
  [padding-bottom "2ex"])

(define-div column
  ,@centered
  [max-width "45em"])

(define-div banner
  [padding-top "1ex"]
  ,@centered)

(define-div title-container
  [display inline-block]
  (margin-left auto)
  (margin-right auto)
  [text-align left]
  [margin-bottom "1em"])

(define-div title-append
  [display flex])

(define-div pagetitle
  [font-size "40pt"]
  [font-family ,monospace])

(define header-font
  `(#;[font-weight "bold"]))

(define-div subtitle
  ,@centered
  [font-size "32pt"]
  [font-weight "bold"]
  ,@header-font)

(define-div subsubtitle
  ,@centered
  [font-size "20pt"]
  ,@header-font)

(define-div section
  [margin-top "3em"])
(define-div sectionHeader
  [font-size "24pt"]
  [margin-bottom "1em"]
  [background "#CCCCEE"]
  [margin-left "-10ex"]
  [margin-right "-10ex"]
  ,@header-font)

(define-div speaker-a
  [color "firebrick"])
(define-a unaffiliated
  [color "inherit"])
(define-a h-card
  [color "inherit"])

(define-span activity)

(define-div talk
  [font-weight bold]
  #;
  [font-style "italic"]
  [font-size "24pt"]
  [margin-top "0.25em"]
  [margin-bottom "0.5em"]
  [color "gray"])

(define-div place
  [font-weight bold]
  #;
  [font-style "italic"]
  [font-size "24pt"]
  [margin-top "0.25em"]
  [color "gray"])

(define-div place-address
  [margin-bottom "0.5em"])

(define-div abstract
  [text-align "left"]
  [margin-left "5em"]
  [margin-right "5em"])

(define-div paragraph
  [text-align "left"])
(define-div center
  [text-align "center"])
(define para paragraph)

(define-div plain)

(define-div joint
  [color "gray"])

(define-div larger
  [font-size "24pt"])

(define-div featuring
  [white-space "nowrap"]
  [font-size "18pt"]
  [margin-top "1em"]
  [color "blue"])

(define-span featured
  [font-weight bold])

(define-span bold
  [font-weight bold])

(define-span emph
  [font-style "italic"])
(define-span book-title
  [font-style "italic"])

(define-span tt
  [font-family ,monospace])

(define-span faded
  [color "gray"])

(define-span nop)

(define-div talk-time-div
  [font-weight bold]
  [position absolute]
  [color "gray"])

(define-div live-link
  [position absolute]
  [right 0]
  [top 0])

(define-div speech
  [margin-top "3em"]
  [position relative])

(define-div first-speech
  [margin-top "1em"]
  [position relative])

(define-div bio-div
  [margin-top "0.5em"]
  [text-align "left"]
  [margin-left "5em"]
  [margin-right "5em"])


(define-span bio-label
  [font-weight "bold"]
  [color "gray"])

(define-div keynote-speaker
  [font-size "24pt"]
  [font-weight "bold"])

(define-div nb
  [text-align "center"]
  [font-style "italic"])

(define-div p-location
  [font-size "24pt"]
  [font-weight "bold"])

(define-div specific-location
  [font-size "18pt"]
  [margin-top "0.25em"])

(define-div specific-location-cotd
  [font-size "18pt"])

(define-div picture
  [margin-top "2em"])

(define (script . contents)
 `(script ,@(map (λ (x) (cdata #f #f x)) contents)))

(define (code content)
 `(code () ,content))

(define mailto:con-organizers
  @(a #:href "mailto:con-organizers@racket-lang.org" "con-organizers@racket-lang.org"))

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

(define saturday (gregor:date 2025 10 4))
(define sunday (gregor:date 2025 10 5))
(define location "Boston, Massachusetts, USA")

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

(define nb-breakfast
  @nb{Breakfast won’t be served, so please eat before coming to the event.})

;; ------------------------------------------------------------

(define page
  (html #:lang "en"
   (head
    (head-meta #:http-equiv "content-type" #:content "text/html; charset=utf-8")
    (link #:href fonts-url
          #:rel "stylesheet")
    (style (cdata #f #f (classes->string)))
    (style (cdata #f #f "a { text-decoration: none; } "))
    `(script ([src      "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.3/moment.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment-timezone/0.5.34/moment-timezone-with-data-10-year-range.js"]) "")
    @title{(fifteenth RacketCon)}
    @script{
$(document).ready(function () {
 $("[data-slot-time]").each(function() {
  var date = new Date($(this).data("slot-time"));
  var localTime = moment.tz(date, "America/New_York").format("dddd, h:mma zz")
  $(this).html(localTime); }); }); })
    (body
     #:class "main h-event"
     #:itemscope ""
     #:itemtype "https://schema.org/Event"
     (meta #:itemprop "startDate" (gregor:~t saturday "y-MM-d"))
     (meta #:itemprop "endDate" (gregor:~t sunday "y-MM-d"))
     (meta #:itemprop "location" location)
(content
     (banner
      (title-container
       (title-append
        @pagetitle[(img #:style "width:140px; float: right"
                        #:src "https://racket-lang.org/img/racket-logo.svg"
                        #:alt "The Racket logo")]
        @pagetitle["(fifteenth" (br) 'nbsp "RacketCon)" 'nbsp 'nbsp 'nbsp]))
      @subtitle{October 4-5, 2025}
      @subtitle{@p-location{UMass Boston}}
      @specific-location{Boston, Massachusetts, USA}
      )

(txexpr* 'time `((class "dt-start") (hidden "") (datetime ,(gregor:~t saturday "y-MM-dd"))))
(txexpr* 'time `((class "dt-end") (hidden "") (datetime ,(gregor:~t sunday "y-MM-dd"))))

(column

 ;; @featuring{Special Guests TBC}

 (section
   @para{RacketCon is a public gathering dedicated to fostering a
   vibrant, innovative, and inclusive community around the Racket
   programming language. We aim to create an exciting and enjoyable
   conference open to anyone interested in Racket, filled with inspiring
   content, reaching and engaging both the Racket community and the
   wider programming world.})

 #;
 (section
  @sectionHeader{Call for Presentations}
  @para{We are looking for @emph{you!} If you have an idea for
  a presentation you’d like to give, please write to @(a #:href
  "mailto:con-organizers@racket-lang.org" #:title "Send mail to the
  RacketCon organizer" "the RacketCon organizers") for consideration.
  All Racket-y ideas are welcome. We’d love to have you!})

 (section
  @sectionHeader{Saturday, October 4th}
  @doors-open[@talk-time{Saturday, 8:30am}]
  @keynote[
   @talk-time{Saturday, 9:00am}
   #:desc "Keynote"
   #:who @joint{
    @speaker[#:url "https://cloudflare.com"]{James Larisch}
    @speaker[#:url "https://cloudflare.com"]{Suleman Ahmad}
   }
   #:what @talk{
    How Cloudflare Uses Racket and Rosette to Verify DNS Changes
   }
   #:more @abstract{
    @paragraph{
     Since 2022, Cloudflare has used Racket and Rosette to prevent DNS-related bugs.
     Cloudflare engineers express desired DNS behavior as small programs called policies,
     written in a custom DSL called topaz-lang. Topaz-lang policies are executed in
     real-time on Cloudflare’s global edge network in response to live DNS queries. But
     before deployment, all policies are checked for bugs using a verifier we wrote in
     Rosette, a solver-aided Racket #lang.
    }
    @paragraph{
     In this talk, we describe our experience writing and using Racket in production
     at Cloudflare. We describe why managing DNS behavior at Cloudflare scale is so
     challenging, and how these challenges motivated topaz-lang and its parent system Topaz.
     We discuss why we chose Racket (and Rosette) and the types of bugs our Rosette verifier
     detects. Finally, we reflect on why making changes to our verifier remains daunting for
     many software engineers.
    }
   }
   #:bio @bio{
    Suleman is a Research Engineer at Cloudflare, working at the intersection of systems engineering
    and Internet security. He holds a Master's degree from the University of Wisconsin–Madison,
    where he focused on analyzing security and privacy challenges in large-scale Internet
    architectures and engineering scalable measurement platforms. It was during his master's studies
    that he developed an appreciation for functional programming and its practical application to
    verifiable distributed systems.

    @paragraph{
     James is a systems/security researcher and programming language fanboy. He developed his
     appreciation for the functional style (and Racket) during his undergraduate degree at
     Northeastern University. He received his PhD in Computer Science from Harvard University,
     where one of his projects involved bringing Prolog to the Web Public Key Infrastructure. He
     is currently a Research Engineer at Cloudflare, where he works on the Web PKI, distributed
     systems, and a bit of formal methods.
    }
   }
  ]
  @coffee[@talk-time{Saturday, 10:00am}]
  @lecture[
   #:when @talk-time{Saturday, 10:15am}
   #:who @speaker[#:url "https://mukn.com"]{François-René Rideau}
   #:what @talk{Compositional Object Oriented Prototypes}
   #:more @abstract{}
  ]
  @lecture[
   #:when @talk-time{Saturday, 10:45am}
   #:who @speaker[#:url "https://github.com/quasarbright"]{Mike Delmonaco}
   #:what @talk{A Match-Like DSL for Deep Immutable Updates}
   #:more @abstract{
    @code{match} is very convenient for deconstructing data and accessing values deep within a data
    structure, but it is not useful for making changes to that data structure. In this talk, I’ll
    present a DSL that looks like match, but allows you to perform immutable updates on the target
    value using pattern variables to specify where an update should occur. I’ll also talk about
    optics, which are the abstraction powering these immutable updates.
   }
   #:bio @bio{
    Mike Delmonaco is a Software Engineer at Amazon Web Services with a hobby interest in
    Programming Languages and Racket. Outside of work, he enjoys rock climbing, video games,
    creating interactive math visualizations, programming language research, and teaching.
   }
  ]
  @lunch[@talk-time{Saturday, 11:45am}]
  @lecture[
   #:when @talk-time{Saturday, 1:30pm}
   #:who @speaker[#:url "https://github.com/toddjonker"]{Todd Jonker}
   #:what @talk{Ion Fusion}
   #:more @abstract{}
   ;#:bio @bio{}
  ]
  @lecture[
   #:when @talk-time{Saturday, 2:00pm}
   #:who @speaker[#:url "https://www.greghendershott.com/"]{Greg Hendershott}
   #:what @talk{racket-mode}
   #:more @abstract{}
   ;#:bio @bio{}
  ]
  @lecture[
   #:when @talk-time{Saturday, 2:30pm}
   #:who @speaker[#:url "https://camoy.net/"]{Cameron Moy}
   #:what @talk{roulette}
   #:more @abstract{}
   ;#:bio @bio{}
  ]
  @break[@talk-time{Saturday, 3:00pm}]
  @lecture[
   #:when @talk-time{Saturday, 3:30pm}
   #:who @joint{
    @speaker[#:url "https://github.com/ariscript"]{Ari Prakash}
    @speaker[#:url "https://github.com/zackbach/"]{Zachary Eisbach}
   }
   #:what @talk{miniDusa: An Extensible Finite-Choice Logic Programming Language}
   #:more @abstract{
    Dusa is a recently designed logic programming language featuring mutually exclusive choice as a
    primitive to enable computation of solutions that satisfy constraints. To explore further host
    integration, we introduce miniDusa, a Dusa-inspired hosted domain-specific language implemented
    using Racket and the syntax-spec metalanguage. This architecture lets us inherit tooling,
    extensibility, and interoperability features from Racket essentially “for free”.
   }
   #:bio @bio{
    Zachary Eisbach is a student at Northeastern University studying Mathematics and Computer
    Science. He is interested in compilers, logic, and safe interoperability.
    @paragraph{
     Ari Prakash is a student at Northeastern University studying Computer Science. She is
     interested in developer tooling to make creating reliable systems easier.
    }
   }
  ]
  @lecture[
   #:when @talk-time{Saturday, 4:00pm}
   #:who @speaker[#:url "https://github.com/jjsimpso"]{Jonathan Simpson}
   #:what @talk{Browsing(and serving) the Slow Internet with Racket}
   #:more @abstract{
    Taking its name from the slow food movement, the slow internet movement seeks to recreate the
    less commercial and more user-centric internet of the early 90s. We will explore how Racket
    facilitated the development of two slow internet applications and one DSL. First is gopher21,
    a gopher server with full text search. Second is the graphical, multi-tabbed gopher and gemini
    client Molasses. And finally, #lang magic is an implementation of the Unix file command's
    custom language for writing filetype determination queries. By the end of the talk we will
    have discussed fear of macros, html layout and rendering, custom canvas widgets and how Racket
    appears from the perspective of an experienced C programmer and low-level engineer.
   }
   #:bio @bio{
    Jonathan Simpson has worked as a professional software engineer since 2001, mostly in the Linux
    and embedded systems spaces. While almost all of his professional work is in C, he has long
    harbored a love for Lisp which eventually led him to Racket.
   }
  ]
  @lecture[
   #:when @talk-time{Saturday, 4:30pm}
   #:who @speaker[#:url "https://github.com/capfredf"]{Fred Fu}
   #:what @talk{TBD}
   #:more @abstract{}
   ;#:bio @bio{}
  ]
  )

 (section
  @sectionHeader{Sunday, October 5th}
  @doors-open[@talk-time{Sunday, 9:00am}]
  @lecture[
   #:when @talk-time{Sunday, 9:30am}
   #:who @speaker[#:url "https://github.com/texdraft"]{Asher Olsen}
   #:what @talk{PROG Rock: Listening to old Lisp code}
   #:more @abstract{
    Music and Lisp code might seem worlds apart, but through various processes it is possible to get
    aurally pleasing results from a LISP 1.5 → music compiler (written in Racket, of course). In
    this talk, I explain how this “musicalization” works and show ways that composers can make use
    of the concept.
   }
   #:bio @bio{
    Asher Olsen is a composer and programming language enthusiast living in Texas.
   }
  ]
  @lecture[
   #:when @talk-time{Sunday, 10:00am}
   #:who @speaker[#:url "https://users.cs.utah.edu/~mflatt/"]{Matthew Flatt}
   #:what @talk{Rhombus}
   #:more @abstract{}
  ]
  @break[@talk-time{Sunday, 10:30am}]
  @lecture[
   #:when @talk-time{Sunday, 11:00am}
   #:who @speaker[#:url "https://samth.github.io"]{Sam Tobin-Hochstadt}
   #:what @talk{The State of Racket}
  ]
  @lecture[
   #:when @talk-time{Sunday, 11:30am}
   #:who @speaker[#:person? #f]{Racket Management}
   #:what @talk{Racket Town Hall}
   #:more @abstract{
    Please come with your big questions and discussion topics.
   }
  ]
  )

 (section
  @sectionHeader{Registration}
   @paragraph{Complete this form to be notified when registration opens, @a[#:href "https://forms.gle/omJAjAQ6xJLjsw3z8"]{Register your interest in RacketCon}. (Yes, we are planning to livestream for those who cannot attend in person, but please register so we can keep you informed.)}
 )

 (section
   @sectionHeader{Accommodation}
   @paragraph{At this stage, we have not made accomodation arrangements. If you would like to be notified please use the @a[#:href "https://forms.gle/omJAjAQ6xJLjsw3z8"]{Register your interest in RacketCon} form.}
  )

 (section
   @sectionHeader{Friendly Policy}
   @paragraph{The proceedings of RacketCon will take place under the Racket @(a #:href "https://racket-lang.org/friendly.html" "Friendly Environment Policy").}
  )

 (section
   @sectionHeader{Organization}
   @paragraph{
    The RacketCon 2025 is organised by a team of volunteers: Stephen De Gabrielle, Robby Findler,
    Jacqueline Firth, Matthew Flatt, Ben Greenman, Siddhartha Kasivajhula, Bogdan Popa, Sam
    Tobin-Hochstadt with local arrangements by Stephen Chang. The organizers may be reached at
    @|mailto:con-organizers|.}
  )

 (section
  @sectionHeader{Previous RacketCons}
  @(apply larger
               (cdr
                (apply
                 append
                 (for/list ([year '(2024 2023 2022 2021 2020 2019 2018 2017 2016 2015 2014 2013 2012 2011)])
                   (list " ∙ "
                         (a #:href (format "https://con.racket-lang.org/~a/" year)
                            (format "~a" year)))))))))))))

;; ------------------------------------------------------------

(provide make)
(define (make p)
  (with-output-to-file
    (build-path p "index.html")
    #:exists 'replace
    (λ ()
      (displayln "<!doctype html>")
      (write-xexpr page))))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path here ".")
  (make here))
