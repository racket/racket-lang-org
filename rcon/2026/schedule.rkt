#lang at-exp racket/base

;; Schedule information 
;; (This page ultimately becomes a record of the conference proceedings including
;;  links to video presentations)

(require racket/match
         racket/string
         (prefix-in gregor: gregor)
         txexpr         
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

(define (social #:when when #:where [where ""] #:more [more ""] #:omit-label? [omit-label? #f])
  (lecture #:when when #:who @speaker[#:person? #f]{@(if omit-label? "" @activity{Evening Social})}
           #:what where
           #:more more))

(define (coffee when)
 (lecture #:when when #:who @speaker[#:person? #f]{@activity{Coffee}}))

(define (break when)
 (lecture #:when when #:who @speaker[#:person? #f]{@activity{Break}}))

(define (lunch when)
 (lecture #:when when #:who @speaker[#:person? #f]{@activity{Lunch}}))

(define (keynote #:when when #:who who #:what what #:more more #:link [link #f]
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

(define (meta #:itemprop [itemprop #f]
              content)
  (define elem (txexpr* 'meta (list (list 'content content))))
  (cond [(non-empty-string? itemprop)
         (attr-set elem 'itemprop itemprop)]
        [else elem]))

(define event-time-zone "America/Los_Angeles")

(define slot-number 0)
(define (talk-time dtime)
 (set! slot-number (add1 slot-number))
 (local-require racket/string gregor)
 (match-define (list day times) (string-split dtime ","))
 (define d (match day
             ["Saturday" saturday]
             ["Sunday"   sunday]))
 (define t (parse-time times " h:mmaa"))
 (define tz (with-timezone (on-date t d) event-time-zone))
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
    (style (cdata #f #f "a { text-decoration: none; } "))
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.3/moment.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment-timezone/0.5.34/moment-timezone-with-data-10-year-range.js"]) "")
    @title{(sixteenth RacketCon)}
    @script{
$(document).ready(function () {
 $("[data-slot-time]").each(function() {
  var date = new Date($(this).data("slot-time"));
  var localTime = moment.tz(date, @event-time-zone).format("dddd, h:mma zz")
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
                        #:src "rcon2026logo.png"
                        #:alt "The Racket logo")]
        @pagetitle["(sixteenth" (br) 'nbsp "RacketCon)" 'nbsp 'nbsp 'nbsp]))
      @subtitle{October 3-4, 2026}
      @specific-location{@p-location{Oakland, California, USA}}
      @specific-location{Oakstop, @a[#:href "localinfo.html"]{Broadway Gallery Suite}}
      )

(txexpr* 'time `((class "dt-start") (hidden "") (datetime ,(gregor:~t saturday "y-MM-dd"))))
(txexpr* 'time `((class "dt-end") (hidden "") (datetime ,(gregor:~t sunday "y-MM-dd"))))

(column

 (top-section
   @para{RacketCon is a public gathering dedicated to fostering a
   vibrant, innovative, and inclusive community around the Racket
   programming language. We aim to create an exciting and enjoyable
   conference open to anyone interested in Racket, filled with inspiring
   content, reaching and engaging both the Racket community and the
   wider programming world.})
  
 #;(section
  @sectionHeader{Live Stream Recording (Day Two)}

  @para{Videos will appear soon for all of the RacketCon talks. For now, a recording of the second day remains available}

  @a[#:href "https://boxcast.tv/view-embed/xtihxdvdmgttkttsp2gj?showTitle=0&showDescription=0&showHighlights=0&showRelated=0&defaultVideo=next&playInline=0&dvr=1&market=smb&showCountdown=0&showDonations=0&showDocuments=0&showIndex=0&showChat=0&hidePreBroadcastTextOverlay=0"]{Link}
  '(iframe ([width "720"]
           [height "480"]
           [src "https://boxcast.tv/view-embed/xtihxdvdmgttkttsp2gj?showTitle=0&showDescription=0&showHighlights=0&showRelated=0&defaultVideo=next&playInline=0&dvr=1&market=smb&showCountdown=0&showDonations=0&showDocuments=0&showIndex=0&showChat=0&hidePreBroadcastTextOverlay=0"]
           [frameBorder "0"]
           [scrolling "auto"]
           [allowfullscreen "true"]
           [allow "autoplay; fullscreen"]))
  
  '(iframe ([src "https://www6.cbox.ws/box/?boxid=846185&boxtag=7afys&tid=127&tkey=b25da2af9627c97d"]
           [width "100%"]
           [height "450"]
           [allowtransparency "yes"]
           [allow "autoplay"]
           [frameborder "0"]
           [marginheight "0"]
           [marginwidth "0"]
           [scrolling "auto"])))

 (section
  @sectionHeader{Registration is open!}
  
   @distinct{Please register for RacketCon 2026 by following
                 @bold{@larger{@(a #:href"https://www.eventbrite.com/e/racketcon-2026-tickets-1997181002140" "this link").}}}
 
  @para{There is still time to submit a proposal for a talk at RacketCon 2026 using
        @(a #:href "https://forms.gle/4YG57adx5snEwVe27" "this form") or
        write to @(a #:href "mailto:con-organizers@racket-lang.org" #:title "Send mail to the
        RacketCon organizer" "the RacketCon organizers") for consideration.

        For more information about presentation format, video streaming details, volunteering and
        sponsorships, please see our detailed call for participation
        @(a #:href "https://racket.discourse.group/t/racketcon-2026-call-for-participation/4211" "here").
        All Racket-y ideas are welcome. We’d love to have you!})  

 (top-section
   @para{@emph{As in previous years, RacketCon will be streamed for those unable to attend in person.
               Recordings will also be made available on YouTube some time after the conference.
               Streaming users will have the option to purchase a remote participation ticket to
               support the livestream. Previous RacketCon presentations can be found @(a #:href "https://www.youtube.com/racketlang/playlists" "here").}})

(section
 @sectionHeader{Keynote}
 @keynote[
          #:when ""
          #:desc "" ; drop when moving to the schedule
          #:who @speaker[#:url "https://profiles.stanford.edu/patrick-hanrahan"]{Pat Hanrahan}
          #:what @talk{On Notation}
          ;#:link "https://youtu.be/OpT2W45w9MQ?si=-twzTu6tFdyvIbmd"
          #:more ""
          #:bio @bio{
          Pat Hanrahan is the Canon Professor of Computer Science and Electrical Engineering Emeritus 
          at Stanford University. He led design of RenderMan at Pixar, co-founded Tableau, and recieved the
          2019 ACM Turing Award.
          }])

(section
 @sectionHeader{Talks}
  @lecture[
           #:when ""
           #:who @speaker[#:url "https://"]{Fred Fu}
           #:what @talk{Type Inference With Logical Types For Untyped Languages}
           ;#:link "https://youtu.be/OpT2W45w9MQ?si=-twzTu6tFdyvIbmd"
           #:more @abstract{
           Racket and Rhombus support flexible idioms that pose challenges for type systems. Typed
           Racket, the gradually typed counterpart of Racket, uses occurrence typing to type-check
           programs whose control flow depends on run-time type tests. To alleviate the burden of
           annotation, Typed Racket also supports local type inference. Problems arise, however, when
           a Typed Racket program imports a macro from a Racket module that expands to complex code
           containing lambda expressions. Programmers cannot add annotations to generated parameters.
           Moreover, local type inference is not designed to infer types for lambda parameters.
           Therefore, the type system usually conservatively rejects the code. As a result,
           programmers often have to rewrite macros in Typed Racket. My ongoing prototype type
           inference system addresses the problem by combining occurrence typing and algebraic
           subtyping. In this talk, I will demonstrate how the new type inference handles patterns
           commonly seen in Racket programs.        
           }
           #:bio @bio{
           I am Fred Fu, a PhD student at IU. I am a racketeer who have made myriad bugs in Typed
           Racket. I am a big fan of Rhombus as well.  
           }])
  @lecture[
           #:when ""
           #:who @speaker[#:url "https://"]{Lucas Myers}
           #:what @talk{Language-Oriented Low-Level Programming with Pille}
           ;#:link "https://youtu.be/OpT2W45w9MQ?si=-twzTu6tFdyvIbmd"
           #:more @abstract{
           Racket and Rhombus offer incredible expressive power, but the Racket VM can
           become a limiting factor for low-level performance and efficiency—and it
           necessitates a runtime system that precludes highly-constrained platforms
           like microcontrollers.

           Pille (pronounced like “peel”) is a new Rhombus-based language that aims to
           bring language-oriented programming to low-level and high-performance
           domains. In particular, Pille grafts Rhombus’s enforestation process onto a
           new core language with an LLVM-based compiler, bypassing the limitations of
           the Racket VM while retaining full Rhombus-based macros.

           This talk will provide an introduction to Pille, with a particular emphasis
           on how its high-level metaprogramming—not all of which derives from
           Rhombus—can address decidedely low-level problems.
           }
           #:bio @bio{
           Lucas Myers is a PhD student at Northwestern University (advised by Robby Findler), 
           where his research focuses on adapting the ideas and technologies of extensible
           programming languages—especially Racket and Rhombus—to the domain of systems
           programming. Prior to starting his PhD, Lucas worked in the software industry on
           an eclectic mix of projects that spanned the hardware/software stack. He is broadly
           interested in finding PL solutions to systems problems, and see extensible languages
           as holding immense (and largely unrealized) potential in that pursuit.
           }
         ]

  @lecture[
           #:when ""
           #:who @speaker[#:url "https://"]{Mike Delmonaco}
           #:what @talk{Treason: Making Macros and IDE Services Work Together}
           ;#:link "https://youtu.be/OpT2W45w9MQ?si=-twzTu6tFdyvIbmd"
           #:more @abstract{
           Racket's macros let us extend the language and create DSLs, but they also 
           get in the way of providing good IDE services when the program is broken or incomplete. 
           Treason is a prototype of a macro-extensible language that provides good IDE services 
           even when such errors prevent complete macro expansion. Treason's macro expander recovers 
           from errors to continue expanding and collecting information used to drive IDE services. 
           Our key contribution is spec-driven subexpression expansion: syntax-class annotations 
           allow us to expand subexpressions even within a broken macro use.
           }
           #:bio @bio{
           Mike Delmonaco is a Software Engineer at Amazon Web Services with a hobby interest in 
           Programming Languages and Racket. Outside of work, he enjoys rock climbing, video games, 
           creating interactive math visualizations, writing music, programming language research, 
           and teaching.
          }
         ]

    @lecture[
           #:when ""
           #:who @speaker[#:url "https://"]{JJ}
           #:what @talk{Effect Handlers in @em{cio}}
           ;#:link "https://youtu.be/OpT2W45w9MQ?si=-twzTu6tFdyvIbmd"
           #:more @abstract{
             Effect handlers — What are they? How do they work? We will
             discuss the design of @em{cio}, an (untyped) effect-handler library for Racket
             and Guile Scheme, and delve into the user interface considerations and
             implementation strategies of effect handlers broadly. Effect handlers,
             similar to delimited continuations, offer the expression of general
             @em{non-local control flow} through a minimal set of primitives, yet more
             closely resemble standard imperative constructions — but preserve pure
             reasoning principles by separating the @em{invocation} of a particular effect
             from its @em{implementation}. We discuss this, and further discuss some of the
             challenges and limitations of a library-level implementation of effect
             handlers.
           }
           #:bio @bio{
             JJ is a linguist by trade, living and working in Vancouver,
             British Columbia, by the Salish Sea. They are also a hobbyist programmer
             and a programming languages nerd: interested in language in its capability
             as an @em{interface} for expressing @em{intent} — between humans and other
             humans, and between humans and computers. They are a big fan of Rust, Lean
             4, and (of course) Racket. 
          }
         ]

    @lecture[
           #:when ""
           #:who @speaker[#:url "https://"]{Matthew Flatt}
           #:what @talk{A New Foreign-Function Interface: ffi2}
           ;#:link "https://youtu.be/OpT2W45w9MQ?si=-twzTu6tFdyvIbmd"
           #:more @abstract{
             Eli Barzilay's FFI (ca. 2004) made C-based dynamic libraries accessible
             within Racket programs without new C code. This approach had precedents
             in Lisp and Scheme systems, including Chez Scheme, but Barzilay's
             dynamic esthetic and pioneering use of Racket macros resulted in an
             especially convenient and composable interface that was a good fit to
             the underlying runtime system. Meanwhile, Andy Keep's ftypes layer (ca.
             2011) for Chez Scheme's FFI similarly takes advantage of macros for
             expressiveness, and it also cleverly exploits Chez Scheme's
             data-representation regime. Keep's more static system offers good
             performance, compact representations of foreign pointers, and low-cost
             checking of pointer operations. The new `ffi2` layer in Racket brings
             these two lines of development together. It relies on small adjustments
             to Chez Scheme that reflect lessons learned from `unsafe/ffi`, and it
             introduces a static approach at the Racket level to better target Chez
             Scheme. As a result, the `ffi2` library offers the convenience and
             composability combined with good performance and low-cost foreign
             pointers.
           }
           #:bio @bio{
             Matthew is a developer of Racket who focuses mostly on macros,
             compilation, and the runtime system.
          }
         ]

 ;; This section can be replaced later with an entry in the schedule
 (section
   @sectionHeader{Evening Social Event}
   (social #:when @talk-time{Saturday, 6:00pm}
           #:omit-label? #t ;; change to `#f` as a schedule entry
           #:where @at-where[@place{@a[#:href "https://drinkdrakes.com/visit/dealership/"]{Drake's Dealership}}
                             @place-address{2325 Broadway}]
           #:more @abstract{Gathering with drinks and snacks.})
  )

 (section
   @sectionHeader{Local Information}
   @paragraph{@a[#:href "localinfo.html"]{See the local-information page}.}
  )

 (section
   @sectionHeader{Friendly Policy}
   @paragraph{The proceedings of RacketCon will take place under the Racket @(a #:href "https://racket-lang.org/friendly.html" "Friendly Environment Policy").}
  )

 (section
   @sectionHeader{Organization}
   @paragraph{
              The RacketCon 2026 is organized by a team of volunteers.
              The organizers may be reached at @|mailto:con-organizers|.}

  )


 (section
  @sectionHeader{Previous RacketCons}
  @(apply larger
               (cdr
                (apply
                 append
                 (for/list ([year '(2025 2024 2023 2022 2021 2020 2019 2018 2017 2016 2015 2014 2013 2012 2011)])
                   (list " ∙ "
                         (a #:href (format "https://con.racket-lang.org/~a/" year)
                            (format "~a" year)))))))))))))

;; ------------------------------------------------------------
