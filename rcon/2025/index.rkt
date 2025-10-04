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

(define-div top-section
  [margin-top "1em"])

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

(define-div fromplace
  [font-size "16pt"]
  [font-weight "bold"]
  [text-align "left"]
  [margin-top "2ex"]
  ,@header-font)

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

(define-div vpara
  [text-align "left"]
  [margin-top "2ex"])

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
      @specific-location{@a[#:href "umb.html"]{University Hall}}
      )

(txexpr* 'time `((class "dt-start") (hidden "") (datetime ,(gregor:~t saturday "y-MM-dd"))))
(txexpr* 'time `((class "dt-end") (hidden "") (datetime ,(gregor:~t sunday "y-MM-dd"))))

(column

 ;; @featuring{Special Guests TBC}

 (top-section
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
  @sectionHeader{Live Stream}
  @para{@emph{Stay tuned}: the live stream will appear here.}
  #;
  '(iframe ([width "720"]
           [height "800"]
           [src "https://boxcast.tv/view-embed/xtihxdvdmgttkttsp2gj?showTitle=0&showDescription=0&showHighlights=0&showRelated=0&defaultVideo=next&playInline=0&dvr=1&market=smb&showCountdown=0&showDonations=0&showDocuments=0&showIndex=0&showChat=0&hidePreBroadcastTextOverlay=0"]
           [frameBorder "0"]
           [scrolling "auto"]
           [allowfullscreen "true"]
           [allow "autoplay; fullscreen"]))
  #;
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
  @sectionHeader{Registration}
  @paragraph{@a[#:href "https://www.eventbrite.com/e/racketcon-2025-tickets-1578775272339"]{To register, buy a ticket via Eventbrite}.
             If you cannot attend in-person, there is an option to help support the livestream for remote participants.}
 )

 (section
   @sectionHeader{Local Infomation and Accommodation}
   @paragraph{@a[#:href "umb.html"]{See the local-information page for directions and hotel information}.}
  )


 (section
  @sectionHeader{Saturday, October 4th}
  @doors-open[@talk-time{Saturday, 8:30am}]
  @nb-yes-breakfast
  @keynote[
   @talk-time{Saturday, 9:00am}
   #:desc "Keynote"
   #:who @joint{
    @speaker[#:url "https://cloudflare.com"]{James Larisch}
    and
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
   #:more @abstract{
    We reconstruct a theory of object-orientation from first principles, as modularity and
    extensibility together. Mixin inheritance then appears as a natural embodiment of these joined
    principles expressed in the lambda-calculus. Further OO concepts such as prototypes, classes,
    single or multiple inheritance, multiple dispatch, method combinations and more naturally
    follow. Interestingly, many misconceptions about OO can also be dispelled, and we find that the
    simplest and most natural context for OO is pure lazy dynamic functional programming, without
    classes, and even without objects(!). A Scheme and/or Racket prototype (ha!) of these ideas will
    be presented.
   }
   #:bio @bio{
    Not fitting in French Academia due to his penchant for dynamic languages, Faré learned how (not)
    to build software in Corporate America (ITA, Google, Bridgewater), and eventually became his own
    startup entrepreneur in the domain of secure blockchain architecture. Trained in Programming
    Language Semantics and Distributed Systems, Faré completed but never defended a thesis on
    Reflective Systems. Once author of versions 2 and 3 of the build system ASDF at the heart of all
    Common Lisp free software, he is now co-maintainer of Gerbil Scheme. Unsettled by online debates
    between OO vs FP back when he was a student at ENS.fr, he finally discovered twenty years later
    the essence of OO thanks to Jsonnet and Nix, and, trying to share his insight and digging into
    old bibliography, became despite himself an expert on Object-Orientation.
   }
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
  @nb-yes-lunch
  @lecture[
   #:when @talk-time{Saturday, 1:30pm}
   #:who @speaker[#:url "https://github.com/toddjonker"]{Todd Jonker}
   #:what @talk{Ion Fusion}
   #:more @abstract{
    Ion Fusion is a customizable programming language that unifies the semantics of persistent data
    and the code that manipulates it. Oriented around the Amazon Ion data format--the backbone of
    Amazon’s retail systems and even consumer products--Fusion has been the brains of internal
    analytics, data processing, and workflow systems since 2013. This talk explores Ion Fusion’s
    unique design goals and constraints, its roots in Scheme and Racket, and its vision of
    sustainable software evolution.
   }
   #:bio @bio{
    I got bored with AppleSoft Basic in 1982 and have been designing PLs ever since. A self-taught
    coder, I was reeducated by PLT at Rice, then failed out of CMU into an industry career. After
    surfing through startups in a variety of industries, I settled down for two decades at Amazon,
    building foundation tech and evolving very large systems, including the company’s central
    package builder. Throughout, my mission is to improve developer happiness through transparent,
    coherent, and sustainable languages, frameworks, and tools.
   }
  ]
  @lecture[
   #:when @talk-time{Saturday, 2:00pm}
   #:who @speaker[#:url "https://www.greghendershott.com/"]{Greg Hendershott}
   #:what @talk{"It Works": More Adventures with Racket and Emacs}
   #:more @abstract{
    The Emacs package "racket-mode" has continued to evolve its design and features. Among other
    things, it allows multiple local and remote back ends, has a redesigned "lossless" REPL,
    supports modern Emacs UI completion annotations, and enables lang-driven editing. Most recently
    the step debugger has gotten some attention.
   }
   #:bio @bio{
    Greg Hendershott has a useless degree in philosophy, a background in the music software
    industry, and a decade working in the open source Racket and Emacs communities. Tedious details
    are available at @(a #:href "https://www.greghendershott.com/About.html" "greghendershott.com").
   }
  ]
  @lecture[
   #:when @talk-time{Saturday, 2:30pm}
   #:who @speaker[#:url "https://camoy.net/"]{Cameron Moy}
   #:what @talk{Roulette for Racketeers}
   #:more @abstract{
    Exact probabilistic inference is a requirement for many applications of probabilistic
    programming languages (PPLs), but implementing a PPL with high-performance inference is
    difficult. Roulette is a new discrete PPL that combines high-performance exact inference with
    expressive language features by leveraging the close connection between exact probabilistic
    inference and the symbolic evaluation strategy of Rosette. Building on this connection,
    Roulette generalizes and extends the Rosette solver-aided programming system to reason about
    probabilistic rather than symbolic quantities. In this talk, I'll demonstrate how to use
    Roulette and discuss some of the ideas that make it work.
   }
   #:bio @bio{
    Cameron is a PhD student and member of the PLT and PRL research groups at Northeastern
    University. He primarily studies contract systems but is broadly interested in programming
    language design.
   }
  ]
  @break[@talk-time{Saturday, 3:00pm}]
  @lecture[
   #:when @talk-time{Saturday, 3:30pm}
   #:who @joint{
    @speaker[#:url "https://github.com/ariscript"]{Ari Prakash}
    and
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

    @social[
#:when
@talk-time{Saturday, 6:00pm}
#:where
@at-where[@place{@a[#:href "https://lordhobo.com/boston/"]{Lord Hobo Brewery}}
          @place-address{2 Drydock Ave}]
#:more
@abstract{
Gathering with drinks and snacks.
}
]
)

 (section
  @sectionHeader{Sunday, October 5th}
  @doors-open[@talk-time{Sunday, 8:30am}]
  @nb-no-breakfast
  @lecture[
   #:when @talk-time{Sunday, 9:00am}
   #:who @speaker[#:url "https://github.com/michaelballantyne"]{Michael Ballantyne}
   #:what @talk{Advanced Macrology: 5 Macro Programming Patterns You (No Longer) Need to Know}
   #:more @abstract{
    Racket's macros make creating small extensions to the language remarkably simple. But when
    you step up to creating DSLs with static semantics and compiler optimizations, you need new
    tools to integrate your DSL implementation with Racket's expansion process. In this talk I'll
    demonstrate 5 macro design patterns that sophisticated DSLs like match, syntax-parse, and Typed
    Racket use to validate syntax, communicate static information, and integrate with DrRacket. But
    as any good language-oriented programmer knows, a design pattern usually indicates a missing
    linguistic abstraction! And in fact, my syntax-spec metalanguage abstracts over all of these
    design patterns, generating implementations from a declarative specification of your DSL
    syntax. So I'll also show how you can more easily achieve the same results with syntax-spec,
    design-pattern-free.
   }
   #:bio @bio{
    Michael Ballantyne recently completed his Ph.D. at Northeastern University, advised by Matthias
    Felleisen. His research works towards a future where mainstream programmers regularly create,
    extend, and fluidly intermix domain-specific languages, with excellent support from their
    programming language and environment.
   }
  ]
  @lecture[
   #:when @talk-time{Sunday, 9:30am}
   #:who @speaker[#:url "https://github.com/jagen31"]{Jared Gentner}
   #:what @talk{Great Composers Steal: Obbligato Reuse of Racket in Tonart}
   #:more @abstract{
    Imagine, for a moment, that a music score could somehow become a Racket module. Imagine if every
    notation on that music score was a syntax object. Imagine selecting areas of the score and
    running macro-like rewrites on the notations within that selection. Imagine a context system
    which allows a composer to summon important static information about any notation, such as the
    key, the tuning system, or the instrument. This is the vision of Tonart, an extensible language
    and library for composing music in Racket. This talk will highlight the ideas Tonart steals
    from Racket in its design, as well as the language features Tonart reuses from Racket in its
    implementation.
   }
   #:bio @bio{
    Jared has been composing music and functions since a young age. A native of Upstate New York,
    he moved to Boston to study computing at Northeastern. He had no clue about Racket when he
    enrolled, but where fate leads, we are bound to follow! What he enjoys most is making music with
    his friends. Jared believes in living life to the fullest, that “Life imitates Art”, and that
    the greatest art is to be found at home.
   }
  ]
  @lecture[
   #:when @talk-time{Sunday, 10:00am}
   #:who @speaker[#:url "https://users.cs.utah.edu/~mflatt/"]{Matthew Flatt}
   #:what @talk{Rhombus Update}
   #:more @abstract{
    @a[#:href "https://rhombus-lang.org/"]{Rhombus} is ready for early adopters. There's still
    plenty to be done in creating Rhombus-style bindings for Racket libraries, building entirely
    new Rhombus libraries when that makes sense, and refining the Rhombus tooling and ecosystem.
    No language is ever finished, but Rhombus is well past the prototype stage and increasingly a
    language that you can use for everyday tasks. The talk will present a status report and some
    guesses about what will happen next.
   }
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
   @sectionHeader{Friendly Policy}
   @paragraph{The proceedings of RacketCon will take place under the Racket @(a #:href "https://racket-lang.org/friendly.html" "Friendly Environment Policy").}
  )

 (section
   @sectionHeader{Organization}
   @paragraph{
              The RacketCon 2025 is organised by a team of volunteers: Bogdan Popa organzied the program,
              Stephen Chang provided local arrangements, and
              Stephen De Gabrielle, Robby Findler, Jacqueline Firth, Matthew Flatt, Ben Greenman,
              Siddhartha Kasivajhula, Sam Tobin-Hochstadt helped. The organizers may be reached at
              @|mailto:con-organizers|.}
  )

 (section
  @sectionHeader{Sponsor: UMass Boston}
  @center{
 @a[#:href "https://www.umb.edu/"]{
  @img[#:src "umb.svg"
       #:alt "University of Massachusetts Boston Logo"]
 }
 }
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

 (define hotel-link
  "https://www.hilton.com/en/hotels/boschdt-doubletree-boston-bayside/")
(define hotel-booking-link
  "https://www.hilton.com/en/book/reservation/deeplink/?ctyhocn=BOSCHDT&corporateCode=0002702479")

(define shuttle-bus @a[#:href "https://www.umb.edu/the_university/getting_here/shuttle_bus_information"]{UMass Boston shuttle bus})

(define umb-page
  (html #:lang "en"
   (head
    (head-meta #:http-equiv "content-type" #:content "text/html; charset=utf-8")
    (link #:href fonts-url
          #:rel "stylesheet")
    (style (cdata #f #f (classes->string)))
    (style (cdata #f #f "a { text-decoration: none; } li { text-align: left; } "))
    @title{(fifteenth RacketCon) Local Information})
   (body
     #:class "main h-event"
     (content
      (banner
       @subtitle{@a[#:href "index.html"]{RacketCon 2025} Local Information})

(column
       (section
        @sectionHeader{Location}

        (column
         @specific-location{UMass Boston}
         @specific-location{University Hall}
         @specific-location{1st floor}
         @specific-location{Y01-1300}))

      (section
       @sectionHeader{Hotel}
       (column
        @vpara{The closest hotel is the @a[#:href hotel-link]{DoubleTree by Hilton Boston Bayside}.}
        @vpara{@bold{@a[#:href hotel-booking-link]{Book at a discounted rate}}
                    (limited number of rooms are available).}
        @vpara{See below for advice on getting from Logan Airport to this hotel.}))

      (section
       @sectionHeader{Saturday Social}
       (column
        @vpara{The Saturday social event will take place 6:00pm-9:00pm at
                   @a[#:href "https://lordhobo.com/boston/"]{Lord Hobo Brewery}
                       in the Seaport neighborhood of Boston.}))

      (section
       @sectionHeader{Getting There}

       (column

        @vpara{@a[#:href "umb-navigation.pdf"]{Navigation Summary (PDF)}}

        @fromplace{From DoubleTree Boston Bayside}

        @vpara{Walk (15-20 minutes):}

        @ul{
            @li{Walk down Mt. Vernon St.}
            @li{Take the walkway through campus.}
            @li{Attive at to University Hall.}
            }

        @vpara{or take the @shuttle-bus (@a[#:href "https://umb.transloc.com/routes"]{track in real-time}):}

        @ul{
            @li{Walk to the Bayside stop on Mt. Vernon St. in front of hotel.}
            @li{Take the @shuttle-bus to Campus Center (5 min).}
            @li{Walk from Campus Center to University Hall (next door).}
            }


        @fromplace{From Logan Airport}

        @ul{
            @li{Take the Silver Line (bus) to South Station.}
            @li{Take the T Red Line (subway), to JFK/UMass station.}
            }

        @vpara{Then either}

        @ul{
            @li{Walk to DoubleTree hotel from JFK/UMass (5min.)}
            }

        @vpara{or}

        @ul{
            @li{Take the @shuttle-bus to Campus Center (5-10 min).}
            @li{Walk from Campus Center to University Hall (next door).}
            }

        @fromplace{From South Station}

        @ul{
            @li{Take the T Red Line (subway) to JFK/UMass station.}
            @li{Take the @shuttle-bus to Campus Center (5-10 min).}
            @li{walk from Campus Center to University Hall (next door).}
           }

       @fromplace{Parking}

       @vpara{The closest parking garage to University Hall (connected, next door) is
                  the Campus Center Garage.}

       @vpara{If that is full, parking is also available in the West Garage or Bayside Lot.}

       @vpara{See @a[#:href "https://www.umb.edu/the_university/getting_here/visitor_resources"]{UMass Boston Visitor Resources}
                  for rates and locations of all parking garages.}

       @fromplace{See Also}

       @vpara{@a[#:href "https://www.umb.edu/the_university/getting_here"]{UMass Boston “Getting Here”}}
       @vpara{@a[#:href "https://www.umb.edu/map"]{UMass Boston Campus Map}}

       ))

      (section
       @sectionHeader{Wi-Fi}
       (column
       @vpara{Attendees with university credentials should be able to access the
                            internet using eduroam.}

       @vpara{For other attendees, we have set up the @tt{Event_Guests} network.}

       @vpara{Alternately, the @tt{UMB-Guest} network may also be used (@a[#:href "https://www.umb.edu/campus_center/services/wireless_access"]{details here}).})))))))

;; ------------------------------------------------------------

(provide make)
(define (make p)
  (with-output-to-file
    (build-path p "index.html")
    #:exists 'replace
    (λ ()
      (displayln "<!doctype html>")
      (write-xexpr page)))
  (with-output-to-file
    (build-path p "umb.html")
    #:exists 'replace
    (λ ()
      (displayln "<!doctype html>")
      (write-xexpr umb-page))))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path here ".")
  (make here))
