#lang at-exp racket/base

(require racket/match
         racket/string
         xml
         txexpr
         (prefix-in gregor: gregor)
         "lib.rkt")

(define-div main
  [font-family "'Montserrat', sans-serif"]
  [background "forestgreen"]
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
  [width "45em"])

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
  [background "#CCEEDD"]
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
                 #:desc [desc "Keynote"])
  (lecture #:when when #:who @speaker[#:person? #f]{@activity{@desc}}
           #:what (keynote-speaker who) #:link link #:more what
           #:even-more more))

(define (bio . contents)
 (apply bio-div @bio-label{Bio: } contents))

(define (q content)
  `(q () ,content))

(define (at-where name addr)
  `(div ()
        (div ,name)
        (div ,addr)))

(define saturday (gregor:date 2024 10 5))
(define sunday (gregor:date 2024 10 6))
(define location "Seattle, WA, USA")

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
 (define tz (with-timezone (on-date t d) "America/Los_Angeles"))
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
    @title{(fourteenth RacketCon)}
    @script{
$(document).ready(function () {
 $("[data-slot-time]").each(function() {
  var date = new Date($(this).data("slot-time"));
  var localTime = moment.tz(date, "America/Los_Angeles").format("dddd, h:mma zz")
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
        @pagetitle["(fourteenth" (br) 'nbsp "RacketCon)" 'nbsp 'nbsp 'nbsp]))
      @subtitle{October 5-6, 2024}
      @subtitle{@p-location{University of Washington}}
      @specific-location{Kane Hall Room 220 @'nbsp @'nbsp 4069 Spokane Way}
      @specific-location{@location  (@a[#:href "https://www.openstreetmap.org/way/26447597" #:title "Kane Hall (OpenStreetMap)"]{map})}
      )

(txexpr* 'time `((class "dt-start") (hidden "") (datetime ,(gregor:~t saturday "y-MM-dd"))))
(txexpr* 'time `((class "dt-end") (hidden "") (datetime ,(gregor:~t sunday "y-MM-dd"))))

(column

 @featuring{Celebrating 40 years of magic with Hal Abelson & Gerald Sussman,@(br)
            and featuring Lisp legend Gregor Kiczales}
  
 #;
 (section
  @sectionHeader{Call for Presentations}

  @para{We are looking for @emph{you!} If you have an idea for a presentation you’d like to give, please write to @(a #:href "mailto:con-organizers@racket-lang.org" #:title "Send mail to the RacketCon organizer" "the RacketCon organizers") for consideration. All Racket-y ideas are welcome. We’d love to have you!}

  )

 (section
  @sectionHeader{Saturday, October 5th}

  @doors-open[@talk-time{Saturday, 8:30am}]

  @nb-breakfast

  @keynote[
@talk-time{Saturday, 9:00am}
#:desc "Invited Talk"
#:who
@speaker[#:url "https://www.cs.ubc.ca/~gregor/"]{Gregor Kiczales}
#:what
@talk{Strategies and Technology for Teaching HtDP at Scale}
#:link "https://youtu.be/Uc4koFmFDdQ"
#:more
@abstract{
}
]

  @break[@talk-time{Saturday, 9:45am}]

  @keynote[
@talk-time{Saturday, 10:15am}
#:desc "Keynote (Remote Presentation)"
#:who
@joint{@speaker[#:url "https://www.csail.mit.edu/person/hal-abelson"]{Hal Abelson}
       and @speaker[#:url "https://www.csail.mit.edu/person/gerald-sussman"]{Gerald Sussman}}
#:what
@talk{}
#:link "https://youtu.be/_2qXIDO-cWw"
#:more
@abstract{
}
]

  @lecture[
#:when
@talk-time{Saturday, 11:15am}
#:who
@speaker[#:url "https://github.com/michaelballantyne"]{Michael Ballantyne}
#:what
@talk{A Multi-Language-Oriented Macro System}
#:link "https://youtu.be/KHxZ3mT9BSo"
#:more
@abstract{
Racket’s macros are fantastic for building DSLs that mix well with general-purpose code and inherit Racket’s extensibility. But they suffer when it comes time to equip your DSL with an optimizing compiler or static analyses. There, the very closeness with Racket that makes language mixing and extension so easy gets in the way. In this talk I’ll present my `syntax-spec` metalanguage. It extends Racket’s macro system to allow specifications of DSLs as fragments of a multi-language: DSL and Racket code mix, but only at carefully delineated boundaries. The result is that optimizing compilation, language mixing, and extension combine constructively rather than clash.
}
]

 @lunch[@talk-time{Saturday, 11:45am}]

  @lecture[
#:when
@talk-time{Saturday, 1:30pm}
#:who
@speaker[#:url "https://github.com/benknoble"]{Ben Knoble}
#:what
@talk{Frosthaven Manager: Built by the Community}
#:link "https://youtu.be/O33NK52ZmUk"
#:more
@abstract{
Consisting of more than 15k lines of code and documentation, written in spare time during the last two and a half years, the @(a #:href "https://github.com/benknoble/frosthaven-manager" #:title "frosthaven-manager (GitHub)" "Frosthaven Manager") would not exist without the Racket community. Let’s explore some of the community’s contributions, reflect on the benefits of building in the open, and dream about what comes next.
}
]

  @lecture[
#:when
@talk-time{Saturday, 2:00pm}
#:who
@speaker[#:url "https://www.linkedin.com/in/charlie-ray-5a11b81a7/"]{Charlie Ray}
#:what
@talk{Mutation Testing: better than coverage?}
#:link "https://youtu.be/Ed6Ocs3U1v4"
#:more
@abstract{
Mutation testing is the idea that we can assess the completeness of a test suite by updating (or ‘mutating’) a single location in the code under test, and checking to make sure at least one of the existing tests fails. Building on Lukas Lazarek’s mutation framework for Racket, we apply mutation testing in two very different settings—the classroom, and the open-source software project—to see what kinds of benefit mutation testing might offer above and beyond the commonly-used test case coverage metric.}
]

    @lecture[
#:when
@talk-time{Saturday, 2:30pm}
#:who
@speaker[#:url "https://github.com/ndh4"]{Nathaniel Hejduk}
#:what
@talk{Trouble with Typed Racket? Try contract-profile!}
#:link "https://youtu.be/FeDVjomZgmE"
#:more
@abstract{
When you add types to a portion of your partially-untyped code base in Typed Racket, the type soundness guarantees you gain will make you feel warm and fuzzy. Sometimes, however, doing so will cause your running time to skyrocket, turning your cute, fluffy type annotations into an unexpected tribulation. When such troubles occur, contract-profile can help you ease the runtime wrath of contract checking. In this talk, I’ll demonstrate how to use contract-profile to boldly reduce the overhead of type-checking in your program, without violating the following prime directive: once a module has been typed, it must stay typed forever.
}
]

   @break[@talk-time{Saturday, 3:00pm}]


  @lecture[
#:when
@talk-time{Saturday, 3:30pm}
#:who
@speaker[#:url "https://lambdaland.org"]{Ashton Wiersdorf}
#:what
@talk{Type Tailoring: Teach an Old Type Checker New Tricks}
#:link "https://youtu.be/rmPCstrSQOA"
#:more
@abstract{

@paragraph{Type checkers evolve too slowly to keep up with the libraries and
DSLs programmers create. For example, compilers typically see only
opaque strings where programmers see regular expressions or SQL
queries with rich internal structure. Consequently, type checkers
will assign overly-general types where the programmer knows more
about the data at hand.}

@paragraph{This talk will present two implementations of a type tailoring:
the first in Racket, and the second in Rhombus. The Racket
tailoring strengthens Typed Racket by making regular expressions
legible to the type system, and the Rhombus tailoring relaxes
Static Rhombus in the spirit of gradual typing—all using the macro
system.}

}

#:bio
@bio{Ashton Wiersdorf is a PhD student beginning his third year at
the University of Utah. He works with his advisor Ben Greenman on
the intersection between types and metaprogramming. When he’s not
working, he enjoys taking hikes with his wife and three-year-old
daughter who already knows to answer “Racket” when asked what her
favorite programming language is.}

]

@;{ @lecture[
#:when
@talk-time{Saturday, 4:00pm}
#:who
@speaker[#:url "https://passingti.me"]{Sean Bocirnea}
#:what
@talk{Making Extensible Language Frameworks That Compile Quickly Too}
#:more
@abstract{

Racket has many frameworks for implementing extensible
languages. Unfortunately, while very extensible, they can exhibit
compile-time performance problems. For example, Cur, a small dependently
typed language written in the Turnstile+ framework, is 5x slower to type
check a simple proof than a comparable language type-checked by
traditional means. In this talk, we’ll figure out why, develop a
criteria for what @q{extensible} even means, explore the tradeoffs between
extensibility and performance in existing frameworks, and determine how
we can make extensible language frameworks faster without losing
extensibility.

}

]}

  @lecture[
#:when
@talk-time{Saturday, 4:00pm}
#:who
@speaker[#:url "https://williamjbowman.com"]{William Bowman}
#:what
@talk{First-class Prompt Engineering with llm-lang! (This is a bad idea.)}
#:link "https://youtu.be/ueGC3xVcDlc"
#:more
@abstract{
@paragraph{As we all know, generative AI and LLMs have replaced software engineering entirely, so all existing programming languages are deprecated. (What are you talking about?) Unfortunately, because even PL people don’t understand the importance of language-oriented programming, interfaces to LLMs remain restricted to chatbots, IDE plugins, and crappy APIs for deprecated languages! This is not the world we were promised; I want first-class prompt engineering! (No, I really don’t!)}

@paragraph{Introducing, @(a #:href "https://github.com/wilbowma/llm-lang" #:title "llm-lang (GitHub)" "llm-lang"), a Racket hash-lang in which prompt engineering in first class! (Why did I do this?) By default, youre writing a prompt! If you need, you can escape into Racket to compute part of the prompt, or compute over the result of the prompt, but you probably won’t since LLMs can do it all! (You’re joking right?) We perform some cursory experiments to show the power of llm-lang, and some limitations. While it doesn’t @emph{strictly} provide any measurable benefit, and might in fact be hugely problematic if deployed for anything real, it is @emph{cool}. (Please, somebody stop me.) I’m sure sufficiently advanced magic will solve these problems in the future. (😫)}}
]

  @social[
#:when
@talk-time{Saturday, 6:00pm}
#:where
@at-where[@place{@(a #:href "https://www.elysianbrewing.com/locations/capitol-hill" "Elysian Capitol Hill")}
          @place-address{1221 E Pike Street}]
#:more
@abstract{
Gathering with drinks and snacks, located close to a light rail station that is one stop away from the university.
}
]

)

 (section
  @sectionHeader{Sunday, October 6th}

  @doors-open[@talk-time{Sunday, 9:00am}]

  @nb-breakfast

  @lecture[
#:when
@talk-time{Sunday, 9:30am}
#:who
@speaker[#:url "https://github.com/jackfirth"]{Jacqueline Firth}
#:what
@talk{Sorted Collections in Rebellion}
#:link " https://youtu.be/-eLw9vpcKI8"
#:more
@abstract{
 Computation exists in service of data. Useful data
 structures, therefore, make the difference between what is possible
 and what is easy. And of the many forms of structured data, sorted
 data is arguably the most useful. In this talk, we'll tour the sorted
 data structures in Rebellion, a second standard library for Racket
 I've developed over the years. We'll collect data into sorted sets,
 sorted maps, and range sets. We'll reduce and transduce data from one
 collection into another. We'll order data according to composable
 comparators. We'll build data up gradually, then all at once. We'll
 query it, mutate it, persistently update it, and concurrently access
 it. And we'll do it all with enough performance to wrap up by lunch.
}
]

  @lecture[
#:when
@talk-time{Sunday, 10:00am}
#:who
@speaker[#:url "https://linkedin.com/in/allanschougaard"]{Allan Schougaard}
#:what
@talk{Racket Syntax: The Great, the Good and the Back-to-the-Drawing-Board}
#:link "https://youtu.be/ZtTqRH1uwu4"
#:more
@abstract{
In this talk I present a linguistic comparison of language choices in
Racket and LISP vs. a number of other computer languages. LISP is by far
the programming language with the simplest syntax: only using parenthesis
as delineations. However, the Racket and LISP communities have over time
added a great variety of semantics on this simple mechanism, some of which
have proven great inventions, and some of which other languages may have
better solutions to, and that the Racket and LISP communities may learn
from. The talk will directly compare elements of yaml, SmallTalk, Ruby,
Java, regular expressions, visual programming, and shell programming with
Racket.
}
]

  @break[@talk-time{Sunday, 10:30am}]

  @lecture[
#:when
@talk-time{Sunday, 11:00am}
#:who
@speaker[#:url "https://samth.github.io" #;#:affiliation #;"Indiana"]{Sam Tobin-Hochstadt}
#:what
@talk{The State of Racket}
#:link "https://youtu.be/PYeKEshvAh8"
]

  @lecture[
#:when
@talk-time{Sunday, 11:30am}
#:who
@speaker[#:person? #f]{Racket Management}
#:what
@talk{Racket Town Hall}
#:link "https://youtu.be/lhkXdE0nPOU"
#:more
@abstract{

Please come with your big questions and discussion topics.

}]

)

 (section
  @sectionHeader{Registration}
   @paragraph{To register, @a[#:href "https://www.eventbrite.com/e/racketcon-2024-tickets-983892828937"]{buy a ticket via Eventbrite}. If you cannot attend in-person, there is an option on Eventbrite for @emph{remote participation} to support the livestream.}
 )

 (section
   @sectionHeader{Accommodation}
   @paragraph{No official hotel has been selected, and no block(s) of rooms have been reserved. However, the University of Washington has @(a #:href "https://www.cs.washington.edu/visitors/getting_here#hotels" #:title "Accomodations near the Paul G. Allen School of Computer Science & Engineering" "a useful list") of nearby hotel recommendations on its Getting Here page. @(bold "NB") On that list of recommended hotels, Hotel Deca has been renamed to @(a #:href "https://graduatehotels.com/seattle/" #:title "Graduate Hotel" "Graduate Hotel").}
    @paragraph{There will be a @(a #:href "https://www.espn.com/college-football/game/_/gameId/401628505/michigan-washington" #:title "Washington vs. Michigan" "college football game") on October 5th, so there might not be parking space for those bringing their own car. It should also be fine if you’re staying somewhere in the downtown area somewhat away from the university, though public transportation will likely be crowded because of the game.}
  )

 (section
   @sectionHeader{Friendly Policy}
   @paragraph{The proceedings of RacketCon will take place under the Racket @(a #:href "https://racket-lang.org/friendly.html" "Friendly Environment Policy").}
  )

 (section
   @sectionHeader{Organization}
   @paragraph{The RacketCon 2024 organizers are Jesse Alama, Matthew Flatt, Robby Findler, Siddhartha Kasivajhula, and Stephen De Gabrielle
              with local arrangements by Zach Tatlock and Sorawee Porncharoenwase.
              The organizers may be reached at @(a #:href "mailto:con-organizers@racket-lang.org" "con-organizers@racket-lang.org").}
  )

 (section
  @sectionHeader{Previous RacketCons}
  @(apply larger
               (cdr
                (apply
                 append
                 (for/list ([year '(2023 2022 2021 2020 2019 2018 2017 2016 2015 2014 2013 2012 2011)])
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
