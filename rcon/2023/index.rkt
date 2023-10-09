#lang at-exp racket/base

(require racket/match
         racket/string
         xml
         txexpr
         (prefix-in gregor: gregor)
         "lib.rkt")

(define monospace
  "'Cutive Mono', monospace")

(define-div column
  ,@centered
  [width "45em"])

(define-div banner
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
  [font-size "48pt"]
  [font-family ,monospace])

(define-div main
  [font-family "'Montserrat', sans-serif"]
  [background "white"]
  [color "black"])

(define header-font
  `(#;[font-weight "bold"]))

(define-div subtitle
  ,@centered
  [font-size "40pt"]
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
  [background "lightgray"]
  ,@header-font)

(define-div speaker-a
  [color "firebrick"])
(define-a unaffiliated
  [color "inherit"])
(define-a h-card
  [color "inherit"])

(define-div talk
  [font-weight bold]
  #;
  [font-style "italic"]
  [font-size "24pt"]
  [margin-top "0.25em"]
  [margin-bottom "1em"]
  [color "gray"])

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

(define-div larger
  [font-size "24pt"])

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

(define-div talk-time-div
  [font-weight bold]
  [position absolute]
  [color "gray"])

(define-div live-link
  [position absolute]
  [right 0]
  [top 0])

(define-div speech
  [margin-top "2em"]
  [position relative])

(define-div bio-div
  [margin-top "0.5em"])

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
                                 name
                                 " ("
                                 (txexpr* 'span
                                          (list (list 'class "p-org"))
                                          affiliation)
                                 ")"))]
                 [else
                  (list (txexpr* 'a attrs name))])]))
  (txexpr 'span
          (list (list 'class "speaker-a"))
          span-kids))
(define (lecture #:when when
                 #:who who
                 #:link [l #f]
                 #:what [what ""]
                 #:more [more ""])
  (speech when
          who
          (if l
           (live-link "" (a #:href l "talk link"))
           "")
          what
          more))

(define (hallway when)
 (lecture #:when when #:who @speaker[#:person? #f]{@bold{Hallway}}))

(define (doors-open when)
 (lecture #:when when #:who @speaker[#:person? #f]{@bold{Doors Open}}))

(define (coffee when)
 (lecture #:when when #:who @speaker[#:person? #f]{@bold{Coffee}}))

(define (lunch when)
 (lecture #:when when #:who @speaker[#:person? #f]{@bold{Lunch}}))

(define (bio . contents)
 (apply bio-div @bold{Bio: } contents))

(define (q content)
  `(q () ,content))

(define saturday (gregor:date 2023 10 28))
(define sunday (gregor:date 2023 10 29))
(define location "Evanston, IL, USA")

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
 (define tz (with-timezone (on-date t d) "America/Chicago"))
 (define m (adjust-timezone tz "Etc/UTC"))
 (talk-time-div
  `(span ([data-slot-time ,(moment->iso8601 m)])
    ,(~t tz "EEEE, h:mma zz"))))

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
    @title{(thirteenth RacketCon)}
    @script{
$(document).ready(function () {
 $("[data-slot-time]").each(function() {
  var date = new Date($(this).data("slot-time"));
  var localTime = moment.tz(date, "America/Chicago").format("dddd, h:mma zz")
  $(this).html(localTime); }); }); })
    (body
     #:class "main h-event"
     #:itemscope ""
     #:itemtype "https://schema.org/Event"
     (meta #:itemprop "startDate" (gregor:~t saturday "y-MM-d"))
     (meta #:itemprop "endDate" (gregor:~t sunday "y-MM-d"))
     (meta #:itemprop "location" location)
     (banner
      (title-container
       (title-append
        @pagetitle[(img #:style "width:140px; float: right"
                        #:src "https://racket-lang.org/img/racket-logo.svg"
                        #:alt "The Racket logo")]
        @pagetitle["(thirteenth" (br) 'nbsp "RacketCon)" 'nbsp 'nbsp 'nbsp]))
      @subtitle{October 28-29, 2023}
      @subtitle{@`(span ((class "p-location")) "Northwestern University")}
      @subsubtitle{@`(span ((class "p-locality")) ,location)})

(txexpr* 'time `((class "dt-start") (hidden "") (datetime ,(gregor:~t saturday "y-MM-dd"))))
(txexpr* 'time `((class "dt-end") (hidden "") (datetime ,(gregor:~t sunday "y-MM-dd"))))

(column

 (section
  @sectionHeader{Saturday, October 28th}

  @para{@bold{Room}: TBD}

  @doors-open[@talk-time{Saturday, 8:30am}]

  @para{NB Breakfast won’t be served! Please eat before coming to the event.}

  @lecture[
#:when
@talk-time{Saturday, 9:00am}
#:who
@speaker[#:url "https://www.crockford.com/riddle.html"]{Douglas Crockford}
#:what
@talk{From Here To Lambda And Back Again}
]

  @coffee[@talk-time{Saturday, 10:00am}]

  @lecture[
#:when
@talk-time{Saturday, 10:30am}
#:who
@speaker[#:url "https://sagegerard.com"]{Sage Gerard}
#:what
@talk{Introducing Rackith}
#:more
@abstract{
Rackith is a language-oriented programming language based on Racket. Use Rackith to define many languages with one syntax object. Discussion covers project design and implications for the personal computer.
}
]

  @lecture[
#:when
@talk-time{Saturday, 11:00am}
#:who
@speaker[#:url "http://eecs.northwestern.edu/~czu2221/"]{Chenhao Zhang}
#:what
@talk{@code{#lang Karp}: Formulating and Random Testing NP Reductions}
#:more
@abstract{
Reduction, a pervasive idea in computer science, is often
taught in algorithm courses with NP problems. The
traditional pen-and-paper approach is notoriously
ineffective both for students and instructors: Subtle
mistakes in reductions are often hard to detect by merely
inspecting the purported solutions. Constructing a
counterexample by hand to expose the mistake is even more
onerous. Based on the observation that reductions are
actually programs, we designed @code{#lang Karp}, a DSL for
formulating and random testing NP reductions. In this talk,
I’m going to discuss the implementation of Karp on top of
Racket and solver-aided host language @a[#:href "https://docs.racket-lang.org/rosette-guide/index.html"]{Rosette}.
}
]

  @lecture[
#:when
@talk-time{Saturday, 11:30am}
#:who
@speaker[#:url "https://llazarek.github.io/home.html"]{Lukas Lazarek}
#:what
@talk{Mutate: inject bugs into your programs!}
#:more
@abstract{
In this talk I’ll introduce @code{@(a #:href "https://docs.racket-lang.org/mutate/" "mutate")}, a library for mutating programs, i.e. injecting possible bugs by making small syntactic changes to the program syntax. I’ll talk about what mutation is, why one might want it, and demo how to use the library.
}
]

  @lecture[
#:when
@talk-time{Saturday, 12:00pm}
#:who
@speaker[#:url "https://github.com/adamperlin"]{Adam Perlin}
#:what
@talk{Incrementally Developing Support for Racket->Wasm Compilation}
#:more
@abstract{
Wasm is an attractive compiler target for a variety of reasons: it has support in all major browsers, its isolation guarantees are beneficial for security reasons, and it has potential as a general-purpose platform-independent execution environment. However, adding Wasm support to Racket has proven a challenging problem due to differences in the execution model each language uses at runtime. Chez Scheme, the backend of Racket CS, utilizes code generation conventions which are difficult to adapt to Wasm. This talk will present an alternative approach to Racket-to-Wasm compilation which is compatible with Racket CS. The approach is accomplished by using an existing bytecode format and interpreter which are already supported under Chez Scheme, and performing an ahead-of-time translation of portions of bytecode programs into Wasm. This sets up an incremental approach to the development of a Racket-to-Wasm compilation system.
}
]

  @lunch[@talk-time{Saturday, 12:30pm}]

  @para{Lunch will served buffet-style right next to the lecture hall.}

  @lecture[
#:when
@talk-time{Saturday, 2:00pm}
#:who
@speaker[#:url "https://wphomes.soic.indiana.edu/jsiek/"]{Jeremy Siek}
#:what
@talk{Teaching and Learning Compilers Incrementally}
#:more
@abstract{
This talk is an introduction to the joys of teaching and
learning about compilers using the incremental approach. The talk
provides a sneak-preview of a compiler course based on the new
textbooks from MIT Press, @book-title{Essentials of Compilation: An Incremental
Approach in @a[#:href "https://mitpress.mit.edu/9780262047760/essentials-of-compilation/"]{Racket}/@a[#:href "https://mitpress.mit.edu/9780262048248/essentials-of-compilation/"]{Python}}.
The course takes students on a journey
through constructing their own compiler for a small but powerful
language.  The standard approach to describing and teaching compilers
is to proceed one pass at a time, from the front to the back of the
compiler. Unfortunately, that approach obfuscates how language
features motivate design choices in a compiler. In this course we
instead take an incremental approach in which we build a complete
compiler every two weeks, starting with a small input language that
includes only arithmetic and variables. We add new language features
in subsequent iterations, extending the compiler as necessary.
Students get immediate positive feedback as they see their compiler
passing test cases and then learn important lessons regarding software
engineering as they grow and refactor their compiler throughout the
semester.
 }
]

  @lecture[
#:when
@talk-time{Saturday, 2:30pm}
#:who
@speaker[#:url "https://www.thelittlelearner.com"]{Anurag Mendhekar and Daniel P. Friedman}
#:what
@talk{Malt: A Deep Learning Framework for Racket}
#:more
@abstract{
We discuss the design of a deep learning toolkit, @a[#:href "https://github.com/themetaschemer/malt"]{Malt}, that has been built for Racket. Originally designed to support the pedagogy of @a[#:href "https://mitpress.mit.edu/9780262546379/the-little-learner/"]{@book-title{The Little Learner—A Straight Line to Deep Learning}}, it is used to build deep neural networks with a minimum of fuss using tools like higher-order automatic differentiation and rank polymorphism.  The natural, functional style of AI programming that Malt enables can be extended to much  larger, practical applications. We present a roadmap for how we hope to achieve this so that it can become a stepping stone to allow Lisp/Scheme/Racket to reclaim the crown of being @emph{the} language for Artificial Intelligence (perhaps!).
 }
]

  @lecture[
#:when
@talk-time{Saturday, 3:00pm}
#:who
@speaker[#:url "https://github.com/dstorrs"]{David Storrs}
#:what
@talk{Data Integrity via Smart Structs}
#:more
@abstract{
@para{Structs in Racket should be more than dumb data storage.  They should be data models in the sense of MVC programming; they should ensure that their contents are valid according to your project’s business rules and they should make it easy to do common operations such as storing to a database or generating a struct from data of another type such as a database row or user input field.}

@para{The @a[#:href "https://pkgs.racket-lang.org/package/struct-plus-plus" #:title "struct-plus-plus (Racket package)"]{struct-plus-plus} module makes this easy.  It allows you to place contracts on individual fields, specify business rules that ensure integrity between fields, easily create converter functions, and much more, with all of these things being part of the struct definition and therefore in one easily-referenced location.  Come see how it all works and how you can simplify your code with struct-plus-plus!}
}
]

  @coffee[@talk-time{Saturday, 3:30pm}]

  @lecture[
#:when
@talk-time{Saturday, 4:00pm}
#:who
@speaker[#:url "https://github.com/samdphillips"]{Sam Phillips}
#:what
@talk{keyring: Uniformly access secrets}
#:more
@abstract{
Hardcoding passwords in your programs is bad.  Using secure password stores are
good.  @a[#:href "https://pkgs.racket-lang.org/package/keyring" #:title "keyring (Racket package)"]{Keyring} is a Racket library that allows programs to access  different
password stores using a simple interface.
}
]

  @lecture[
#:when
@talk-time{Saturday, 4:30pm}
#:who
@speaker[#:url "https://github.com/countvajhula"]{Siddhartha Kasivajhula}
#:what
@talk{Redeeming open source with attribution based economics}
#:more
@abstract{
Attribution Based Economics (ABE) is a new paradigm for economics that revises several foundational assumptions governing today’s systems, including the nature of economic value and the origin of money. In this new paradigm, open source software becomes economically viable and, indeed, even financially favored over proprietary models. This talk describes our experiences implementing an early prototype for @a[#:href "https://pkgs.racket-lang.org/package/qi" #:title "Qi (Racket package)"]{the Qi project}, and also how Racket will be an essential part of the solution as ABE scales past the pilot stage.}
]

  @lecture[
#:when
@talk-time{Saturday, 5:00pm}
#:who
@speaker[#:url "https://www.micahcantor.com"]{Micah Cantor}
#:what
@talk{Crafting Interpreters in Typed Racket}
#:more
@abstract{
My first Typed Racket program was an interpreter for the Lox language from Bob Nystrom’s book @a[#:href "https://craftinginterpreters.com"]{@emph{Crafting Interpreters}}. In this talk, I’ll discuss the design decisions I made when translating from Nystrom’s Java, as well as the fun and frustrating aspects of Typed Racket I discovered in the process. I’ll also give a retrospective on learning how to adapt a traditional compiler to Racket’s language-oriented paradigm.
}
]

)

 (section
  @sectionHeader{Sunday, October 29th}

  @para{@bold{Room}: TBD}

  @para{NB Breakfast won’t be served! Please eat before coming to the event.}

  @doors-open[@talk-time{Sunday, 9:00am}]

  @lecture[
#:when
@talk-time{Sunday, 9:30am}
#:who
@speaker[#:url "https://users.cs.northwestern.edu/~robby/"]{Robby Findler}
#:what
@talk{Esterel in Racket}
#:more
@abstract{
@para{Concurrency and thread preemption are tools that can make programs more modular. Unfortunately, in conventional programming models, combining state and concurrency (to say nothing of preemption!) makes programs extremely hard to get right.}

@para{Esterel offers a different programming model that is designed such that concurrency, state change, and thread preemption can all be used harmoniously. It dates to the 1980s and is the brainchild of Gérard Berry.}

@para{Unfortunately, the standard implementation technique for Esterel (as embodied in Manuel Serrano’s JS+Esterel integration, HipHop) requires a form of staging that leaks out and affects the programming model. I’ve been working on a different implementation technique that uses continuations as to try to get a more seamless integration with a conventional programming language (Racket, naturally).}

@para{In the talk, I’ll try to use examples to make some sense out of what’s written in the second paragraph and, more generally, give a demo of this @a[#:href "https://docs.racket-lang.org/esterel/" #:title "Esterel in Racket (Racket documentation)"]{new implementation}. If we have time, I’ll also try to explain how the implementation actually works.}

}
]

  @lecture[
#:when
@talk-time{Sunday, 10:00am}
#:who
@speaker[#:url "https://users.cs.utah.edu/~mflatt/" #:affiliation "Utah"]{Matthew Flatt}
#:what
@talk{Rhombus: Status update}
]

  @coffee[@talk-time{Saturday, 10:30am}]

  @lecture[
#:when
@talk-time{Sunday, 11:00am}
#:who
@speaker[#:url "https://samth.github.io" #:affiliation "Indiana"]{Sam Tobin-Hochstadt}
#:what
@talk{The State of Racket}
]

  @lecture[
#:when
@talk-time{Sunday, 11:30am}
#:who
@speaker[#:person? #f]{Racket Management}
#:what
@talk{Racket Town Hall}
#:more
@abstract{

Please come with your big questions and discussion topics.

}]

)

 (section
   @sectionHeader{Registration}
   @paragraph{To register, @a[#:href "https://www.eventbrite.com/e/racketcon-2023-tickets-669052563227"]{buy a ticket via Eventbrite}. If you cannot attend in-person, there is an option on Eventbrite for @emph{remote participation}.}
)

 (section
   @sectionHeader{Friendly Policy}
   @paragraph{The proceedings of RacketCon will take place under the Racket @(a #:href "https://racket-lang.org/friendly.html" "Friendly Environment Policy").}
  )

 (section
  @sectionHeader{Accommodation}

  @paragraph{
 We have reserved a block of rooms at the
 @a[#:href "https://www.hilton.com/en/attend-my-event/ordoehf-906-49eed243-3f13-4694-a6cf-5bc5184f7c39/"]{Hilton Orrington}.
 The block includes Deluxe King rooms at the rate of $189
 per night and Deluxe Queen rooms with two beds at the rate
 of $209 per night. To make a reservation, use the previous
 direct booking link, which already includes the RacketCon
 discount.
 If that link doesn’t work, you may also reserve by phone by
 calling @a[#:href "tel:+18004458667"]{+1 (800) 445-8667}.
 When calling, be sure to tell them you are reserving a room
 to attend
 RacketCon. Reservations must be made by September 27th to
 get the block rate.
}

 )

 (section
  @sectionHeader{Sponsor}
  @center{
 @a[#:href "https://www.cs.northwestern.edu/"]{
  @img[#:src "nwu-cs.svg"
       #:alt "Northwestern Computer Science Logo"]
 }
 }
 )

 (section
  @sectionHeader{Previous RacketCons}
  @(apply larger
               (cdr
                (apply
                 append
                 (for/list ([year '(2022 2021 2020 2019 2018 2017 2016 2015 2014 2013 2012 2011)])
                   (list " ∙ "
                         (a #:href (format "https://con.racket-lang.org/~a/" year)
                            (format "~a" year))))))))))))

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
