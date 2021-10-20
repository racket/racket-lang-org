#lang at-exp racket/base
(require racket/match
         xml
         "lib.rkt")

(define-div column
  ,@centered
  [width "45em"])

(define-div banner
  ,@centered)

(define-div title-append
  [display inline-block]
  (margin-left auto)
  (margin-right auto)
  [text-align left]
  [margin-bottom "1em"])

(define-div pagetitle
  [font-size 64]
  [font-family "'Source Code Pro', monospace"])

(define-div main
  [font-family "'Raleway', sans-serif"])

(define header-font
  `([font-weight "bold"]))

(define-div subtitle
  ,@centered
  [font-size 40]
  ,@header-font)

(define-div section
  [margin-top "3em"])
(define-div sectionHeader
  [font-size 24]
  [margin-bottom "1em"]
  ,@header-font)

(define-a speaker-a
  [font-size 24]
  [color "firebrick"])
(define-div talk
  [font-style "italic"]
  [font-size 24]
  [margin-top "0.25em"]
  [margin-bottom "1em"])

(define-div abstract
  [text-align "left"]
  [margin-left "5em"]
  [margin-right "5em"])

(define-div paragraph)
(define para paragraph)

(define-div plain)

(define-div larger
  [font-size 24])

(define-span bold
  [font-weight bold])

(define-span emph
  [font-style "italic"])

(define-span tt
  [font-family "'Source Code Pro', monospace"])

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

;; ------------------------------------------------------------

(define which 0)
(define (speaker . x)
  (set! which (add1 which))
  (define label (format "slot~a" which))
  (apply speaker-a #:id label #:href (format "#~a" label) x))

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
 (lecture #:when when #:who @speaker{@bold{Hallway}}))

(define (bio . contents)
 (apply bio-div @bold{Bio: } contents))

(define slot-number 0)
(define (talk-time dtime)
 (set! slot-number (add1 slot-number))
 (local-require racket/string gregor)
 (match-define (list day times) (string-split dtime ","))
 (define d (match day
             ["Friday"   (date 2021 11 5)]
             ["Saturday" (date 2021 11 6)]
             ["Sunday"   (date 2021 11 7)]))
 (define t (parse-time times " h:mmaa"))
 (define tz (with-timezone (on-date t d) "America/New_York"))
 (define m (adjust-timezone tz "Etc/UTC"))
 (talk-time-div
  `(span ([data-slot-time ,(moment->iso8601 m)])
    ,(~t tz "EEEE, h:mma zz"))))

;; ------------------------------------------------------------

(define page
  (html
   (head
    (link #:href fonts-url
          #:rel "stylesheet")
    (style (cdata #f #f (classes->string)))
    (style (cdata #f #f "a { text-decoration: none; } "))
    `(script ([src      "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.5.1/jquery.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.27.0/moment.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment-timezone/0.5.31/moment-timezone-with-data-10-year-range.js"]) "")
    @title{(chaperone (eleventh RacketCon))}
    @script{
$(document).ready(function () {
 var zone = moment.tz.guess();
 $("[data-slot-time]").each(function() {
  var date = new Date($(this).data("slot-time"));
  var localTime = moment.tz(date, zone).format("dddd, h:mma zz")
  $(this).html(localTime); }); }); }
    (body
     #:class "main"
     (banner
      (title-append
       @pagetitle[(faded "(chaperone")
                  (img #:style "width:80px; float: right"
                       #:src "https://racket-lang.org/img/racket-logo.svg")]
       @pagetitle[@'nbsp "(eleventh RacketCon)" (faded ")")])
      @subtitle{November 5-7, 2021}
      @subtitle[@faded{Online}])
(column

 (section
  @sectionHeader{Attending}

  @paragraph{Join the @a[#:href "https://gather.town/app/POxm4HbriLKltrzP/racketlang"]{Gather space}; everything is there!}

  @paragraph{@br{}}

  @paragraph{You don't need to register.}

  @paragraph{Really, everything you need to do is in the Gather.}

  @paragraph{Trust me, join it and you'll see people and the talks and everything you need.}

  )

 (section
  @sectionHeader{Friday, November 5th}

  @lecture[
#:when
@talk-time{Friday, 2:00pm}
#:who
@speaker{@bold{Virtual Biergarten}}
]

)

 (section
  @sectionHeader{Saturday, November 6th}
  
  @lecture[
#:when
@talk-time{Saturday, 10:00am}
#:who
@speaker{Stephen R. Foster, PhD &@br{} Lindsey D. Handley, PhD}
#:link
#f
#:what
@talk{Racket + React + Unreal = CodeSpells}
#:more
@abstract{

This talk is a tale of three ecosystems. Racket is finely tuned for creating novel programming
languages. Likewise, React is tuned for creating novel user interfaces, and Unreal is tuned for
creating novel 3D worlds and games. CodeSpells is a platform built atop all three, allowing for
seamless communication between each of these three runtimes.
Why? We'll demonstrate through visually compelling examples how we are using CodeSpells to
create educational technologies, novel programming interfaces, and immersive 3D games. We
hope that you'll leave this talk knowing how you too can benefit from our open-source tools and
examples.

@bio{
Dr. Stephen R. Foster is a researcher, author, and co-founder of multiple social
enterprises with a mission to teach teachers how to teach coding. A fierce advocate for the
power of coding to bring about worldwide change, he has himself coded to generate peer-
reviewed scientific results, coded to build educational technology solutions for teachers and
students, and coded to bootstrap educational startups and non-profit organizations out of thin
air. All in all, these countless lines of code have all been in service of a single vision: to establish
coding education as a basic human right across the globe. In short, he codes to teach coding.
@br{}
Dr. Lindsey D. Handley is a researcher, entrepreneur, teacher, and author. For the last
10 years, the National Science Foundation has funded the research, design work, and the social
enterprises that she operates. As a skilled coder, data scientist, and biochemist, she envisions a
world in which we no longer suffer from a worldwide shortage of scientific fluency. To this end,
she fights for the unification of science and education on two fronts: the use of science to
improve education; and the improved teaching of science worldwide. In short, she applies
science to design better ways of teaching science.

Together, they are the co-founders and leaders of ThoughtSTEM and MetaCoders.org --
two social enterprises that have touched the lives of hundreds of thousands of beginning coders
worldwide. They are also the co-authors of Don't Teach Coding: Until You Read This Book, a
book that uses Racket to illustrate their language-oriented educational philosophy.
}

}]

  @hallway[@talk-time{Saturday, 11:00am}]
  
  @lecture[
#:when
@talk-time{Saturday, 12:30pm}
#:who
@speaker{Hazel Levine}
#:link
#f
#:what
@talk{Slicing Tabular Data with Sawzall}
#:more
@abstract{

dplyr, a data manipulation library for the R programming language created by Hadley Wickham, is a popular way of managing tabular data for data analysis purposes. By speaking the consistent language of tidy data, dplyr is able to achieve high levels of ergonomics, and manage complex data analysis tasks in a compositional, functional style.

@br{}

We present a new Racket library, Sawzall, inspired heavily by dplyr and the relational algebra. Sawzall builds on top of Alex Harsanyi's data-frame package, but provides a set of operations that are designed to compose and avoid mutating the original data-set, leading to a natural style of data manipulation following the idea of "do this, then that". We demonstrate the approach, implementation, and results, with a wide range of example data science tasks.

@bio{Hazel Levine (they/them) is a second-year undergraduate at Indiana University, studying computer science (with a specialization in programming languages) and theoretical mathematics. They are particularly interested in language design and ergonomics, aiming to create tools bridging the gap between concepts typically left in academia and the everyday programmer.}

}]

  @lecture[
#:when
@talk-time{Saturday, 1:00pm}
#:who
@speaker{Sid Kasivajhula}
#:link
#f
#:what
@talk{Qi: A Functional, Flow-Oriented DSL}
#:more
@abstract{

One way to think about a function is as a disjointed logical sequence, which we can depict as a flowchart. This is the usual model we employ when writing functions in a typical programming language (or even an atypical one like Racket), where the absence of constraints gives us a lot of flexibility but also the ability to shoot ourselves in the foot by doing something silly.
@br{}
Another way of thinking about a function is as a flow of energy, more like an electrical circuit. This model, enabled by the Qi DSL, allows us to phrase computations in terms of flows which are simply functional building blocks with an arbitrary number of inputs and outputs that can be composed in series and in parallel to yield other flows. With such flows, we can express many common patterns more economically and more clearly than in the usual ways, eliminating repetition and boilerplate, and deriving all of the power of the functional paradigm.

@bio{As the shadowy lurker in the ramparts of Drym.org, Sid is indeed, as many have suspected, none other than the whispered-about-in-hushed-tones "Count Vajhula," vampirous author of Racket and Emacs packages. When not writing code, Sid spends his time working on fundamental solutions to social and existential problems, all of which he believes can be addressed by transitioning from an economic system based on supply and demand, to an economic system based on attribution, which would preserve and nurture our humanity, rather than feed off of and seep it as capitalism does. Struggling to bring attention to these ideas, Sid came to Racket hoping to write tangible prototypes to demonstrate the promise of these proposals. He's authored a few packages along the way, including the cli language for writing command line interfaces and the relation package for type-generic relations and operators.}

}]

  @hallway[@talk-time{Saturday, 1:30pm}]
  
  @lecture[
#:when
@talk-time{Saturday, 2:30pm}
#:who
@speaker{Bogdan Popa}
#:link
#f
#:what
@talk{Declarative GUIs}
#:more
@abstract{

Racket has an extensive standard library that even includes
support for creating cross-platform graphical user interfaces.  Its
GUI library is flexible but imperative and verbose, thanks to its
reliance on the class system.  In this talk, I will present a new
library, built atop racket/gui, for creating GUI applications in a
declarative style.  I will review its advantages and disadvantages
compared to racket/gui and demo building a small UI with it.

@bio{Bogdan is a software developer based in Cluj-Napoca, Romania.  He
is the author of several Racket libraries, and he runs an e-commerce
business built on top of Racket.}

}]

  @lecture[
#:when
@talk-time{Saturday, 3:00pm}
#:who
@speaker{Ryan Culpepper}
#:link
#f
#:what
@talk{Implementing HTTP/2 with Events, Objects, and Ports}
#:more
@abstract{

In this talk I'll introduce http123, a new HTTP client library that
supports HTTP/2, and give a high-level overview of some of the
interesting parts of its implementation. These include the use of
synchronizable events for multiplexing streams and communication with
user threads; the use of objects and basic OO design, for example to
implement the application-level state machine; and the use of custom
ports to propagate error information to users.

}]

  @hallway[@talk-time{Saturday, 3:30pm}]

  @lecture[
#:when
@talk-time{Saturday, 4:30pm}
#:who
@speaker{Sage Lennon Gerard}
#:link
#f
#:what
@talk{On Integrating Racket with GNU Mes}
#:more
@abstract{

GNU Mes helps one reproduce an exact operating system in terms of auditable binaries. Mes integrates with Guile such that GNU Guix may leverage these benefits. By integrating Mes with Racket, we can boot directly to a Rash shell on a fresh installation. In this system, the full power of a trusted Racket installation is omnipresent and available immediately after login. Sage explains what he has done to reach this goal, and shows what remains to be done.

@bio{Sage is a computer scientist by hobby and trade. His contributions to Racket include aiding its relicensing effort, publishing twenty-one packages released under generous licenses, and researching alternative programming practices. His Vulkan integration won "Best Package" in the 2019 Racket Game Jam, and he maintains a home lab.  He is currently researching how to create a Racket-centric operating system using his dependency manager, Xiden.}

}]
  @lecture[
#:when
@talk-time{Saturday, 5:00pm}
#:who
@speaker{Jack Firth}
#:link
#f
#:what
@talk{Resyntax: A Macro-Powered Refactoring Tool}
#:more
@abstract{

Resyntax is a tool that wields the power of Racket’s macro expander to analyze Racket code and suggest improvements. It uses a domain-specific language to specify refactoring rules in terms of syntax-parse macros. Rules explain why they’re improvements, allowing Resyntax to teach users how to make their code more straightforward, more readable, and more efficient. This talk covers how Resyntax works, why it’s different from tools like code formatters and linters, and what it means for the future of Racket’s static analysis ecosystem.

@bio{Jack Firth (they/them) is a software engineer at Google working on continuous integration systems, Java libraries, and static analysis tools. Special interests include asynchronous programming, large-scale distributed computing, martial arts, pretty diagrams, and dyeing their hair pink.}

}]

  @hallway[@talk-time{Saturday, 5:30pm}]

)

 (section
  @sectionHeader{Sunday, November 7th}

  @lecture[
#:when
@talk-time{Sunday, 10:00am}
#:who
@speaker{Sam Tobin-Hochstadt}
#:link
#f
#:what
@talk{The State of Racket}
#:more
@abstract{

}]

  @lecture[
#:when
@talk-time{Sunday, 10:30am}
#:who
@speaker{Racket Management}
#:link
#f
#:what
@talk{Racket Town Hall}
#:more
@abstract{

Please come with your big questions and discussion topics.

}]

  @hallway[@talk-time{Sunday, 11:00am}]

  @lecture[
#:when
@talk-time{Sunday, 12:30pm}
#:who
@speaker{Jay McCarthy}
#:link
#f
#:what
@talk{C-exprs and L-exprs, oh my!}
#:more
@abstract{

S-expressions impose a uniform grouping discipline on syntax that makes them easy to parse and manipulate.
They are also flexible for adding new syntactic forms to a language.
Languages, like C, JavaScript, Haskell, and Python, have a variety of different rules for grouping and parsing syntax.
These rules can produce beautiful and information dense syntax.
They can also be hard to comprehend and internalize, as well as rigid and difficult to customize for particular programs.
C-expressions are an attempt to get the beauty and information density of C-like languages, without sacrificing comprehension and flexibilty.
L-expressions have the same goal, but try to follow the model of Python & Haskell.

@bio{Jay is a perfectionist misanthrope that wants everything to be perfect.}

}]
  @lecture[
#:when
@talk-time{Sunday, 1:00pm}
#:who
@speaker{Matthew Flatt}
#:link
#f
#:what
@talk{Shrubbery Notation}
#:more
@abstract{

Shrubbery notation is similar to S-expression notation, but instead of
generating fully formed trees, it is intended to partially group input
for further enforestation by another parser (e.g., as in Honu). The
notation is line- and indentation-sensitive, and the parsed form of a
shrubbery imposes grouping to ensure that further parsing is consistent
with the shrubbery's lines and indentation.

@bio{Matthew Flatt is a professor at the University of Utah and one of the
main developers of Racket. He has worked primarily on Racket’s macro
system, run-time system, and compiler.}

}]

  @hallway[@talk-time{Sunday, 1:30pm}]

  @lecture[
#:when
@talk-time{Sunday, 2:30pm}
#:who
@speaker{Matthew Flatt}
#:link
#f
#:what
@talk{A Shrubbery-Flavored Rhombus Experiment}
#:more
@abstract{

This talk will describe a @tt{#lang rhombus} experiment that combines
shrubbery notation for its reader-level syntax with Honu-style
enforestation in its macro-expansion rules. The prototype demonstrates
how shrubbery notation is meant to work, and it serves as a further
proof-of-concept for macro expansion with operators and infix notation.
It also incorporates various ideas from the Rhombus brainstorming wish
list, where the intent is to show how notation interacts with possible
improvements.

@bio{Matthew Flatt is a professor at the University of Utah and one of the
main developers of Racket. He has worked primarily on Racket’s macro
system, run-time system, and compiler.}

}]
  @lecture[
#:when
@talk-time{Sunday, 3:00pm}
#:who
@speaker{Community}
#:link
#f
#:what
@talk{Rhombus Discussion}
#:more
@abstract{

We will have a jam session about Rhombus.

}]

  @hallway[@talk-time{Sunday, 3:30pm}]
  
  @lecture[
#:when
@talk-time{Sunday, 4:30pm}
#:who
@speaker{Jack Firth}
#:link
#f
#:what
@talk{Collections for Rhombus}
#:more
@abstract{

Rhombus is a new language based on Racket, exploring one possible future of macro-extensible programming. This talk covers one part of the Rhombus project: a modern generic collections framework. Rhombus’s approach to collections is influenced by Java’s stream API and interface-based collection views, Clojure’s persistent collections and transducers, and many other Racketeers’ prior work in the field. With lists, sets, multisets, maps, and multimaps galore, come learn a hundred and one different ways to slice a red-black tree.

@bio{Jack Firth (they/them) is a software engineer at Google working on continuous integration systems, Java libraries, and static analysis tools. Special interests include asynchronous programming, large-scale distributed computing, martial arts, pretty diagrams, and dyeing their hair pink.}

}]
  @lecture[
#:when
@talk-time{Sunday, 5:00pm}
#:who
@speaker{Ben Greenman}
#:link
#f
#:what
@talk{Fun and Games 2}
#:more
@abstract{

Last summer was a excellent season for fun, games, and macros in
Racket. Ben will tell us all about the recent Syntax Parse Bee and
present a showcase of contributed macros.

@bio{Ben is a postdoc at Brown University and a fan of the 2019 Standard
Fish competition. He is seeking ideas for a summer 2022 community
event.}

}]

  @hallway[@talk-time{Sunday, 5:30pm}]

       )

 (section
  @sectionHeader{Previous RacketCons}
  @(apply larger
               (cdr
                (apply
                 append
                 (for/list ([year '(2020 2019 2018 2017 2016 2015 2014 2013 2012 2011)])
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
      (write-xexpr page))))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path here ".")
  (make here))
