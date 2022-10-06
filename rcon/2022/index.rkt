#lang at-exp racket/base

(require racket/match
         racket/string
         xml
         txexpr
         (prefix-in gregor: gregor)
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
  [font-size "64pt"]
  [font-family "'Source Code Pro', monospace"])

(define-div main
  [font-family "'Raleway', sans-serif"])

(define header-font
  `([font-weight "bold"]))

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
  ,@header-font)

(define-a speaker-a
  [font-size "24pt"]
  [color "firebrick"])
(define-div talk
  [font-style "italic"]
  [font-size "24pt"]
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
  [font-size "24pt"])

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

(define (code content)
 `(code () ,content))

(define reg-form-url "https://form.typeform.com/to/oNbZByZQ")

;; ------------------------------------------------------------

(define (speaker . x)
  `(span ((class "speaker")) ,@x))

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

(define (coffee when)
 (lecture #:when when #:who @speaker{@bold{Coffee}}))

(define (lunch when)
 (lecture #:when when #:who @speaker{@bold{Lunch}}))

(define (bio . contents)
 (apply bio-div @bold{Bio: } contents))

(define (q content)
  `(q () ,content))

(define friday  (gregor:date 2022 10 28))
(define saturday (gregor:date 2022 10 29))
(define sunday (gregor:date 2022 10 30))
(define location "Providence, RI, USA")

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
             ["Friday"   friday]
             ["Saturday" saturday]
             ["Sunday"   sunday]))
 (define t (parse-time times " h:mmaa"))
 (define tz (with-timezone (on-date t d) "America/New_York"))
 (define m (adjust-timezone tz "Etc/UTC"))
 (talk-time-div
  `(span ([data-slot-time ,(moment->iso8601 m)])
    ,(~t tz "EEEE, h:mma zz"))))

;; ------------------------------------------------------------

(define page
  (html #:lang "en"
   (head
    (link #:href fonts-url
          #:rel "stylesheet")
    (style (cdata #f #f (classes->string)))
    (style (cdata #f #f "a { text-decoration: none; } "))
    `(script ([src      "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.3/moment.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment-timezone/0.5.34/moment-timezone-with-data-10-year-range.js"]) "")
    @title{(twelfth RacketCon)}
    @script{
$(document).ready(function () {
 $("[data-slot-time]").each(function() {
  var date = new Date($(this).data("slot-time"));
  var localTime = moment.tz(date, "America/New_York").format("dddd, h:mma zz")
  $(this).html(localTime); }); }); })
    (body
     #:class "main"
     #:itemscope ""
     #:itemtype "https://schema.org/Event"
     (meta #:itemprop "startDate" (gregor:~t friday "y-MM-d"))
     (meta #:itemprop "endDate" (gregor:~t sunday "y-MM-d"))
     (meta #:itemprop "location" location)
     (banner
      (title-append
       @pagetitle[(img #:style "width:80px; float: right"
                       #:src "https://racket-lang.org/img/racket-logo.svg"
                       #:alt "The Racket logo")]
       @pagetitle["(twelfth" (br) 'nbsp "RacketCon)" 'nbsp 'nbsp 'nbsp])
      @subtitle{October 28-30, 2022}
      @subtitle{Brown University}
      @subsubtitle{location})

(column

 (section
  @sectionHeader{Friday, October 28th}

  @paragraph{Friday afternoon is for hackathons! More details to follow.}

  @lecture[
#:when
@talk-time{Friday, 2:00pm}
#:who @speaker{Internals Nerds Unite}
#:what @talk{Hacking Racket Internals}
#:more
@abstract{Your chance to dig in to Racket internals and help contribute to Racket development! As the Racket community grows, there’s an increasing interest in not just making cool Racket programs, but getting into the guts of Racket itself. If that sounds interesting to you, this is your hackathon! Sam Tobin-Hochstadt will offer a brief kick-off crash course and stick around for answering questions.}
]

)

 (section
  @sectionHeader{Saturday, October 29th}

  @lecture[
#:when
@talk-time{Saturday, 09:30am}
#:who
@speaker{@(a #:href "https://github.com/titzer" "Ben L. Titzer") (CMU)}
#:what
@talk{The final tier is Shed: Inside the Wizard Engine’s fast in-place interpreter for WebAssembly}
#:more
@abstract{
@(a #:href "https://webassembly.org" "WebAssembly") is a compact, well-specified bytecode format that offers a portable compilation target with near-native execution speed. The bytecode format was specifically designed to be fast to parse, validate, and compile, positioning itself as a portable alternative to native code. It was pointedly not designed to be @(em "interpreted") directly. Instead, most engines have focused on optimized JIT compilation for maximum performance. Yet compilation time and memory consumption critically impact application startup, leading many Wasm engines to now employ two compilers. But interpreters start up even faster. A typical interpreter being infeasible, some engines resort to compiling Wasm not to machine code, but to a more compact, but easy to interpret format. This still takes time and wastes memory. Instead, we introduce a new design for an in-place interpreter for WebAssembly, where no rewrite and no separate format is necessary. Our measurements show that in-place interpretation of Wasm code is space-efficient and fast, achieving performance on-par with interpreting a custom-designed internal format. This fills a hole in the execution tier space for Wasm, allowing for even faster startup and lower memory footprint than previous engine configurations.

@bio{Ben L. Titzer is a Principal Researcher at Carnegie Mellon University. A former member of the V8 team at Google, he co-founded the WebAssembly project, led the team that built the implementation in V8, and led the initial design of V8’s TurboFan optimizing compiler. Prior to that he was a researcher at Sun Labs and contributed to the Maxine Java-in-Java VM. He is the designer and main implementer of the Virgil programming language.}
}
]

  @coffee[@talk-time{Saturday, 10:30am}]

  @lecture[
#:when
@talk-time{Saturday, 11:00am}
#:who
@speaker{@(a #:href "https://pp.ipd.kit.edu/person.php?id=144" "Sebastian Ullrich") (KIT)}
#:what
@talk{Metaprograms and Proofs: Macros in Lean 4}
#:more
@abstract{

A core feature of the @(a #:href "https://leanprover.github.io" "Lean 4 programming language") and theorem prover is
an expressive macro system, taking heavy inspiration from Racket. In
this talk, we give an overview of macros in Lean and discuss the ideas
we took from Racket as well as the problems we decided to solve in a
different way. In particular, we talk about recent work on @(q "typed macros")
that prevent many common mistakes by Lean macro authors.

@bio{Sebastian is a PhD student at Karlsruhe Institute of Technology,
Germany, and a Lean core developer. He enjoys both working on the
user-facing frontend of the system as well as on the code generation
backend to make users and binaries go fast.}
}
]

  @lecture[
#:when
@talk-time{Saturday, 11:30am}
#:who
@speaker{@(a #:href "http://cs.brown.edu/people/bgreenma/" "Ben Greenman") (Brown)}
#:what
@talk{Shallow and Optional Types}
#:more
@abstract{
Typed Racket (TR) is powerful—but sometimes too powerful. In addition to a
type checker and type-driven optimizer, it includes a contract layer that
dynamically monitors interactions with untyped code. The contracts make TR one
of the strongest and most flexible type systems in the world, but also one
of the slowest when there are many boundaries to untyped.

Shallow TR and Optional TR are two alternatives that have (finally!) arrived
with the Racket 8.7 release. Shallow TR enforces types with local assertions
rather than compositional contracts, keeping a bit of soundness at low cost.
Optional TR enforces types with nothing at all. This talk will explain Shallow
and Optional in depth and show how they can interact with untyped code,
standard TR, and each another.

@bio{Ben is currently a postdoc at Brown University studying human factors for type
systems and logics. Next Fall, he will be an assistant professor at the
University of Utah. Reach out if you would like to live on a mountain for N
years studying programming languages.}
}
]

  @lecture[
#:when
@talk-time{Saturday, 12:00pm}
#:who
@speaker{@(a #:href "http://camoy.name" "Cameron Moy") (Northeastern)}
#:what
@talk{Contracts for protocols}
#:more
@abstract{
Racketeers often use contracts to express the obligations that their libraries
impose on, or promise to, clients. While Racket’s contract system can handle
many specifications, it cannot naturally express protocols. For example,
a specification may constrain the permitted call sequence of functions,
or the context in which functions may be applied. This talk will present
several extensions to Racket’s contract system that attempt to fill this gap.

@bio{Cameron is a Ph.D. student studying programming languages at Northeastern University.
He spends most of his time thinking about how to make Racket’s contract system better
for developers.}
}
]

  @lunch[@talk-time{Saturday, 12:30pm}]

  @lecture[
#:when
@talk-time{Saturday, 2:30pm}
#:who
@speaker{@(a #:href "https://www.shu.edu/profiles/marcomorazan.cfm" "Marco Morazán") (Seton Hall)}
#:what
@talk{What Can Beginners Learn from Video Games?}
#:more
@abstract{
Beginners need to learn important Computer Science concepts revolving around problem solving, program design, modularity, documentation, testing, efficiency, running time, and program refinement. This presents unique challenges given students that are enthusiastic but have little experience quickly lose interest. Instructors must capture their imagination to channel their enthusiasm into learning the important lessons. An effective medium to do so is the development of video games. This talk outlines a design-based curriculum for beginners that is based on video game development.

@bio{Dr. Marco T. Morazán joined Seton Hall in 1999. He did his undergraduate studies at Rutgers University and his graduate work at the City University of New York. At Seton Hall he teaches at all levels of the Computer Science curriculum including his signature courses: @emph{Introduction to Program Design} I and II, @emph{Organization of Programming Languages}, and @emph{Automata Theory and Computability}. His main research foci are the implementation of programming languages and Computer Science Education. As the graduate school advisor, he takes special pride in making sure that his students are prepared to continue studies outside of Seton Hall. Dr. Morazán is a strong proponent of undergraduate research opportunities and routinely has students collaborate with him on projects. Along with his undergraduate research students, he is responsible for an optimal lambda lifting algorithm and an effective mechanism for closure memoization. In Computer Science education, he is especially proud of the effectiveness of the Computer Science curriculum, based on the development of video games, he has developed for beginners.}
}
]

  @lecture[
#:when
@talk-time{Saturday, 3:00pm}
#:who
@speaker{@(a #:href "https://bicompact.space" "Hazel Levine") (Indiana)}
#:what
@talk{Design Recipe Guided Synthesis with Bingus}
#:more
@abstract{
The @emph{How to Design Programs} (@(a #:href "htdp.org" "HtDP")) curriculum,
utilizing simple subsets of the Racket programming language, teaches
the fundamentals of data-driven program design using the @q{design recipe}.
This approach teaches recursion by structural decomposition of
the input data, enough to express many algorithms on simple recursive
data structures. Furthermore, this approach is deeply mechanical,
having students write a @q{template} depending on the input type of the
function that when filled in produces structurally-recursive programs.

We present a work-in-progress program synthesizer for the HtDP Student
languages, @(a #:href "https://github.com/ralsei/bingus" "Bingus"), that utilizes the design recipe as its primary means
of generating programs. By parsing the signature (specification in a
comment) of a function, the first step of the design recipe, Bingus
makes guesses depending on the signatures of the function inputs,
utilizing the check-expects (unit tests) to determine when synthesis is
complete.

We demonstrate usage of Bingus as a program synthesis tool integrated
into DrRacket, and discuss ways that we plan to extend this tool for
pedagogic purposes, such as providing better feedback to students from
an auto-grader, or determining when student-provided unit tests are
insufficient.
}
]

  @coffee[@talk-time{Saturday, 3:30pm}]

  @lecture[
#:when
@talk-time{Saturday, 4:00pm}
#:who
@speaker{@(a #:href "http://leifandersen.net" "Leif Andersen") (Northeastern)}
#:what
@talk{VISr: Visual and Interactive Syntax}
#:more
@abstract{
While macros continue to take us to the frontiers of what is possible with embedded Domain Specific Languages, they are still limited to textual programming languages. Visual and Interactive Syntax realized (VISr) introduces the world to hybrid textual and visual programming languages. Programmers are no longer forced to choose between readable code and runnable code. VISr gives programmers the power of Language Oriented Programming...now with pictures. This talk teaches programmers how to use VISr for ClojureScript. It also introduces Frankenstein: an early prototype of VISr for Racket.

@bio{Leif Andersen is a postdoctoral Researcher studying programming languages in the University of Massachusetts Boston. She  studies topics in Programming Languages and Human Computer Interaction. Specifically, she works on domain-specific languages for creating hybrid textual-visual programs.}
}
]

  @lecture[
#:when
@talk-time{Saturday, 4:30pm}
#:who
@speaker{@(a #:href "https://mballantyne.net" "Michael Ballantyne") (Northeastern)}
#:what
@talk{A language workbench in Racket}
#:more
@abstract{
Racket’s macro system gives programmers immense power to create domain specific languages, but little help in structuring their implementation. Building a sophisticated DSL requires following a bevy of design patterns for structuring a parser, checking scoping and binding rules, and conveying static information between language elements. This talk presents a new meta-DSL that instead supports declarative specification of grammar, binding rules, and interface points.

@bio{Michael is a PhD student at Northeastern University working on making Racket a better host for DSLs.}
}
]

)

 (section
  @sectionHeader{Sunday, October 30th}

  @lecture[
#:when
@talk-time{Sunday, 09:00am}
#:who
@speaker{@(a #:href "https://github.com/jackfirth" "Jack Firth")}
#:what
@talk{Resyntax: A Macro-Powered Refactoring Tool}
#:more
@abstract{
Resyntax is a tool that wields the power of Racket’s macro expander to analyze Racket code and suggest improvements. It uses a domain-specific language to specify refactoring rules in terms of syntax-parse macros. Rules explain why they’re improvements, allowing Resyntax to teach users how to make their code more straightforward, more readable, and more efficient. This talk covers how Resyntax works, why it’s different from tools like code formatters and linters, and what it means for the future of Racket’s static analysis ecosystem.
@bio{Jack Firth (they/them or she/her) is a software engineer at Google working on continuous integration systems, Java libraries, and static analysis tools. Special interests include asynchronous programming, large-scale distributed computing, martial arts, pretty diagrams, and dyeing their hair pink.}
}
]

  @lecture[
#:when
@talk-time{Sunday, 09:30am}
#:who
@speaker{@(a #:href "https://github.com/sorawee" "Sorawee Porncharoenwase") (Washington)}
#:what
@talk{fmt: A Racket code formatter}
#:more
@abstract{
@(a #:href "https://pkgs.racket-lang.org/package/fmt" "fmt") is a code formatter for Racket. Its applications range from teaching beginners the Racket coding conventions to allowing frictionless collaborative projects. As Racket allows user-defined macros and has a relatively non-traditional code style, fmt faces unique challenges: it must be extensible yet expressive enough to capture the style. This talk will cover the design of fmt, how it overcomes these challenges, and how to use our code formatting DSL to extend fmt.
}
]

  @lecture[
#:when
@talk-time{Sunday, 10:00am}
#:who
@speaker{@(a #:href "http://cs.brown.edu/people/bgreenma/" "Ben Greenman")}
#:what
@talk{Summary of the Summer of @code{#lang} (Fun + Games III)}
#:more
@abstract{
Come learn about the @(a #:href "https://github.com/lang-party/Summer2022" "amazing entries") to this summer’s @code{#lang} party!
Submissions include new languages, improved languages, language ideas, and
Standard ML.

@bio{Ben is a postdoc at Brown University and a co-organizer of the lang party with
Stephen DeGabrielle.}
}
]

  @coffee[@talk-time{Sunday, 10:30am}]

  @lecture[
#:when
@talk-time{Sunday, 11:00am}
#:who
@speaker{@(a #:href "https://samth.github.io" "Sam Tobin-Hochstadt") (Indiana)}
#:what
@talk{The State of Racket}
]

  @lecture[
#:when
@talk-time{Sunday, 11:30am}
#:who
@speaker{Racket Management}
#:what
@talk{Racket Town Hall}
#:more
@abstract{

Please come with your big questions and discussion topics.

}]

       )

 (section
   @sectionHeader{Registration}
   @paragraph{To help gauge interest in the event, please complete the @(a #:href reg-form-url "registration form") if you would like to attend.}
  )

 (section
    @sectionHeader{COVID-19 Policy}
    @paragraph{RacketCon 2022 will be governed by @(a #:href "https://healthy.brown.edu/campus-activity-status" #:title "Summary of current COVID-19 health protocols on the Brown University campus" "the COVID-19 policy in place at tbe conference venue") (Brown University). Please familiarize yourself with it before coming. Because the pandemic is not static, the policy may change. Take one last look at Brown’s COVID-19 policy shortly before coming to campus. Once you’re on campus, you are expected to comply with the policy.}
 )

 (section
   @sectionHeader{Friendly Policy}
   @paragraph{The proceedings of RacketCon are expected to take place under the Racket @(a #:href "https://racket-lang.org/friendly.html" "Friendly Environment Policy").}
  )

 (section
   @sectionHeader{Getting There}
   @paragraph{The Brown Computer Science Department has put together @(a #:href "https://cs.brown.edu/about/directions/" "a great page") with instructions for how to reach the department by plane, train, or car. Once you’re at the right building, RacketCon signs will guide you to the RacketCon place.}
 )

 (section
  @sectionHeader{Accommodation}
  @paragraph{Stay wherever you want! There are plenty of hotels in the area.}

  @paragraph{That said, we have reserved blocks at two local hotels. The booking links below may still work even if all rooms in the bloom have been booked and the discount has expired. You may be guided to a generic booking form unconnected to RacketCon. In no particular order:}

  @ul{
    @li{@bold{@(a #:href "https://www.hilton.com/en/attend-my-event/pvdwyhx-bcr-103bbe07-4367-4aef-8cd5-0a6f6c5f418c/" "Hampton Inn & Suites")}}
    @li{@bold{@(a #:href "https://www.my-event.hilton.com/pvdexhw-brg-9e543c15-7dce-4fb0-aaa5-1d1c1f4020ec/" "Homewood Suites")}}
  }
 )

 (section
  @sectionHeader{Previous RacketCons}
  @(apply larger
               (cdr
                (apply
                 append
                 (for/list ([year '(2021 2020 2019 2018 2017 2016 2015 2014 2013 2012 2011)])
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
