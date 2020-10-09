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
                 #:what [what ""]
                 #:more [more ""])
  (speech when
          who
          (live-link "" #;(a #:href "tbd" "talk link"))
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
             ["Friday"   (date 2020 10 16)]
             ["Saturday" (date 2020 10 17)]
             ["Sunday"   (date 2020 10 18)]))
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
    @title{(chaperone (tenth RacketCon))}
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
       @pagetitle[@'nbsp "(tenth RacketCon)" (faded ")")])
      @subtitle{October 16-18, 2020}
      @subtitle[@faded{Online}])
(column

 (section
  @sectionHeader{Attending}

  @paragraph{Join the @a[#:href "https://gather.town/app/POxm4HbriLKltrzP/racketlang"]{Gather space}; everything is there!}

  )

 (section
  @sectionHeader{Friday, October 16th}

  @lecture[
#:when
@talk-time{Friday, 2:00pm}
#:who
@speaker{@bold{Virtual Biergarten}}
]

)

 (section
  @sectionHeader{Saturday, October 17th}
  
  @lecture[
#:when
@talk-time{Saturday, 10:00am}
#:who
@speaker{@bold{Keynote:} Kathi Fisler}
#:what
@talk{Data-Centric Computing: The Educational Horizon Expands}
#:more
@abstract{

The latest installment of the language wars for early programming education begins as a new player enters the scene: data science.
Promising to bring engaging real-world applications to tired CS curricula, data science inspires new debates over which programming language is the best starting point for beginners.
As the advocates for R, Python, and Scratch sharpen their arguments, functional programming fans spot an unanticipated opportunity.
Data science could be playing into their hands, if they get their pitch right.
Join in as an important strategic discussion gets underway in the higher-order headquarters ...

@bio{Kathi Fisler is a Professor (Research) of Computer Science at Brown
University and co-director of Bootstrap (a national K-12 outreach
program for integrating computing into existing K-12 courses). She is broadly
interested in various facets of how people learn and use formal
systems. Her current focus is computing education, with an emphasis on
how programming languages impact learning and pedagogy in computing
for each of college students, K-12 students, and K-12
teachers. She was present for the birth of How to Design Programs, and 
has just graduated a PhD student (Francis Castro, WPI) whose work explored 
how students work with the Design Recipe.  Outside of
CS, she likes a good jigsaw puzzle, a bad pun, and the best that pizza
has to offer.}

}]

  @hallway[@talk-time{Saturday, 11:15am}]

  @lecture[
#:when
@talk-time{Saturday, 12:30pm}
#:who
@speaker{Christopher Lemmer Webber}
#:what
@talk{Distributed secure programming with Spritely Goblins}
#:more
@abstract{

Learn how Spritely Goblins uses Racket, object capabilities, and the classic actor model to bring a distributed programming environment that is transactional and safe in a mutually suspicious network.  Learn about Goblins’ inspiration from the E programming language, see a space shooter video game taking advantage of transactionality to implement time-travel debugging, and see a small preview of social network tools that will be built on top of the system.

@bio{Christopher Lemmer Webber is a longtime user freedom advocate and parenthesis enthusiast.  Past work is all over the place from being technical lead of Creative Commons, co-founder of MediaGoblin, and co-author / co-editor of the W3C ActivityPub social network standard.}

}]

  @lecture[
#:when
@talk-time{Saturday, 1:00pm}
#:who
@speaker{Konrad Hinsen}
#:what
@talk{Liberating computational science from software complexity}
#:more
@abstract{

Whereas computers have undoubtedly empowered scientific research in many ways, they have also wrapped scientific models and methods in an opaque layer of software, making them invisible and incomprehensible to the majority of scientists. The increasing use of black-box methods is a major contributor to the much-discussed reproducibility crisis. My attempt to liberate science from software is the introduction of specification languages for scientific models and methods, which I call Digital Scientific Notations. They are meant to be used in user interfaces of scientific software as well as in technical documentation and research papers. I will illustrate this idea with Leibniz, an in-progress notation for physics and chemistry implemented in Racket as an extension to Scribble.

@bio{I am a computational physicist, working mainly on protein structure and dynamics and on the methodology of biomolecular simulation. Unhappy with how the complexity of scientific software increasingly became an obstacle to productive and reliable research, I co-founded the scientific Python ecosystem in order to do science at a higher level than Fortran and C permitted. Now I am tackling the essential complexity in computational science, playing with ideas for specification languages for scientific models.}

}]

  @hallway[@talk-time{Saturday, 1:30pm}]

  @lecture[
#:when
@talk-time{Saturday, 2:30pm}
#:who
@speaker{Brian Adkins}
#:what
@talk{The Joy of Web Development in Racket}
#:more
@abstract{

After a brief history of discovering the ultimate language, we will
walk through building a simple web application using Racket and parts
of the upcoming Axio Web Framework. Along the way, we will look at
some of the features of Racket that make it such a great language for
web development, some of the completed parts of the Axio Web Framework
that enhance it, and a roadmap for the future.

@bio{Brian is a programmer, avid cyclist and guitar playing hack. He's been
programming professionally since the early eighties, and runs a
successful solo consulting business specializing in using Racket to
solve challenging business problems.

Brian has developed software professionally in Ruby, Java, C++, Perl,
S/360 Assembler, BASIC, etc. at companies such as CompuServe, IBM, Sun
Microsystems and a number of small companies & startups. On his way to
discovering Racket, Brian also spent a fair amount of time with
Standard ML, Haskell, OCaml, Common Lisp and Scheme. He graduated Cum
Laude With Distinction from The Ohio State University where he was a
co-winner of the ACM Scholastic Programming Competition.
}

}]

  @lecture[
#:when
@talk-time{Saturday, 3:00pm}
#:who
@speaker{Sage Gerard}
#:what
@talk{Zero-Collection Package Management}
#:more
@abstract{

This talk covers a functional dependency manager for Racket. The dependency manager has no side-effect on a Racket installation, which means it defines no collections. The talk includes discussion of the goals behind this approach, and a short demonstration.

@bio{Sage is a prematurely-bald technophile who dreams to live off the land.  His Vulkan bindings generator won "Best Package" for the 2019 Racket Game Jam, and he played a part in Racket's relicensing effort. Academically, Sage graduated summa cum laude from Kennesaw State University, competed as a NASA Aerospace Scholar, and served as an officer of Phi Theta Kappa. Commercially, he built software for Google, AirBnB, and Carvana, among others.  Besides writing code to relax, Sage enjoys reading, petting his rabbits, reintroducing himself to sunlight, arguing loudly with invisible critics in the shower, and "researching" creatures in Monster Hunter World.}

}]

  @hallway[@talk-time{Saturday, 3:30pm}]

  @lecture[
#:when
@talk-time{Saturday, 4:30pm}
#:who
@speaker{Pierce Darragh}
#:what
@talk{Clotho: A Racket Library for Parametric Randomness}
#:more
@abstract{

Programs such as simulators and fuzz testers often use randomness to walk through a large state space in search of interesting paths or outcomes. These explorations can be made more efficient by employing heuristics that “zero-in” on paths through the state space that are more likely to lead to interesting solutions. Given one path that exhibits a desired property, it may be beneficial to generate and explore similar paths to determine if they produce similarly interesting results. When the random decisions made during this path exploration can be manipulated in such a way that they correspond to discrete structural changes in the result, we call it @emph{parametric randomness}.

Many programming languages, including Racket, provide only simple randomness primitives, making the implementation of parametric randomness somewhat difficult. To address this deficiency, we present Clotho: a Racket library for parametric randomness, designed to be both easy to use and flexible. Clotho supports multiple strategies for using parametric randomness in Racket applications without hassle.

@bio{Pierce Darragh is a “pre-doc” researcher at the University of Utah with an interest in human-centered programming language design. After overcoming his initial allergy to parentheses by way of exposure therapy, he has embraced language-oriented programming as a way of life. Pierce also occasionally posts photos of his cats on Twitter.}

}]

  @lecture[
#:when
@talk-time{Saturday, 5:00pm}
#:who
@speaker{Gustavo Massaccesi}
#:what
@talk{A Type Recovery Pass for Chez Scheme}
#:more
@abstract{

A program in Racket CS is expanded and then transformed into a program in Chez Scheme to complete the compilation. The program has many run time checks due to the transformation and also the ones that were in the original Racket program or it's expansion.
We added a pass to the version of Chez Scheme used in Racket CS that does a control-flow analysis that includes the information gained with the dynamic type predicates like @tt{box?} and  type restricted operators like @tt{unbox}. This allows the reduction of many of the redundant run time checks, adding only @tt{O(n log^2(n))} time to the compilation.

@bio{Gustavo Massaccesi is a Mathematics Professor at the University of Buenos Aires. His research focuses on Math applied to Molecular Physics. He is also the Coordinator of the Jury of the Ñandú Mathematical Olympiad for primary school students in Argentina.}

}]

  @hallway[@talk-time{Saturday, 5:30pm}]

)
      
 (section
  @sectionHeader{Sunday, October 18th}

  @lecture[
#:when
@talk-time{Sunday, 10:00am}
#:who
@speaker{Sam Tobin-Hochstadt}
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
@speaker{Jesse Alama}
#:what
@talk{Black-box HTTP API testing with @tt{#lang riposte}}
#:more
@abstract{

If you’re into testing, you might like the challenge of doing system
(or integration) tests for a menagerie of JSON-speaking HTTP APIs,
organized as distinct microservices. Sure, you can write those system
tests using whatever language you want (for example, the language in
which the microservices were written). But what if those tests start
to look awkward and ugly? Would you make a #lang? With Racket, you
certainly can, and I did! Riposte is my DSL that tries to compactly
express (some of) the domain of JSON-based HTTP APIs. Here, URI
Template, JSON Schema, a rough-and-ready language for specifying HTTP
requests, and (of course) JSON are just some commonly occurring
languages used to express concepts in this domain. In Riposte, they
are available as-is (not in an encoded form, and without using
S-expressions). Come find out how I made this with Racket!

@bio{Jesse Alama is a theorem prover who works at a PHP shop in Germany as a full-stack developer.}

}]

  @lecture[
#:when
@talk-time{Sunday, 1:00pm}
#:who
@speaker{Peter Zhong}
#:what
@talk{Breaking Down Your Defense: Building Up Contracts from Defensive Programming}
#:more
@abstract{

Liskov, who coined the term defensive programming, advocated for programs to be written with, in her words, “a streak of suspicion”, checking the validity of inputs in every procedure. Meyer, however, stresses that these checks are often not performed systematically and can be scattered in places. Hence, they can pollute codebases, hamper performance and increase complexity. Contracts to the rescue! By lifting the defensive checks to the interface level of a component, contracts concisely articulate and enforce obligations on communicating components, ensuring that each piece of a software system does its job and can rely on the others parts to do theirs. 

Code written defensively remains prevalent, however, and its manual conversion to contracts is a time consuming process — but it doesn't have to be. In this talk, I present a tool that automates the translation of legacy defensive checks in Racket programs to equivalent contracts. In particular, I will describe how the tool identifies and fixes a defensive Racket program: starting from the fully expanded form, the tool descends down the conditions looking for errors, constructs contracts from their test positions, and finally removes residual defensive code.

@bio{Peter Zhong is a rising sophomore studying computer science at Northwestern University. Thanks to his Covid-19 motivated return to home, he now gets to enjoy the beautiful Australian coastal sunrises, right before heading to bed.}

}]

  @hallway[@talk-time{Sunday, 1:30pm}]

  @lecture[
#:when
@talk-time{Sunday, 2:30pm}
#:who
@speaker{Panicz Godek}
#:what
@talk{Deconstructing Textuality of Programs}
#:more
@abstract{

In the recent years, there've been several interesting attempts in the area of non-textual programming environments. However, the dominating idea - that programs essentially are text - seems to prevail in the heads and the tools of software developers. The purpose of this talk is to recognize the less apparent properties of text in order to pave the way to programming environments based on direct structure manipulation, rather than on serialization formats of programs.

@bio{Panicz is a guy who figured out how to write Android apps on his phone, and has not been heard from since.}

}]

  @lecture[
#:when
@talk-time{Sunday, 3:00pm}
#:who
@speaker{Stephen De Gabrielle}
#:what
@talk{Fun and Games}
#:more
@abstract{

Stephen will talk briefly about having fun with Racket, aided by a showcase of contributions to community events including the 2019 standard-fish summer picture competition, the 1st Racket GameJam, and the Quickscript competition.

@bio{Stephen is a Healthcare integration developer working at a London hospital. If you ask about HL7 he will block you.}

}]

  @hallway[@talk-time{Sunday, 3:30pm}]

  @lecture[
#:when
@talk-time{Sunday, 4:30pm}
#:who
@speaker{Ben Greenman}
#:what
@talk{Shallow Typed Racket}
#:more
@abstract{

Typed Racket adds a new dimension to Racket; any module in a program can be strengthened with full-power static types. Good! But powerful types rely on contracts, and these contracts can make a program run unusably slow. Not good! Shallow Typed Racket adds a new dimension to Typed Racket. Instead of deep and powerful types, Shallow offers weak types. And instead of potentially-devastating run-time overhead, shallow types charge a small fee for every line of typed code. Your code of tomorrow can mix deep and shallow types to find the best fit.

@bio{Ben Greenman is a final-semester Ph.D. student at Northeastern University. Next year, he will be studying how programmers interact with contracts and types. Reach out if you've been puzzling over blame errors.}

}]

  @lecture[
#:when
@talk-time{Sunday, 5:00pm}
#:who
@speaker{Matthias Felleisen}
#:what
@talk{Ask Me Anything}
#:more
@abstract{

Please come with your big questions to ask Racket's CPS.

}]

  @hallway[@talk-time{Sunday, 5:30pm}]

       )

 (section
  @sectionHeader{Previous RacketCons}
  @(apply larger
               (cdr
                (apply
                 append
                 (for/list ([year '(2019 2018 2017 2016 2015 2014 2013 2012 2011)])
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
