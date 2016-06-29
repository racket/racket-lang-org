#lang pollen
◊(require racket/file pollen/cache)

◊(define rc-2016-date "18 Sept 2016")
◊h1{◊(object #:id "rcon_svg" #:type "image/svg+xml" #:data "rcon.svg")}


◊h2{◊xlink["register"]{◊rc-2016-date}}
◊h2{◊xlink["register"]{St. Louis: Union Station hotel}}
◊h2{◊xlink{Speakers}}
◊h2{◊xlink{Register}}


(sixth RacketCon) is a public meeting for everyone interested in Racket: developers, contributors, programmers, educators, and bystanders. It's an opportunity for all members of the community to come together to share plans, ideas, and enthusiasm. RacketCon will enable the entire Racket community to mingle: to update each other, to exchange ideas, to collaborate, and to help shape the future of Racket.

◊gap[1]

◊h3{◊xtarget["speakers"]{◊(object #:id "keynote_svg" #:type "image/svg+xml" #:data "keynote.svg")}}

◊gap[1]

◊keynote-speaker["" "Emina Torlak"]{Synthesis and Verification for All}

◊gap[0.5]

Rosette is a programming language for creating new programming tools. It extends Racket with a few constructs that make it easy to build advanced tools for program verification and synthesis. Building these tools usually takes months or years of work, as well as expertise in many fields, from formal methods to programming languages to software engineering. With Rosette, creating such a tool is as easy as defining a new domain-specific language in Racket. Once you define your language, you get the tools for (almost) free. This talk will provide a brief introduction to Rosette, concluding with a whirlwind tour of recent applications to finding bugs in radiotherapy software, generating efficent code for ultra low-power hardware, and creating custom tutors for K-12 algebra.

◊bio{◊link["https://homes.cs.washington.edu/~emina/index.html"]{Emina Torlak}  is an assistant professor at the University of Washington. She works on computer-aided design, verification, and synthesis of software. Emina is the creator of Rosette, a new Racket-based language that makes it easy to build efficient tools for verifying and synthesizing all kinds of programs, from radiotherapy controllers to automated algebra tutors.}

◊gap[1.5]

◊h3{◊xtarget["speakers"]{◊(object #:id "speakers_svg" #:type "image/svg+xml" #:data "speakers.svg")}}

◊gap[0]

◊folded{
◊speaker["" "Matthew Butterick"]{The Making of "Beautiful Racket"}
◊;rlink["2015/mccarthy.pdf"]{Slides} ◊;link["https://www.youtube.com/watch?v=BDg79CPbdXQ&list=PLXr4KViVC0qJAsNuDeQzhFDjMK1gEdls8&index=2"]{Video}

I've been devoting the year 2016 strictly to Racket projects. The most ambitious of these is a book about making domain-specific languages with Racket, called ◊link["http://beautifulracket.com"]{Beautiful Racket}. Why a book? Because making my own DSL with Racket — ◊link["http://pollenpub.com/"]{Pollen} — has improved my life tremendously. I want others to share the joy. To do that, however, I've had to forget what I already knew about making DSLs, and confront a few rough edges within Racket's ◊tt{#lang}-making apparatus. Of course, I'm a better person for it. Soon, you will be too: a summary of what I've discovered so far, and questions for further study. 

◊bio{◊link["http://pollenpub.com/"]{Matthew Butterick}: I'm a writer and designer in Los Angeles. I made ◊link["http://practicaltypography.com/"]{Practical Typography}.}}


◊folded{
◊speaker["" "Stephen Chang and Alex Knauth"]{Type Systems as Macros}
◊;rlink["2015/mccarthy.pdf"]{Slides} ◊;link["https://www.youtube.com/watch?v=BDg79CPbdXQ&list=PLXr4KViVC0qJAsNuDeQzhFDjMK1gEdls8&index=2"]{Video}

The Racket Manifesto says that "Racket is a programming language for creating new programming languages". Yet, nearly all languages created with Racket are untyped, ignoring the significant typed portion of the PL spectrum. Typed Racket demonstrates that creating typed languages in Racket is possible ... with monumental effort. This talk attempts to give hope to the common Racket programmer who wishes to create typed languages with Racket, yet does not possess multiple PhDs. Specifically, I'll demonstrate a technique for embedding type checking directly into macro definitions, in a manner that is compatible with the language-creation patterns that Racket programmers are already familiar with, thus leading to typed languages with little effort.

◊bio{◊link["http://www.ccs.neu.edu/home/stchang/"]{Stephen Chang} is a research scientist at Northeastern University who uses Racket to help him explore programming language designs and implementations. ◊link["https://github.com/AlexKnauth"]{Alex Knauth} is a computer science and physics major, also at Northeastern, who loves using Racket and Typed Racket in his pursuit of learning. Together, they enjoy tinkering with and discovering novel applications of Racket's macro system.}}


◊folded{
◊speaker["" "Byron Davies"]{Racket Does Dijkstra}
◊;rlink["2015/mccarthy.pdf"]{Slides} ◊;link["https://www.youtube.com/watch?v=BDg79CPbdXQ&list=PLXr4KViVC0qJAsNuDeQzhFDjMK1gEdls8&index=2"]{Video}

In the name of Systems Integrity Assurance, ◊link["http://Ontopilot.com"]{Ontopilot.com} is prototyping new tools for automatically finding bugs in software, using Dijkstra Guarded Commands and weakest precondition (WP). One member of our team developed a new method for applying WP methods to software, and has used it successfully to find nearly intractable bugs in operating systems and other software. We’re automating the method and incorporating the new Micron Automata Process to make it possible to apply our approach to million-line programs. To prove the concept, we have implemented the method in Racket, using Syntax as the intermediate language before conversion to Dijkstra Guarded Commands, and using the Syntax Browser to examine and debug the multiple representations used to convert a programming language into mathematical logic.

◊bio{◊link["https://twitter.com/daviesaz"]{Byron Davies} has been programming in Lisp for 42 years, at Caltech, MIT, Stanford, Texas Instruments, Motorola, and a host of start-ups. He took up Racket two years ago — and couldn’t be happier about his choice. With Racket he developed a formal methods prototype for Ontopilot.com, demonstrating a novel approach to using weakest precondition to rid programs of bugs. He now spends two hours a day in kindergarten, learning everything he needs to know.}}


◊folded{
◊speaker["" "Jack Firth"]{Language Integrated Nitpicking: Syntax Warnings in Racket}
◊;rlink["2015/mccarthy.pdf"]{Slides} ◊;link["https://www.youtube.com/watch?v=BDg79CPbdXQ&list=PLXr4KViVC0qJAsNuDeQzhFDjMK1gEdls8&index=2"]{Video}

Many popular languages automatically warn developers of potential mistakes with static analysis: dangerous patterns, use of deprecated features, and inconsistent variable names are all caught by automated tools, making it easier for programmers to stay on the same page. This talk presents a Racket library for adding syntax warnings and suggesting refactors via the macro system. True to Racket's philosophy, library users can define their own warnings with equal power to built in suggestions. Next time you argue over style guide rules, resolve it with a new syntax warning so it never comes up again!

◊bio{◊link["http://codepen.io/Universalist/"]{Jack Firth} is a software engineer at Google, working on backend systems in Google Cloud. In his spare time he enjoys thinking about making DSLs (and their authors) cooperate, solving his problems with Racket, microservice oriented distributed systems, and explaining monads in terms of burritos.
}}


◊folded{
◊speaker["" "Alexis King"]{Languages in an Afternoon}
◊;rlink["2015/mccarthy.pdf"]{Slides} ◊;link["https://www.youtube.com/watch?v=BDg79CPbdXQ&list=PLXr4KViVC0qJAsNuDeQzhFDjMK1gEdls8&index=2"]{Video}

Racket is not just a programming language; it is a programming language platform. Ideally, it could be more than a platform, but a greenhouse—a perfect tool for incubating new languages and growing mature ones (general purpose or domain specific). This talk presents a survey of Racket’s tools for building languages, the process it takes to add an entirely new, fully-integrated language to the ecosystem, and the places where that process could be streamlined to achieve the dream of designing, implementing, and applying an entirely new programming language in a single afternoon.

◊bio{◊link["https://lexi-lambda.github.io/"]{Alexis King} works at CJ Affiliate writing primarily Haskell and JavaScript building web applications, APIs, and internal infrastructure. She is especially interested in the intersection of programming languages and usability and how smart tooling and domain specific languages can work together to make systems simpler and easier to reason about by treating programs like user interfaces.}}

◊folded{
◊speaker["" "Jay McCarthy"]{R-r-r-r-REMIX!}
◊;rlink["2015/mccarthy.pdf"]{Slides} ◊;link["https://www.youtube.com/watch?v=BDg79CPbdXQ&list=PLXr4KViVC0qJAsNuDeQzhFDjMK1gEdls8&index=2"]{Video}

Parentheses getting you down? Not enough dots in your life? Can't tell a parenthesis from a brace or bracket? Standard library inconsistency getting you down? #lang remix is the droid you're looking for. It's Racket with delicious, buttery C-like syntax and a new standard library that remixes old forms to increase their Racketosity over 9,000!!!! Come for the dots, stay for the ◊tt{def+}.

◊bio{◊link["https://jeapostrophe.github.io"]{Jay McCarthy} isn't joking when he says the only languages he likes are Racket, Coq, and C. After being desensitized from the flames after the new package system, making controversial programming choices is the only way he can feel anything anymore. Hence, ◊tt{#lang remix}. Also, he is a core Racket developer and associate professor at UMass Lowell.}}



◊folded{
◊speaker["" "Rodrigo Setti"]{Generative Art with Racket}
◊;rlink["2015/mccarthy.pdf"]{Slides} ◊;link["https://www.youtube.com/watch?v=BDg79CPbdXQ&list=PLXr4KViVC0qJAsNuDeQzhFDjMK1gEdls8&index=2"]{Video}

Stamps is a language built on top of Racket, in which one can describe high-level generating rules for 2D shapes, and which takes care of interpreting those rules and rendering. It is easy to create beautiful art that resembles smooth biological forms, as well as recursive fractal-like works. In this talk I introduce my first non-trivial Racket project, and talk about my particular experience developing it.

◊bio{◊link["http://rodrigosetti.com/"]{Rodrigo Setti} is a Brazilian software engineer, he works at Yahoo Search, and before that was working at Evernote (the note taking app). Most of his experience is around information retrieval systems - but his interests are broader, including programming games, creative coding and generative art. He discovered Racket after searching for the perfect Scheme, and has been happily hacking ever since.}}

◊folded{
◊speaker["" "Bruce Steinberg"]{Functional Lighting}
◊;rlink["2015/mccarthy.pdf"]{Slides} ◊;link["https://www.youtube.com/watch?v=BDg79CPbdXQ&list=PLXr4KViVC0qJAsNuDeQzhFDjMK1gEdls8&index=2"]{Video}

I’ve been working on ◊link["https://github.com/brucehs/simple-hue-control-racket"]{an application for controlling Phillips Hue Bulbs in theatrical settings}. As a professional lighting designer specializing in new and experimental work, I often find myself in galleries and other untraditional settings. Rather than bringing in an entire rig (and unhelpfully transforming the space into a theatre), I've used ◊link["http://www2.meethue.com/en-us/"]{Phillip Hue Bulbs} instead. They allow wireless individual control of fixtures, even when they are all powered via a single track or outlet. Using functional logic to determine the most efficient json commands, ◊em{Simple Hue Control} offers a designer a familiar programming interface for creating beautiful shows.

◊bio{◊link["www.BruceSteinbergLD.com"]{Bruce Steinberg}’s lighting designs have been seen in venues ranging from a Soho laundromat to Italian concert halls—and even an occasional theatre. New York: ◊em{The Maids' The Maids} (Kathryn Hamilton), ◊em{Les Mamelles de Tirésias} (Emma Griffin), ◊em{Art of Memory} (Tanya Calamoneri), ◊em{Feeder} (José Zayas), ◊em{Blue Before Morning} (Gia Forakis/NYIT Award). Regional: ◊em{Moby Dick—Rehearsed} (Jonathan Rosenberg), ◊em{Brief Interviews with Hideous Men} (Daniel Fish). International: ◊em{Sounds} (Paola Prestini). Installations: Mark di Suvero's ◊em{For Euler} (1997) and ◊em{Double Tetrahedron} (2004) at Salem Art Works; Keren Cytter's ◊em{Mysterious Serious} (2009) at X-Initiative. Architectural: ◊em{Happy Lucky No. 1 Gallery} (2015). He received his MFA from NYU TSOA, Department of Design for Stage & Film.}}

◊h3{Plus}

◊div[#:class "two-col"]{
◊speaker["" "Linh Chi Nguyen" ""]{}
◊speaker["" "(more speakers to come)" ""]{}
}

◊gap[1]

◊h3{◊xtarget["register"]{Register}}
◊link["https://www.eventbrite.com/e/racketcon-2016-tickets-24349152972"]{Via Eventbrite.}
Use the "RKC" group code when reserving rooms at the ◊link["http://doubletree3.hilton.com/en/hotels/missouri/st-louis-union-station-a-doubletree-by-hilton-hotel-STLUSDT/index.html"]{Union Station hotel}.

◊h3{Sponsors}

◊inline-list['sponsor]{
◊link["airstash.com"]{AirStash}
◊link["pollenpub.com"]{Matthew Butterick}
◊link["robotic.de"]{DLR}
◊link["brian.mastenbrook.net"]{Brian Mastenbrook}
◊link["blog.markshead.com"]{Mark Shead}}

◊h3{Previous RacketCons}

◊(define (conlink year) 
  (link (format "con.racket-lang.org/~a" year) year))

◊inline-list['con]{
◊conlink{2015}
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}
