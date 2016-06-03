#lang pollen
◊(require racket/file)

◊h1{(sixth RacketCon)}
◊(define rc-2016-date "18 Sept 2016")
◊h2{◊xlink["venue"]{◊rc-2016-date}}
◊h2{◊xlink["venue"]{St. Louis}}
◊h2{◊xlink{Speakers}}
◊h2{◊xlink{Register}}

◊; ◊div['class: "codebox"]{◊div['class: "opacity-control"]{◊(file->string "eero-demo.rkt")}}

◊h3{◊xtarget["venue"]{◊rc-2016-date at the St. Louis Union Station hotel (the day after ◊link["http://thestrangeloop.com"]{Strange Loop})}}

RacketCon is a public meeting for everyone interested in Racket: developers, contributors, programmers, educators, and bystanders. It's an opportunity for all members of the community to come together to share plans, ideas, and enthusiasm. RacketCon will enable the entire Racket community to mingle: to update each other, to exchange ideas, to collaborate, and to help shape the future of Racket.

◊h3{◊xtarget["speakers"]{Keynote speaker}}
◊folded{
◊keynote-speaker["" "Emina Torlak"]{Synthesis and Verification for All}

Rosette is a programming language for creating new programming tools. It extends Racket with a few constructs that make it easy to build advanced tools for program verification and synthesis. Building these tools usually takes months or years of work, as well as expertise in many fields, from formal methods to programming languages to software engineering. With Rosette, creating such a tool is as easy as defining a new domain-specific language in Racket. Once you define your language, you get the tools for (almost) free. This talk will provide a brief introduction to Rosette, concluding with a whirlwind tour of recent applications to finding bugs in radiotherapy software, generating efficent code for ultra low-power hardware, and creating custom tutors for K-12 algebra.

◊bio{◊link["https://homes.cs.washington.edu/~emina/index.html"]{Emina Torlak}  is an assistant professor at the University of Washington. She works on computer-aided design, verification, and synthesis of software. Emina is the creator of Rosette, a new Racket-based language that makes it easy to build efficient tools for verifying and synthesizing all kinds of programs, from radiotherapy controllers to automated algebra tutors.}}

◊h3{◊xtarget["speakers"]{Confirmed Speakers}}
◊speaker["" "Byron Davies" ""]{}
◊speaker["" "Jack Firth" ""]{}
◊speaker["" "Alexis King" ""]{}
◊speaker["" "Jay McCarthy" ""]{}
◊speaker["" "Linh Chi Nguyen" ""]{}
◊speaker["" "Rodrigo Setti" ""]{}
◊speaker["" "Bruce Steinberg" ""]{}

◊h3{Sponsors}

◊inline-list['sponsor]{
◊link["pollenpub.com"]{Matthew Butterick}
◊link["blog.markshead.com"]{Mark Shead}
◊link["robotic.de"]{DLR}
◊link["wearable.com"]{Wearable}}

◊h3{Previous RacketCons}

◊(define (conlink year) 
  (link (format "con.racket-lang.org/~a" year) year))

◊inline-list['con]{
◊conlink{2015}
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}
