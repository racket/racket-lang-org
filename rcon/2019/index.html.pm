#lang pollen

◊string->svg{ninth 
RacketCon}

◊(define this-rc-date "13–14 July 2019")

◊div[#:class "menu-container"]{
◊h2{◊xlink["register"]{◊this-rc-date}}
◊h2{◊xlink{Venue}}
◊h2{◊xlink{Speakers}}
◊h2{◊◊xlink{Register}}
◊h2{◊xlink{Schedule}}
}

◊gap[1]


(ninth RacketCon) is the meeting for everyone interested in ◊link["https://racket-lang.org"]{Racket} — a ◊link["https://docs.racket-lang.org/quick/index.html"]{general-purpose programming language} that's also the ◊link["https://felleisen.org/matthias/manifesto/"]{world’s first ecosystem} for language-oriented programming. 

RacketCon is for developers, contributors, programmers, educators, and bystanders. It's an opportunity for all of us to share plans, ideas, and enthusiasm, and help shape the future of Racket.

◊(define heading-width 16)

◊string->svg[#:width heading-width]{>(keynote)}
◊string->svg{Aaron Turon}

◊gap[1]


◊h3{◊xtarget["venue"]{◊string->svg[#:width heading-width]{>(venue)}}}

Two firsts this year: 

1) RacketCon is located in Salt Lake City, Utah. We'll be at the Tessman Auditorium at ◊link["https://www.slcpl.org/"]{The City Library}. 

2) RacketCon happens right after Racket School. All Racket School tickets also include admission to RacketCon. For more info, visit the ◊link["https://school.racket-lang.org/"]{Racket School web page}.

◊gap[1]

◊h3{◊xtarget["register"]{◊string->svg[#:width heading-width]{>(register)}}}

◊grid[#:id "register"
◊row{◊at{RacketCon only (2 days)} ◊desc{$69 base
$99 patron
$49 academic}}

◊row{◊at{◊link["https://school.racket-lang.org/#brw"]{Beautiful Racket workshop} + RacketCon (5 days total)} ◊desc{$299 base
$499 patron
$149 academic}}

◊row{◊at{◊link["https://school.racket-lang.org/#htdl"]{How to Design Languages} + RacketCon (7 days total)} ◊desc{$499 base
$699 patron
$249 academic}}
]

◊button[#:id "eventbrite-widget-modal-trigger-55663521090" #:type "button" #:class "buy-button"]{Buy ticket at Eventbrite}

◊strong{Base} tickets are available to all. 

◊strong{Patron} tickets cover our full cost of offering these events, plus a little extra to help support Racket development at large. Racket is part of the ◊link["https://sfconservancy.org/"]{Software Freedom Conservancy}.

◊strong{Academic} tickets are available to participants from academic institutions that need a subsidy. If this subsidized rate is still not low enough, we are offering a limited number of scholarships. Please ◊link["https://goo.gl/forms/SIxIJADVZVxzq1nf2"]{apply here}.

Eventbrite fees are added during checkout.

◊gap[1]


◊h3{◊xtarget["speakers"]{◊string->svg[#:width heading-width]{>(speakers)}}}

◊div{

◊folded{
◊speaker["" "Phil Hagelberg"]{In Production: creating physical objects with Racket}

◊bio{◊strong{Phil Hagelberg} (aka technomancy) has been using Lisp dialects since he first discovered Emacs in school. He writes Clojure at work, uses Racket for producing DIY keyboards, and is a lead developer on the Fennel compiler.}
}

◊folded{
◊speaker["" "Gershon Mathew Wolfe"]{A Mental Model for Algorithmic Music Composition}

◊bio{◊strong{Gershon Mathew Wolfe} has a background in statistical physics and has been developing and prototyping software algorithms for the past 29 years in the fields of bioinformatics, machine learning in medical diagnostics, decision making processes in defense, and algorithmic music composition.  Worked at Nalorac, Incyte Pharmaceuticals, Large Scale Biology, Advanced Ideas in Medicine, contract positions, SoSACorp, and currently at Algorhythms LLC.  Gershon has a B.S. in chemistry from UCLA, a Ph.D. in chemical physics from the University of Washington, and a postdoctoral fellowship  from UCSF.}
}

◊folded{
◊speaker["" "Bradley M. Kuhn"]{Conservancy and Racket: What We Can Do Together!}

◊bio{◊strong{Bradley M. Kuhn} is the Distinguished Technologist at Software Freedom Conservancy, and editor-in-chief of copyleft.org.  Kuhn began his work in the software freedom movement as a volunteer in 1992, as an early adopter of GNU/Linux, and contributor to various Free Software projects.  Kuhn's non-profit career began in 2000 at FSF. As FSF's Executive Director from 2001-2005, Kuhn led FSF's GPL enforcement, launched its Associate Member program, and invented the Affero GPL. Kuhn was appointed President of Conservancy in April 2006, was Conservancy's primary volunteer from 2006-2010, and has been a full-time staffer since early 2011. Kuhn holds a summa cum laude B.S. in Computer Science from Loyola University in Maryland, and an M.S. in Computer Science from the University of Cincinnati. Kuhn received an O'Reilly Open Source Award, in recognition for his lifelong policy work on copyleft licensing.}
}

◊folded{
◊speaker["" "Greg Hendershott"]{Racket and Emacs: Fight! (in which we spend 5 more years herding cats)}

◊bio{Regardless of how they may feel about each other, ◊strong{Greg Hendershott} loves both Racket and Emacs. He first showed racket-mode at RacketCon 2014. He founded Cakewalk, Inc. and Extramaze LLC. Through the latter he is sometimes available to consult on Racket projects.}
}


◊folded{
◊speaker["" "Eric Griffis"]{Algebraic Racket in Action}

◊bio{◊strong{Eric Griffis} is an intuitive meta-programmer with an eye for composition, recursion, and self-similarity. A consummate bottom-up thinker, he enjoys playing with tools and techniques for developing software that creates and interacts with other software-producing software.}
}

◊folded{
◊speaker["" "Vlad Kozin"]{#lang wishful thinking (will! it be so)}

◊bio{◊strong{Vlad Kozin} is a dilettante programmer from London who taught himself programming through HtDP and PLAI, did some paid Javascript, which he does not recommend, then paid Clojure, which he does. He has now gone back to the roots. Former @yandex and @droitfintech. Fall'13 @recursecenter aka @hackerschool alum.}
}

◊folded{
◊speaker["" "Andrew Blinn"]{Fructure: A Structured Editing Engine in Racket}

◊bio{◊strong{Andrew Blinn} bounced off programming early in life, finding it unbearably fiddly in some ways and not fiddly enough in others. Two years ago and mostly through a BSc in math, he accidentally took a Racket-based PL course and immediately pivoted to CS. At the moment he is defrosting a dormant interest in visual design and interaction, hoping to further tweak programming's fiddliness attribute.}
}

◊folded{
◊speaker["" "John Clements"]{… many small steps backward for mankind.}

◊bio{◊strong{John Clements} is Arrogant, Self-Centered, and Lazy. Also Intelligent, Honest, and Caring. Also, he’s a Professor at Cal Poly State University, and the author of DrRacket’s Stepper and many other things that can be entertainingly completed in a weekend, including this bio. Ask him about Bread,  Knitting, Speedcubing, or Bicycling. Or the difficulty of wedging models of computation into CS 1.}
}
}

◊gap[1]

◊h3{◊xtarget["schedule"]{◊string->svg[#:width heading-width]{>(schedule)}}}

◊grid[#:id "schedule"
◊row{◊at{09:00–09:25} ◊desc{◊em{breakfast & registration}}}
◊row{◊at{09:25–09:30} ◊desc{Welcome}}
◊row{◊at{09:30–10:30} ◊desc{Aaron Turon}}
◊row{◊at{10:30–10:50} ◊desc{◊em{break}}}
◊row{◊at{10:50–11:50} ◊desc{
Phil Hagelberg
Gershon Mathew Wolfe}}
◊row{◊at{11:50–14:00} ◊desc{◊em{lunch (provided)}}}
◊row{◊at{14:00–15:00} ◊desc{
Bradley M. Kuhn
Greg Hendershott}}
◊row{◊at{15:00–15:20} ◊desc{◊em{break}}}
◊row{◊at{15:20–16:20} ◊desc{
Eric Griffis
Vlad Kozin}}
◊row{◊at{16:20–16:40} ◊desc{◊em{break}}}
◊row{◊at{16:40–17:40} ◊desc{
Andrew Blinn
John Clements
}}
]


◊gap[1]

◊h3{◊xtarget["previous"]{◊string->svg[#:width (* heading-width 1.5)]{>(previous RacketCons)}}}

◊(define (conlink year) 
  (link (format "https://con.racket-lang.org/~a" year) year))

◊inline-list['div]{
◊conlink{2018}
◊conlink{2017}
◊conlink{2016}
◊conlink{2015}
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}

◊gap[1]

◊em{My dominant feeling at RacketCon was: “wait hold on are you trying to tell me you people been here doing this the whole time?”} 

Satisfied Customer, RacketCon 2018