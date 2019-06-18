#lang pollen

◊string->svg{ninth 
RacketCon}

◊(define this-rc-date "13–14 July 2019")

◊div[#:class "menu-container"]{
◊h2{◊xlink["register"]{◊this-rc-date}}
◊h2{◊xlink{Venue}}
◊h2{◊xlink{Speakers}}
◊h2{◊xlink{Register}}
◊h2{◊xlink{Schedule}}
}

◊gap[1]


(ninth RacketCon) is the meeting for everyone interested in ◊link["https://racket-lang.org"]{Racket} — a ◊link["https://docs.racket-lang.org/quick/index.html"]{general-purpose programming language} that's also the ◊link["https://felleisen.org/matthias/manifesto/"]{world’s first ecosystem} for language-oriented programming. 

RacketCon is for developers, contributors, programmers, educators, and bystanders. It's an opportunity for all of us to share plans, ideas, and enthusiasm, and help shape the future of Racket.

◊(define heading-width 16)

◊string->svg[#:width heading-width]{>(keynote)}
◊string->svg{Aaron Turon}
◊string->svg{Governing Rust}

◊link["http://aturon.github.io/"]{Aaron Turon} is a Research Engineer on the Rust team at Mozilla. He received his PhD from Northeastern University, where he studied programming-language design, program verification, and low-level concurrency. His dissertation was awarded the SIGPLAN John C. Reynolds Doctoral Dissertation Award in 2014. After his PhD studies, he continued his research in concurrency verification and programming techniques as a postdoc at MPI-SWS.

◊gap[1]


◊h3{◊xtarget["venue"]{◊string->svg[#:width heading-width]{>(venue)}}}

Two firsts this year: 

1) RacketCon is located in Salt Lake City, Utah. We'll be at the Tessman Auditorium at ◊link["https://www.slcpl.org/"]{The City Library} main branch at 210 East 400 South.

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


◊h3{◊xtarget["register"]{◊string->svg[#:width heading-width]{>(housing)}}}

For those attending RacketCon only, a limited block of ◊link["https://reservations.travelclick.com/4650?groupID=2537449#/guestsandrooms"]{discounted rooms} is available from the ◊link["https://saltlake.littleamerica.com/"]{Little America} hotel. ◊link["https://reservations.travelclick.com/4650?groupID=2537449#/guestsandrooms"]{Click here to reserve.}

For those also attending ◊link["https://school.racket-lang.org"]{Racket School}, subsidized housing for both events is available in the University of Utah campus dorms. The cost is $40/night. ◊link["https://forms.gle/spaMYg1LL4CK3J8p9"]{Register here.}



◊gap[1]

◊h3{◊xtarget["speakers"]{◊string->svg[#:width heading-width]{>(speakers)}}}

◊div{

◊folded{
◊speaker["" "Phil Hagelberg"]{In Production: creating physical objects with Racket}

As the maker movement has taken off, fabrication procedures have become more and more approachable to the enterprising hacker. We will cover how Racket was used in the creation of the Atreus DIY ergonomic keyboard kit in two of the key design elements: the circuit board and the laser-cut enclosure, as well as details about how these techniques can be applied to other projects.

◊bio{◊strong{Phil Hagelberg} (aka ◊link["https://github.com/technomancy"]{technomancy}) has been using Lisp dialects since he first discovered Emacs in school. He writes Clojure at work, uses Racket for producing DIY keyboards, and is a lead developer on the Fennel compiler.}
}

◊folded{
◊speaker["" "Gershon Mathew Wolfe"]{A Mental Model for Algorithmic Music Composition}

I have put together a melody-generating system in Racket called arsNova and arsMosis. My workflow is based on the theory that the human brain enjoys listening to mid-complex melodies, as opposed to very simple or overly complex patterns. My system uses a combination of cellular automata, entropy, musical set theory, and interval class vectors to deduce the most appealing melodies.

◊bio{◊strong{Gershon Mathew Wolfe} has a background in statistical physics and has been developing and prototyping software algorithms for the past 29 years in the fields of bioinformatics, machine learning in medical diagnostics, decision making processes in defense, and algorithmic music composition.  Worked at Nalorac, Incyte Pharmaceuticals, Large Scale Biology, Advanced Ideas in Medicine, contract positions, SoSACorp, and currently at Algorhythms LLC.  Gershon has a B.S. in chemistry from UCLA, a Ph.D. in chemical physics from the University of Washington, and a postdoctoral fellowship  from UCSF.}
}

◊folded{
◊speaker["" "Bradley M. Kuhn"]{Conservancy and Racket: What We Can Do Together!}

◊bio{◊strong{Bradley M. Kuhn} is the Distinguished Technologist at Software Freedom Conservancy, and editor-in-chief of copyleft.org.  Kuhn began his work in the software freedom movement as a volunteer in 1992, as an early adopter of GNU/Linux, and contributor to various Free Software projects.  Kuhn's non-profit career began in 2000 at FSF. As FSF's Executive Director from 2001-2005, Kuhn led FSF's GPL enforcement, launched its Associate Member program, and invented the Affero GPL. Kuhn was appointed President of Conservancy in April 2006, was Conservancy's primary volunteer from 2006-2010, and has been a full-time staffer since early 2011. Kuhn holds a summa cum laude B.S. in Computer Science from Loyola University in Maryland, and an M.S. in Computer Science from the University of Cincinnati. Kuhn received an O'Reilly Open Source Award, in recognition for his lifelong policy work on copyleft licensing.}
}

◊folded{
◊speaker["" "Greg Hendershott"]{◊span{Racket and Emacs: Fight! 
(in which we spend 5 more years herding cats)}}

Racket Mode started in 2012 as some awkward Emacs Lisp to send XREPL commands to command-line Racket running in an M-x shell buffer. The official repo's initial commit 804b3c6 was in January 2013. After nearly six years and 900 commits (37,612 insertions and 19,661 deletions) it has evolved. The presentation includes live demonstration, discussion of design considerations including interprocess communication and security, and possible future directions. A user's guide and reference ◊link["https://racket-mode.com"]{is available here}.


◊bio{Regardless of how they may feel about each other, ◊strong{Greg Hendershott} loves both Racket and Emacs. He first showed racket-mode at RacketCon 2014. He founded Cakewalk, Inc. and Extramaze LLC. Through the latter he is sometimes available to consult on Racket projects.}
}


◊folded{
◊speaker["" "Eric Griffis"]{Algebraic Racket in Action}

Algebraic Racket is a fresh take on functional meta-programming with Racket. Its transparent data structures and pattern-based de-structuring syntax makes a broad range of functional programming idioms easier to read and write. Come see how I use Algebraic Racket to get in the zone faster, ride that flow longer, and exploit Racket's proclivity for language-oriented programming to get more done with less effort.

◊bio{◊strong{Eric Griffis} is an intuitive meta-programmer with an eye for composition, recursion, and self-similarity. A consummate bottom-up thinker, he enjoys playing with tools and techniques for developing software that creates and interacts with other software-producing software.}
}

◊folded{
◊speaker["" "Vlad Kozin"]{#lang wishful thinking (will! it be so)}

What might solving a real production problem in a language specifically designed for building languages look like? Let's get a glimpse by implementing FastCGI in Racket — a simple protocol that'll take us all the way from bit twiddling to Web frameworks. We won't tie ourselves to the defaults that Racket designers blessed us with, but boldly employ wishful thinking, borrow readily from other languages. You want prototypes with Lua-style metatables? Concise syntax? Single and multiple inheritance with generic dispatch? How generic is generic? Multimethods? Full Metaobject Protocol? Beyond Metaobject Protocol? A language of bit patterns? The one true Web framework? Build yourself a better language. Dare say: I wish I could — then make it so.

◊bio{◊strong{Vlad Kozin} is a dilettante programmer from London who taught himself programming through HtDP and PLAI, did some paid Javascript, which he does not recommend, then paid Clojure, which he does. He has now gone back to the roots. Former @yandex and @droitfintech. Fall'13 @recursecenter aka @hackerschool alum.}
}

◊folded{
◊speaker["" "Andrew Blinn"]{Fructure: A Structured Editing Engine in Racket}

Fructure is a prototype for an extensible structured editing engine. In structured editing, core edit actions reflect the extended selves of the objects you're bringing to life (grammatical programs with semantic properties), as opposed to the shape of their serialization (text files). Editing abstractions like cursors and code-folding are reimagined as syntactic scaffolding in a meta-grammar of syntactic affordances. Fructure lets language providers specify syntactic forms and semantic refactorings as production and transformation rules in a tiny term-rewriting DSL. Lessening reliance on after-the-fact correction or ad-hoc autocompletion, program creation consists rather of a guided search in the space of valid programs. Over the hood, my emphasis is on visual and kinetic appeal; creating a fun, fluid editing process with an eye to discoverability. 

◊bio{◊strong{Andrew Blinn} bounced off programming early in life, finding it unbearably fiddly in some ways and not fiddly enough in others. Two years ago and mostly through a BSc in math, he accidentally took a Racket-based PL course and immediately pivoted to CS. At the moment he is defrosting a dormant interest in visual design and interaction, hoping to further tweak programming's fiddliness attribute.}
}

◊folded{
◊speaker["" "John Clements"]{Computational models and one big problem with the stepper}

DrRacket's stepper shows the evaluation of a beginning student language (BSL) program as a sequence of steps in an algebraic reduction semantics.  But ... should it?  

We propose a simple change to the stepper's interface which weakens its connection to the standard-format small-step formal semantics it has historically sought to adhere to, but which strengthens its connection to the notion of semantics that I believe that most working programmers actually use.  

It would be awesome to actually try to validate this change. Um, maybe next year.

◊bio{◊strong{John Clements} is Arrogant, Self-Centered, and Lazy. Also Intelligent, Honest, and Caring. Also, he’s a Professor at Cal Poly State University, and the author of DrRacket’s Stepper and many other things that can be entertainingly completed in a weekend, including this bio. Ask him about Bread,  Knitting, Speedcubing, or Bicycling. Or the difficulty of wedging models of computation into CS 1.}
}
}

◊gap[1]

◊h3{◊xtarget["schedule"]{◊string->svg[#:width (* heading-width 1.4)]{>(schedule 'saturday)}}}

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
◊row{◊at{18:00–22:00} ◊desc{◊em{food & drink at ◊link["https://squatters.com"]{Squatters} 
147 W Broadway (300 S)}}}
]


◊h3{◊string->svg[#:width (* heading-width 1.4)]{>(schedule 'sunday)}}

◊grid[#:id "schedule"
◊row{◊at{09:00–09:25} ◊desc{◊em{breakfast}}}
◊row{◊at{09:25–09:30} ◊desc{Welcome}}
◊row{◊at{09:30–10:30} ◊desc{Matthew Flatt: ◊em{State of Racket}}}
◊row{◊at{10:30–10:50} ◊desc{◊em{break}}}
◊row{◊at{10:50–11:05} ◊desc{Interlude}}
◊row{◊at{11:05–11:50} ◊desc{
Tutorials (multiple tracks)
◊strong{GameJam with Jay McCarthy}: Explore the libraries that are available for building games and graphics with Racket.
}}
◊row{◊at{11:50–13:20} ◊desc{◊em{lunch (provided)}}}
◊row{◊at{13:20–16:00} ◊desc{
Hackathon (multiple tracks)
◊strong{LangJam}: Make an experimental language.
◊strong{GameJam}: Build a simple game.
◊strong{Data science}: Improve the data-science libraries.
}}
◊row{◊at{16:00–16:20} ◊desc{LangJam demos}}
◊row{◊at{16:20–16:40} ◊desc{GameJam demos}}
◊row{◊at{16:40–17:00} ◊desc{Data science demos}}
]


◊h3{◊xtarget["sponsors"]{◊string->svg[#:width (* heading-width 1.5)]{>(sponsors)}}}

◊inline-list['div]{
◊link["//lisp.sh"]{Jesse Alama}
}

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
