#lang pollen
◊(require racket/file pollen/cache)



◊head["a"]{Seventh}
◊head["b"]{RacketCon}
◊head["c"]{  Seattle 2017}

◊gap[3]

◊(define this-rc-date "7–8 October 2017")

◊h2{◊xlink["register"]{◊this-rc-date}}
◊h2{◊xlink{Venue}}
◊h2{◊xlink{Speakers}}
◊h2{◊◊xlink{Register}}
◊;{◊h2{◊xlink{Schedule}}}


(seventh RacketCon) is the meeting for everyone interested in ◊link["http://racket-lang.org"]{Racket} — a ◊link["https://docs.racket-lang.org/quick/index.html"]{general-purpose programming language} that's also the ◊link["https://www.ccs.neu.edu/home/matthias/manifesto/"]{world’s first ecosystem} for developing and deploying new languages. 

RacketCon is for developers, contributors, programmers, educators, and bystanders. It's an opportunity for all of us to share plans, ideas, and enthusiasm, and help shape the future of Racket.


◊gap[0]

◊h3{◊xtarget["speakers"]{◊head["a"]{Keynote}}}

◊head["b"]{Dan Friedman}
◊head["b"]{& Will Byrd}

◊gap[1]

◊h3{The Reasoned Racketeers}

◊div{As Racketeers we are familiar with functional programming: each pure function we write produces one output value when given zero or more input values.  What if we erase this distinction between inputs and outputs, and only think about the ◊em{relationships} between these values?  We end up with ◊em{relational} programming, which lets us run our programs backwards: we can infer the "input" values that produce a desired "output" value.  We can also reorder our code arbitrarily, without changing the meaning of our programs.  By writing an interpreter for a subset of Racket using this approach, the interpreter inherits the ability to synthesize Racket programs from example inputs and outputs, among other interesting abilities.

We will demonstrate all of these features using ◊link["http://minikanren.org/"]{miniKanren}, a domain-specific language for constraint logic programming that is itself embedded in Racket.
}

◊bio{

◊img[#:src "dfried.jpg"] ◊link["https://www.cs.indiana.edu/~dfried/"]{Daniel P. Friedman} is Professor of Computer Science at Indiana University.  He is co-author of ◊em{The Little Schemer}, 4th ed., ◊em{The Seasoned Schemer}, ◊em{The Reasoned Schemer}, 2nd ed., ◊em{The Little Prover}, ◊em{Scheme and the Art of Programming}, and ◊em{Essentials of Programming Languages}, 3rd ed., all published by MIT press.

◊img[#:src "webyrd.jpg"] ◊link["http://webyrd.net/"]{William E. Byrd} is a Research Assistant Professor in the School of Computing at the University of Utah.  He is co-author of ◊em{The Reasoned Schemer}, 2nd ed., and runs weekly online hangouts on ◊link["http://minikanren.org/"]{miniKanren} and relational programming.  Will is also interested in the intersection of programming languages and biology.  Ask him about the scanning tunneling microscope (STM) he is building.
}

◊h3{◊xtarget["venue"]{Venue}}

For the first time this year, RacketCon happens in Seattle. We'll be at ◊link["https://goo.gl/maps/ENXQWSbp6Zo"]{Mary Gates Hall} on the University of Washington campus. We thank ◊link["https://homes.cs.washington.edu/~emina/index.html"]{Emina Torlak} and the UW ◊link["https://www.cs.washington.edu/"]{School of Computer Science & Engineering} for hosting us.

◊;iframe[#:src "https://www.google.com/maps/embed?pb=!1m14!1m8!1m3!1d2314.149470473195!2d-122.3093303405857!3d47.656477572249656!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x549014f2ab81c35f%3A0x6dda64382ddc87a4!2sMary+Gates+Hall%2C+1851+NE+Grant+Ln%2C+Seattle%2C+WA+98105!5e0!3m2!1sen!2sus!4v1496964066706" #:width "600" #:height "450" #:frameborder "0" #:style "border:0" #:allowfullscreen "true"]

◊div[#:id "map"]

◊gap[1]


◊h3{◊xtarget["register"]{Register}}

◊schedule[
◊row{◊at{◊span[#:style "font-weight: bolder"]{Early bird}} ◊desc{◊strong{$75 individual · $45 student (until September 16)}}}
◊row{◊at{Standard} ◊desc{$100 individual · $60 student (after September 16)}}
]

Buy your ticket at ◊link["https://www.eventbrite.com/e/racketcon-2017-tickets-34928590354"]{Eventbrite}.

RacketCon attendees also get a fantastic ◊link["https://gc.synxis.com/rez.aspx?Hotel=76675&Chain=10069&arrive=10/6/2017&depart=10/8/2017&adult=1&child=0&group=4AM98Y"]{group rate} at the beautiful & convenient ◊link["http://www.hoteldeca.com/"]{Hotel Deca}.



◊h3{◊xtarget["speakers"]{Speakers}}

◊gap[0.5]

◊div{
◊folded{
◊speaker["" "Leif Andersen"]{Movies as Programs: The Story of a Racket}
Racket is more than a programming language. It is a programming language for making new languages. In fact, it is a programming language for making languages for making movies. Video is a language made from the sweat and parenthesis provided by the Racket ecosystem. It integrates into Racket from concept to final rendering. Come for the video demos, stay form the language tower.

◊bio{◊link["http://leifandersen.net/"]{Leif Andersen} is a PhD student at Northeastern University and a core Racket developer. She is interested in compilers, low-level performance tools, creative and unusual DSLs, and the interactions between programing languages and their ecosystems. Mix these in a pot and Video pops out.}}

◊link["https://www.viewpoint.org/"]{◊speaker["" "Dan Anderson" ""]}

◊folded{
◊speaker["" "David Christiansen"]{A Little Bit of Dependent Types}
You can use the functional programming techniques that you're familiar with from Racket to write mathematical proofs. An upcoming book in the spirit of "The Little Schemer" will demonstrate how to do this, and tell you how to write your own proof system in Racket. This talk is a little taste of #lang pie, where programming and proving meet.

◊bio{◊link["http://davidchristiansen.dk/"]{David Christiansen} is a postdoc at Indiana University. In the past, he wrote most of the metaprogramming features and much of the interactive environment for Idris, a language with dependent types. Together with Dan Friedman, he is a co-author of an upcoming book on dependent types in the tradition of The Little Schemer.}}

◊folded{
◊speaker["" "Deren Dohoda"]{Prototype to Production: A Racket Experience Report}
With the rise in adoption and decrease in price of single-board computers, high-level languages like Racket can find good use in embedded scenarios which are not real-time. Features of the language support all stages of development with few downsides.

◊bio{◊link["http://www.pion-inc.com"]{Deren Dohoda} is the Engineering Manager at Pion Inc and has worked at all phases of product development, deployment, and support for 17 years.}}

◊link["https://charlesearl.blog/"]{◊speaker["" "Charles Earl" ""]}

◊link["https://github.com/jackfirth"]{◊speaker["" "Jack Firth" ""]}

◊link["https://lexi-lambda.github.io/"]{◊speaker["" "Alexis King" ""]}

◊link["https://jeapostrophe.github.io/home/"]{◊speaker["" "Jay McCarthy" ""]}

◊folded{
◊speaker["" "Mangpo Phitchaya Phothilimthana"]{High-Coverage Hint Generation for Racket Programming Assignments}
In massive programming courses, automated hint generation offers the promise of zero-cost, zero-latency assistance for students who are struggling to make progress on solving a program. 

In this talk, I will describe a robust hint generation system for Racket programming assignments that extends the hint coverage of the mutation-based approach using two complementary techniques. A syntax checker detects common syntax misconception errors in individual subexpressions to guide students to partial solutions that can be evaluated for the semantic correctness. A mutation-based approach is then used to generate hints for almost-correct programs. If the mutation-based approach fails, a case analyzer detects missing program branches to guide students to partial solutions with reasonable structures. Our system itself is implemented in Racket. We utilize Rosette, a solver-aided language embedded in Racket, as a constraint solver to prove program equivalence.

After analyzing over 75,000 program submissions and 8,789 hint requests from UC Berkeley’s introductory computer science course, we found that using all three techniques together could offer hints for any program, no matter how far it was from a correct solution. Furthermore, our analysis shows that hints contributed to students’ progress while still encouraging the students to solve problems by themselves.

◊bio{◊link["https://people.eecs.berkeley.edu/~mangpo/www/home.html"]{Phitchaya Mangpo Phothilimthana} is a PhD student from UC Berkeley working with Professor Ras Bodik. Her PhD thesis focuses on designing programming models and using program synthesis to build smart compilers for unconventional hardwares (from an ultra-low-power spatial architecture to programmable network cards). She is also interested in applying program synthesis to other domains, including educational tools.}}

◊folded{
◊speaker["" "Prabhakar Ragde"]{Proust: A Nano Proof Assistant}
Three things that don't normally go together are intuitionistic type theory, Racket as a teaching language, and a required sophomore course in logic and computation. I'll describe how I've been teaching the course by having students code a small proof assistant (Proust) in Racket and using it to do their homework. Its construction proceeds in stages through propositional and predicate logic, going as far as recursive functions on natural numbers before a lateral transition to Coq.

◊bio{◊link["https://cs.uwaterloo.ca/~plragde/"]{Prabhakar Ragde} has been a professor of Computer Science at the University of Waterloo since 1988. He first encountered Racket fifteen years ago when looking for a way to introduce his young daughters to CS. Now more than two thousand students a year take Waterloo courses that use Racket to introduce them to CS, and he can't go a day without using a lambda.}}

◊folded{
◊speaker["" "David Storrs"]{Racket and Business}
Racket is the official language at Biomantica Inc, the biotech startup I co-founded. I had a little experience with it in advance and my cofounder learned it from scratch as we worked. In this talk we'll go over why we chose it, the pros and cons we've found, the process of learning Racket on the job, and thoughts on how it worked out in practice.

◊bio{◊link["http://biomantica.com"]{David Storrs} started programming professionally when Netscape and IE were duking it out for market share. He's done a lot of web+devops work, and at least a little of most other types of programming -- financial software, medical software, video games, etc. He's co-founder of Biomantica Inc, a biotech software company whose initial product focuses on big data transport between scientific or corporate collaborators who may be continents apart.}}

◊folded{
◊speaker["" "Vishesh Yadav"]{RacketScript}
Want to embrace the power of Racket in the ugly JavaScript world? RacketScript can compile Racket (ok! not all of it) to JavaScript. I'll will demonstrate how the RacketScript compiler works, currently implemented features of Racket, and how it differs from Racket and other Racket to JavaScript compilers.

◊bio{◊link["https://www.twistedplane.com/"]{Vishesh Yadav} is a Software Engineer at Nutanix where he works on distributed storage systems. He started hacking in Racket while pursuing M.S. in Computer Science from Northeastern University, where he also got interested in programming languages.}}

}



◊h3{◊xtarget["schedule"]{Saturday Oct 7}}

Keynote and talks. Schedule to come!

◊;{
◊schedule[
◊row{◊at{9:00–9:30} ◊desc{Registration}}
◊row{◊at{9:30} ◊desc{Welcome}}
◊row{◊at{9:30–10:30} ◊desc{Keynote: Dan Friedman & Will Byrd}}
◊row{◊at{10:30–11:00} ◊desc{Break}}
◊row{◊at{11:00–12:00} ◊desc{
Speaker 1
Speaker 2
Speaker 3}}
◊row{◊at{12:00–14:00} ◊desc{Lunch}}
◊row{◊at{14:00} ◊desc{Contributor Awards}}
◊row{◊at{14:00–15:00} ◊desc{
Speaker 4
Speaker 5
Speaker 6}}
◊row{◊at{15:00–15:15} ◊desc{Break}}
◊row{◊at{15:15–16:15} ◊desc{
Speaker 7
Speaker 8
Speaker 9}}
◊row{◊at{16:15–16:30} ◊desc{Break}}
◊row{◊at{16:30–17:30} ◊desc{
Speaker 10
Speaker 11
Speaker 12}}
◊row{◊at{17:30–17:45} ◊desc{Break}}
◊row{◊at{17:45–} ◊desc{Town Hall Meeting / General Q&A}}
]
}



◊h3{Saturday evening}

◊schedule[
◊row{◊at{19:00–23:00} ◊desc{Food & drink at local brewpub ◊link["http://www.elysianbrewing.com/locations/elysian-fields/"]{Elysian Fields}}}
]



◊h3{Sunday Oct 8}

New for (seventh RacketCon) — Racketeer Office Hours, a day devoted to collaborating on existing projects, planning new ones, and getting help and advice from other members of the community. Racket's core developers & many others will be there — bring your questions & projects!


◊div[#:class "two-col"]{
◊link["http://www.cs.utah.edu/~mflatt/"]{Matthew Flatt} (Racket core)
◊link["https://www.eecs.northwestern.edu/~robby/"]{Robby Findler} (Racket core, DrRacket)
◊link["http://www.ccs.neu.edu/home/samth/"]{Sam Tobin-Hochstadt} (Racket core, Typed Racket)
◊link["https://jeapostrophe.github.io/home/"]{Jay McCarthy} (Racket core, web)
◊link["http://users.eecs.northwestern.edu/~stamourv/"]{Vincent St-Amour} (Typed Racket, optimizer, Racket releases)
◊link["http://beautifulracket.com"]{Matthew Butterick} (Pollen)
}

◊;{
◊schedule[
◊row{◊at{9:00–9:30} ◊desc{Registration}}
◊row{◊at{9:30} ◊desc{Welcome}}
◊row{◊at{9:30–10:30} ◊desc{Keynote: Emina Torlak: Synthesis and Verification for All}}
]
}

◊h3{Sponsors}

◊inline-list['sponsor]{
◊link["http://blackswanlearning.com/"]{Black Swan Learning Corp}
◊link["https://www.comcasttechnologysolutions.com/"]{◊img[#:src "comcast-technology-solutions-vrt-black-with-transparent-background.png" #:style "width:25%;"]{}}
◊link["https://github.com/david-vanderson/"]{David Vanderson}
◊link["pollenpub.com"]{Matthew Butterick}
}

◊gap[1]


Sponsorship slots are still available! If you or your company would like to sponsor RacketCon, please ◊a[#:href "mailto:con@racket-lang.org"]{get in touch}.


◊h3{Friendly Environment Policy}

The Racket community aims to improve the world through programming. It started with the goal of introducing everyone to the wonderful world of program design, with a spirit of full inclusion and no emphasis on any specific group.  Over time it has grown into a full-fledged professional community with a well-known reputation for helpfulness and openness on its on-line communication channels. The organizers want to encourage an equally open exchange of ideas at RacketCon, the community's in-person meet-up.

This spirit requires an environment that enables all to participate without fear of personal harassment. We define harassment as unwelcome or hostile behavior, that is, behavior that focuses on people instead of ideas. ◊link["http://www.acm.org/sigs/volunteer_resources/officers_manual/anti-harassment-policy"]{The ACM's anti-harassment policy} lists a variety of specific unacceptable factors and behaviors. The organizers consider responses such as “just joking,” or “teasing,” or being “playful” as unacceptable.

Anyone witnessing or subject to unacceptable behavior should notify one of the RacketCon organizers (Matthew Flatt, Vincent St-Amour).

If a participant engages in harassing behavior, the conference organizers may take any action they deem appropriate, from a warning of the offender to an expulsion from the conference [without refund].

[The wording of this policy is directly derived from ◊link["http://snapl.org/2015/policy.html"]{that of the SNAPL conference}, with thanks.]


◊h3{Previous RacketCons}

◊(define (conlink year) 
  (link (format "con.racket-lang.org/~a" year) year))

◊inline-list['con]{
◊conlink{2016}
◊conlink{2015}
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}


◊gap[2]

◊em{"Everyone has a universe of beautiful things in their head. Maintaining a nurturing environment for conflicting interests is important. And Racket has it. So if you worry that you do weird and insignificant stuff, I tell you that the world has taught you wrong and Racket is your refugee shelter. Please do not hesitate."}

— Satisfied Customer, RacketCon 2016

