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
◊h2{◊xlink{Schedule}}


(seventh RacketCon) is the meeting for everyone interested in ◊link["http://racket-lang.org"]{Racket} — a ◊link["https://docs.racket-lang.org/quick/index.html"]{general-purpose programming language} that's also the ◊link["https://www.ccs.neu.edu/home/matthias/manifesto/"]{world’s first ecosystem} for developing and deploying new languages. 

RacketCon is for developers, contributors, programmers, educators, and bystanders. It's an opportunity for all of us to share plans, ideas, and enthusiasm, and help shape the future of Racket.


◊gap[1]

◊h3{Live Stream}

◊link["https://www.youtube.com/watch?v=et0C4ijed04"]{Morning Live Stream Archive}
◊link["https://www.youtube.com/watch?v=WI8uA4KjQJk"]{Afternoon Live Stream Archive}
Individual talk videos coming soon!

◊gap[2]

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
◊rlink["2017/andersen.pdf"]{Slides} ◊; TODO remove the 2017 once this page goes in that directory

Racket is more than a programming language. It is a programming language for making new languages. In fact, it is a programming language for making languages for making movies. Video is a language made from the sweat and parenthesis provided by the Racket ecosystem. It integrates into Racket from concept to final rendering. Come for the video demos, stay for the language tower.

◊bio{◊link["http://leifandersen.net/"]{Leif Andersen} is a PhD student at Northeastern University and a core Racket developer. She is interested in compilers, low-level performance tools, creative and unusual DSLs, and the interactions between programming languages and their ecosystems. Mix these in a pot and Video pops out.}}

◊folded{
◊speaker["" "Dan Anderson and Anthony Pineci"]{A Methodology for Teaching Kalman Filtering to High School students}
The universe library of Dr Racket is interesting in its use of state and the contracts of the functions of the main loop (big-bang) are particularily apropos in the field of robotics. In this talk we will present a layered approach for teaching high school students about Kalman filtering using the universe teachpack. The approach develops the functions for one-dimensional movement both for exact motion and inexact motion. One-dimensional particle systems are introduced and then the model is extended for the full Kalman filtering algorithms using matrices. In the process, students learn how to develop physical models for their objects using standard trignometric and algebraic ideas. Models are extended to include two-dimensional models.

◊bio{◊link["https://www.viewpoint.org/"]{Dan Anderson} teaches Middle School and Upper School computer science courses at Viewpoint School. Mr. Anderson received his B.S. in Physics from U.C.L.A. and his M.S. in Building Systems (Civil Engineering) from Colorado University at Boulder. Before he started teaching he was a photovoltaic manufacturing engineer for Solec International.

His teaching methodology uses programming languages to communicate increasingly abstract concepts to children from 7th grade through 12th grade. His courses are designed to substantially augment a student’s comprehension of topics in mathematics and science. He uses the HTDP curriculum for his first year of instruction and he has added material to teach Virtual Reality and Artificial Intelligence courses for students on top of that foundation.

◊link["https://www.viewpoint.org/"]{Anthony Pineci} is a high school student in the 12th grade at Viewpoint School. He has been a student of Dan Anderson since 7th grade. He has experience in machine learning, writing the code for a neural network and using it to predict the outcome of the March Madness basketball bracket. He also has gained knowledge about kalman filtering and particle filters in implementing algorithms for self-driving cars, and gained experience teaching when helping other students write the same code.}}

◊folded{
◊speaker["" "David Christiansen"]{A Little Bit of Dependent Types}
You can use the functional programming techniques that you're familiar with from Racket to write mathematical proofs. An upcoming book in the spirit of "The Little Schemer" will demonstrate how to do this, and tell you how to write your own proof system in Racket. This talk is a little taste of #lang pie, where programming and proving meet.

◊bio{◊link["http://davidchristiansen.dk/"]{David Christiansen} is a postdoc at Indiana University. In the past, he wrote most of the metaprogramming features and much of the interactive environment for Idris, a language with dependent types. Together with Dan Friedman, he is a co-author of an upcoming book on dependent types in the tradition of The Little Schemer.}}

◊folded{
◊speaker["" "Deren Dohoda"]{Prototype to Production: A Racket Experience Report}
With the rise in adoption and decrease in price of single-board computers, high-level languages like Racket can find good use in embedded scenarios which are not real-time. Features of the language support all stages of development with few downsides.

◊bio{◊link["http://www.pion-inc.com"]{Deren Dohoda} is the Engineering Manager at Pion Inc and has worked at all phases of product development, deployment, and support for 17 years.}}

◊folded{
◊speaker["" "Charles Earl"]{Deep Learning with Racket: An Experience Report}
◊rlink["2017/earl.pdf"]{Slides}

What can we gain by looking at deep learning through the lens of functional programming? This question has gained attention over the last few years as increasingly complex neural network architectures are developed. I recently started developing a framework for specifying and training neural networks in Racket. In this talk, I'll give a report on the project and invite discussion on future work.

◊bio{◊link["https://charlesearl.blog/"]{Charles Earl} has enjoyed programming in Lisp since the '80s. He now works as a data scientist at Automattic -- the parent company of WordPress -- building systems that use machine learning and natural language processing.}}

◊folded{
◊speaker["" "Jack Firth"]{A RackUnit Toolkit: Growing Racket's Testing Ecosystem}
◊rlink["2017/firth.pdf"]{Slides}

Testing in Racket can be downright joyful. Submodules and RackUnit make simple unit tests easy and quick to write, even in an unfamiliar codebase. But Racket's testing story has more than a few holes when it comes to more complex tests. This talk discusses the current state of Racket's testing ecosystem and presents a toolkit of packages to improve it. The toolkit packages add support for unit testing side effects with mocks, integration testing with fixtures, and abstracting over tests with composable first-class assertions. The packages are for the most part independent but designed to work together, and are built on top of a few general purpose libraries that can be used for non-testing purposes. There's far more work to be done however, and this talk highlights what other forms of testing aspiring Racketeers ought to write packages for so I don't have to.
◊bio{◊link["https://github.com/jackfirth"]{Jack Firth} is a software engineer at Google, primarily working on Google's continuous integration and testing infrastructure and platforms. His interests include domain specific languages, distributed systems, and figuring out how to stop software from breaking all the time. He also likes punching things.}}

◊folded{
◊speaker["" "Andrew Gwozdziewycz"]{Simplifying Slideshow for DWIM Presentations that Stick, Quick}
◊rlink["2017/gwozdziewycz.pdf"]{Slides} ◊link["https://github.com/apg/racket-con-2017"]{Source}

Slideshow is a programming language for producing presentations. It's extremely powerful, and has great tooling, but it's anything but simple, even for experienced Racketeers. I present "slideshow/simple", a language inspired by markdown and a tool called "sent" that simplifies the creation of DWIM presentations, for when you just need to put something on screen without a lot of fuss.

◊bio{◊link["http://apgwoz.com/"]{Andrew Gwozdziewycz} is an engineer at Heroku focused on metrics and operations for customers. Since his coworkers think Racket "has too many parentheses", or is "too complicated", or just flat out say, "huh?", he's forced to use much lesser languages--mostly Go--which he's definitely not a fan of.}}

◊folded{
◊speaker["" "William G Hatch"]{Rash: Reckless Shell Programming in Racket}
◊rlink["2017/hatch.pdf"]{Slides}

Do you enjoy automating with shell scripts, but find you need to toss out your Bash implementation in favor of using a "real" programming language as your script grows?  Do you find yourself writing in Racket wishing you could throw down a quick pipeline of five processes without several lines of port and process juggling?  Do you wish your interactive Bash shell were more like a Racket repl, or that your Racket repl were more like Bash?  In this talk I describe my solution to these problems and more:  Rash, the shell language for Racket.

◊bio{◊link["http://willghatch.net/blog/"]{William Hatch} is a PhD student at the University of Utah, with interests in language extensibility, DSLs, and education.  He loves Unix shells, and does most of his computing in a terminal emulator.  In his spare time he enjoys music, yo-yoing, orange juice, and relaxing at home with his wife.}}

◊folded{
◊speaker["" "Alexis King"]{Hackett, a Haskell for Racketeers}
◊rlink["2017/king.pdf"]{Slides}

Racket, as a language, excels at and encourages a style of linguistic abstraction using metaprogramming. One of the most potentially useful tools to a metaprogrammer is intimate knowledge of expressions’ types, but existing general purpose, statically typed languages do not make that information easily accessible to the macro writer. This talk presents an early, experimental version of Hackett, an embedding of a Haskell-style type system in Racket’s macro system. Hackett attempts to provide a robust typechecker with friendly, powerful type inference for general-purpose programs that simultaneously exposes that information to Racket’s ordinary macro-writing facilities.

◊bio{◊link["https://lexi-lambda.github.io/"]{Alexis King} is a software engineer at CJ Affiliate, primarily writing Haskell and generally encouraging functional programming. In her free time, she spends time thinking about types, macros, programming languages, and making friendly software.}}

◊folded{
◊speaker["" "Jay McCarthy"]{Teaching and Designing Microarchitecture in Racket}
◊rlink["2017/mccarthy.pdf"]{Slides}

In my computer architecture course, we study the full stack of CPU design from basic components (like adders) through microarchitecture, microcode, IO & interrupt systems, and through assembly using the MIC-1 by Tanenbaum. Students use tools for simulating the CPU, compiling the microcode, and assembling the macrocode. In previous years, I used a C-based suite (3779 LOC) which was very painful to use and maintain. For my most recent offering, I rewrote all of the tools in Racket (with 55% less code, including an extensive test-suite and more features!) In order to do this well, I wrote a general purpose hardware description DSL in Racket with three different backends, including compilers to C and LLVM IR. In this talk, I'll discuss how to design compilers and DSLs like this in Racket and give a brief explanation of some fun microarchitecture points.

◊bio{◊link["https://jeapostrophe.github.io/home/"]{Jay McCarthy} can't fathom not using one of his favorite languages (C, Coq, and Racket---in reverse order of favor) in his classes. Since he already taught this course using a C simulator, he was forced to write an HDL in Racket. Next year, I guess he'll be doing Coq-based verification of circuits. *shudder* When he's not writing Racket programs, he is a core Racket developer, associate professor at UMass Lowell, and a very happy father.}}

◊folded{
◊speaker["" "Mangpo Phitchaya Phothilimthana"]{High-Coverage Hint Generation for Racket Programming Assignments}
◊rlink["2017/phothilimthana.pdf"]{Slides}

In massive programming courses, automated hint generation offers the promise of zero-cost, zero-latency assistance for students who are struggling to make progress on solving a program. 

In this talk, I will describe a robust hint generation system for Racket programming assignments that extends the hint coverage of the mutation-based approach using two complementary techniques. A syntax checker detects common syntax misconception errors in individual subexpressions to guide students to partial solutions that can be evaluated for the semantic correctness. A mutation-based approach is then used to generate hints for almost-correct programs. If the mutation-based approach fails, a case analyzer detects missing program branches to guide students to partial solutions with reasonable structures. Our system itself is implemented in Racket. We utilize Rosette, a solver-aided language embedded in Racket, as a constraint solver to prove program equivalence.

After analyzing over 75,000 program submissions and 8,789 hint requests from UC Berkeley’s introductory computer science course, we found that using all three techniques together could offer hints for any program, no matter how far it was from a correct solution. Furthermore, our analysis shows that hints contributed to students’ progress while still encouraging the students to solve problems by themselves.

◊bio{◊link["https://people.eecs.berkeley.edu/~mangpo/www/home.html"]{Phitchaya Mangpo Phothilimthana} is a PhD student from UC Berkeley working with Professor Ras Bodik. Her PhD thesis focuses on designing programming models and using program synthesis to build smart compilers for unconventional hardwares (from an ultra-low-power spatial architecture to programmable network cards). She is also interested in applying program synthesis to other domains, including educational tools.}}

◊folded{
◊speaker["" "Prabhakar Ragde"]{Proust: A Nano Proof Assistant}
◊rlink["2017/ragde.pdf"]{Slides}

Three things that don't normally go together are intuitionistic type theory, Racket as a teaching language, and a required sophomore course in logic and computation. I'll describe how I've been teaching the course by having students code a small proof assistant (Proust) in Racket and using it to do their homework. Its construction proceeds in stages through propositional and predicate logic, going as far as recursive functions on natural numbers before a lateral transition to Coq.

◊bio{◊link["https://cs.uwaterloo.ca/~plragde/"]{Prabhakar Ragde} has been a professor of Computer Science at the University of Waterloo since 1988. He first encountered Racket fifteen years ago when looking for a way to introduce his young daughters to CS. Now more than two thousand students a year take Waterloo courses that use Racket to introduce them to CS, and he can't go a day without using a lambda.}}

◊folded{
◊speaker["" "David Storrs"]{Racket and Business}
◊link["https://tinyurl.com/David-Storrs-7th-RacketCon"]{Slides}

Racket is the official language at Biomantica Inc, the biotech startup I co-founded. I had a little experience with it in advance and my cofounder learned it from scratch as we worked. In this talk we'll go over why we chose it, the pros and cons we've found, the process of learning Racket on the job, and thoughts on how it worked out in practice.

◊bio{◊link["http://biomantica.com"]{David Storrs} started programming professionally when Netscape and IE were duking it out for market share. He's done a lot of web+devops work, and at least a little of most other types of programming -- financial software, medical software, video games, etc. He's co-founder of Biomantica Inc, a biotech software company whose initial product focuses on big data transport between scientific or corporate collaborators who may be continents apart.}}

◊folded{
◊speaker["" "Vishesh Yadav"]{RacketScript}
◊rlink["2017/yadav.pdf"]{Slides}

Want to embrace the power of Racket in the ugly JavaScript world? RacketScript can compile Racket (ok! not all of it) to JavaScript. I'll will demonstrate how the RacketScript compiler works, currently implemented features of Racket, and how it differs from Racket and other Racket to JavaScript compilers.

◊bio{◊link["https://www.twistedplane.com/"]{Vishesh Yadav} is a Software Engineer at Nutanix where he works on distributed storage systems. He started hacking in Racket while pursuing M.S. in Computer Science from Northeastern University, where he also got interested in programming languages.}}

}



◊h3{◊xtarget["schedule"]{Saturday Oct 7}}

◊schedule[
◊row{◊at{8:30–9:30} ◊desc{Breakfast & Registration}}
◊row{◊at{9:30} ◊desc{Welcome}}
◊row{◊at{9:30–10:30} ◊desc{Keynote: Dan Friedman & Will Byrd: The Reasoned Racketeers}}
◊row{◊at{10:30–11:00} ◊desc{Break}}
◊row{◊at{11:00–12:00} ◊desc{
Deren Dohoda: Prototype to Production: A Racket Experience Report
Leif Andersen: Movies as Programs: The Story of a Racket
Jack Firth: A RackUnit Toolkit: Growing Racket's Testing Ecosystem}}
◊row{◊at{12:00–14:00} ◊desc{Lunch}}
◊; ◊row{◊at{14:00} ◊desc{Contributor Awards}} ◊; if we have them
◊row{◊at{14:00–15:00} ◊desc{
Jay McCarthy: Teaching and Designing Microarchitecture in Racket
Mangpo Phitchaya Phothilimthana: High-Coverage Hint Generation for Racket Programming Assignments
Prabhakar Ragde: Proust: A Nano Proof Assistant}}
◊row{◊at{15:00–15:15} ◊desc{Break}}
◊row{◊at{15:15–16:30} ◊desc{
David Christiansen: A Little Bit of Dependent Types
Vishesh Yadav: RacketScript
David Storrs: Racket and Business
Alexis King: Hackett, a Haskell for Racketeers}}
◊row{◊at{16:30–16:45} ◊desc{Break}}
◊row{◊at{16:45–18:00} ◊desc{
Charles Earl: Deep Learning with Racket: An Experience Report
Dan Anderson and Anthony Pineci: A Methodology for Teaching Kalman Filtering to High School students
William G Hatch: Rash: Reckless Shell Programming in Racket
Andrew Gwozdziewycz: Simplifying Slideshow for DWIM Presentations that Stick, Quick
}}
]



◊h3{Saturday evening}

◊schedule[
◊row{◊at{19:00–23:00} ◊desc{Food & drink at local brewpub ◊link["http://www.elysianbrewing.com/locations/elysian-fields/"]{Elysian Fields}}}
]



◊h3{Sunday Oct 8}

New for (seventh RacketCon) — Racketeer Office Hours!

The day begins with an open discussion about the state of Racket, happenings in the last year, and plans for the future. The floor is open to any questions and comments you may have.

Following this discussion are office hours proper. Bring your projects to get assistance from experienced members of the community, start new ones inspired by what you saw on Saturday, or pick from a list of suggested tasks to help us improve Racket!

◊schedule[
◊row{◊at{9:00–10:00} ◊desc{Breakfast}}
◊row{◊at{10:00–11:00} ◊desc{Town Hall Meeting}}
◊row{◊at{11:00–11:45} ◊desc{Racket Development Mini-Tutorials
Ben Greenman: ◊link["https://blog.racket-lang.org/2017/09/tutorial-contributing-to-racket.html"]{Contributing to the Racket codebase}
Stephen Chang: Packaging Racket projects
Spencer Florence and Jesse Tov: ◊link["https://gist.github.com/florence/b3fcc1df922008604e64362484dc1c28"]{Scribbling documentation}
}}
◊row{◊at{11:45–12:00} ◊desc{Project Introductions}}
◊row{◊at{12:00–13:00} ◊desc{Lunch}}
◊row{◊at{13:00–open}  ◊desc{Office Hours / Hackathon
◊link["https://github.com/racket/racket/wiki/Racketeer-Office-Hours-2017-Task-Ideas"]{Task Ideas}
}}
]

◊div{
◊b{Participating:}
◊link["http://www.cs.utah.edu/~mflatt/"]{Matthew Flatt} (Racket core)
◊link["https://www.eecs.northwestern.edu/~robby/"]{Robby Findler} (Racket core, DrRacket)
◊link["http://www.ccs.neu.edu/home/samth/"]{Sam Tobin-Hochstadt} (Racket core, Typed Racket)
◊link["https://jeapostrophe.github.io/home/"]{Jay McCarthy} (Racket core, web)
◊link["http://users.eecs.northwestern.edu/~stamourv/"]{Vincent St-Amour} (Typed Racket, optimizer, Racket releases)
◊link["http://beautifulracket.com"]{Matthew Butterick} (Pollen)
And many more!
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

