#lang pollen
◊(require racket/file pollen/cache)

◊head["a"]{Eighth}
◊head["b"]{RacketCon}
◊head["c"]{St. Louis 2018}

◊gap[1]

◊(define this-rc-date "29–30 September 2018")

◊h2{◊xlink["register"]{◊this-rc-date}}
◊h2{◊xlink{Venue}}
◊h2{◊xlink{Speakers}}
◊h2{◊◊xlink{Register}}
◊h2{◊xlink{Schedule}}

◊code{(eighth RacketCon)} is the meeting for everyone interested in ◊link["http://racket-lang.org"]{Racket} — a ◊link["https://docs.racket-lang.org/quick/index.html"]{general-purpose programming language} that's also the ◊link["http://felleisen.org/matthias/manifesto/"]{world’s first ecosystem} for developing and deploying new languages.

RacketCon is for developers, contributors, programmers, educators, and bystanders. It's an opportunity for all of us to share plans, ideas, and enthusiasm, and help shape the future of Racket.

◊h3{Live Stream}

We plan on live streaming the event, as we have in the past.

◊h3{◊xtarget["speakers"]{Keynote}}

◊head["d"]{Kent Dybvig}

◊bio{◊strong{R. Kent Dybvig} is a Distinguished Engineer at Cisco Systems, Inc. and Professor Emeritus of Computer Science at Indiana University. He is the creator and primary developer of Chez Scheme and author of The Scheme Programming Language, published by MIT Press.  He served as chair of the editorial committee for the Revised^6 Report on Scheme from 2006 until its completion in 2008.}

◊h3{◊xtarget["venue"]{Venue}}

This year, RacketCon will join the ◊link["https://icfp18.sigplan.org"]{International Conference on Functional Programming} (ICFP) and ◊link["https://www.thestrangeloop.com/"]{Strange Loop} for a week of programming revelry in St. Louis, Missouri at the ◊link["http://curiocollection3.hilton.com/en/hotels/missouri/st-louis-union-station-hotel-curio-collection-by-hilton-STLCUQQ/index.html"]{Union Station Hotel}. Specifically, we are in the Jeffersonian and Knickerbocker rooms.

◊h3{◊xtarget["register"]{Register}}

◊schedule[
◊row{◊at{◊span[#:style "font-weight: bolder"]{Early bird}} ◊desc{◊strong{$75 individual · $45 student (until September 8)}}}
◊row{◊at{Standard} ◊desc{$100 individual · $60 student (after September 8)}}
]

Buy your ticket at ◊link["https://www.eventbrite.com/e/racketcon-2018-tickets-47064849231"]{Eventbrite}.

RacketCon attendees also get a fantastic ◊link["http://group.curiocollection.com/RacketCon2018"]{group rate} at the ◊link["http://curiocollection3.hilton.com/en/hotels/missouri/st-louis-union-station-hotel-curio-collection-by-hilton-STLCUQQ/index.html"]{Union Station Hotel} itself.

◊h3{◊xtarget["speakers"]{Speakers}}

◊div{

◊folded{
◊speaker["" "Jesse Tov"]{A #lang for data structures students}

At Northwestern, undergraduates take a data structures course in their third quarter, after only 20 weeks of programming instruction. I teach data structures primarily as a programming course, which raises the question: What language should students program in? I was dissatisfied with most professional languages, as they present too much distracting complexity for these inexperienced students. The students were dissatisfied with Racket-like teaching languages. As a compromise, I designed Data Structures Student Language 2, which offers Python-like syntax, but with sensible semantics and a minimal library. In this talk, I will show you some of the syntax and semantics of DSSL2 and discuss how it is implemented as a Racket #lang.

◊bio{◊strong{Jesse Tov} is an Assistant Professor of Instruction at Northwestern University. Previously he was a lecturer at Northeastern and Harvard Universities. He likes cooking, dogs, and functional programming, whatever that means.}
}

◊folded{
◊speaker["" "Ryan Culpepper"]{The Cost of Sugar}

Racket allows programmers to adapt the language to their needs, from defining simple syntactic abbreviations to creating entire new languages hosted within Racket. Macros are the primary source of this power. But macros have costs, too. One kind of cost is compiled code size (which indirectly also affects compilation time).

This talk is about the code size cost of macros. In particular, I'll talk about how to measure the costs associated with macros, I'll explain how to reduce those costs, and I'll tell some stories about costly macros in the Racket ecosystem.}

◊folded{
◊speaker["" "Jeffrey Edgington and Chris GauthierDickey"]{Bringing Back Design by Numbers}

"Design by Numbers," by John Maeda, was a formative book on learning how to program mixed with a design perspective which later influenced the creation of Processing and other languages targeting artists and designers. While the book is now out of print, working versions of the language itself have mostly disappeared. We decided to reconstruct the Design by Numbers language using Racket's #lang facilities as a way to explore beginning languages and to allow our artist colleagues an easy way to access this language and its interesting set of features once again. In this talk, we go through the process of creating a #lang in Racket from scratch and also describe the challenges in reconstructing the semantics of the language which only became apparent after example programs did not have the same results as the text.

◊bio{◊strong{Chris GauthierDickey} is an Associate Professor in Computer Science and the Director of Game Development Programs and ◊strong{Jeffrey Edington} is an Associate Teaching Professor at the University of Denver.  Both are interested in how beginners work with and learn new programming languages. Chris was introduced to the Design by Numbers book and language by his colleague Jeff where they collaborated to reconstruct the language.}
}

◊folded{
◊speaker["" "Jesse Alama"]{Language-oriented programming: A cultural anthropology}

Racket’s more than just a batteries-included Lisp. It’s about solving problems by making languages; language-oriented programming is the name of the game. The idea has an immediate intuitive appeal. Perhaps it’s what drove you to Racket in the first place. And if it wasn’t, chances are good, if you stick around the community for a while, that you’ll find yourself thinking about languages.

But what are we talking about when we talk about “language-oriented programming”? Looking around the Racket community & among its fellow travelers, one finds that “make your own language” has a variety of senses. Although some common core ideas can be enumerated, they get expressed in different ways. At times, one wonders: are we all talking about the same thing?

To help foster the growth of language-oriented programming in general—and Racket in particular as the best environment for solving problems in that way—it’s important that we understand the diversity of meanings of “language-oriented programming” and kindred expressions. A good place to help develop that understanding is the Racket community itself and its principal drivers. Here I present the results of a survey of core Racket developers about what “make your own language” means to them.

◊bio{◊strong{Jesse Alama} counts himself lucky for having been exposed early in life to Scheme. After enjoying Common Lisp for several years, he wanted something new; he flirted with Clojure but then settled on Racket a few years ago and has never looked back. Although in his day job he works at an ecommerce company, for most of his adult life Jesse was involved in academia, doing philosophy of mathematics, automated theorem proving & proof checking, and mathematical logic.}
}

◊folded{
◊speaker["" "Jon Zeppieri"]{How to Ask for the Time in Racket}

◊bio{◊strong{Jon Zeppieri} is a software developer at CarePort Health in Boston where he has been overheard on multiple occasions asking "Yes, but 'today' in what time zone?" He discovered Racket in the late '90s, when it was called PLT Scheme, and has used it ever since.}
}

◊folded{
◊speaker["" "Stephen R. Foster"]{I Pivoted My Startup to Use Racket.  Can We Pivot the World?}

Every year, my company teaches coding to over 5000 students in San Diego.  Topics range from: robotics, game design, web design, virtual reality, Java, Python, physical computing, and Minecraft modding.  Why so many topics?  That's what the market wants, so that's what we provide.  In 2018, I took on the seemingly impossible challenge of unifying all our topics under a single language of instruction.  That language was Racket.  This talk reports on our use of Racket and Racket-embedded DSLs to teach a broad spectrum of computing topics. 

Due to the resounding success of our Racket pivot, I'm convinced that Racket is an educational power-tool for computer science education.  I used to say that my mission was to teach coding to every child in the world.  I can now confidently refine that mission: I want to teach Racket to every child in the world.  This talk outlines the growth vision for ThoughtSTEM -- a vision that will bring broad-spectrum Racket-based education beyond San Diego and into 20 more major American cities by the end of 2022.  Ultimately, I want to see DrRacket on every computer in America, as a basic tool that everybody knows how to use -- as familiar and useful as a web browser or a word processor.  Can we make this happen?  I think we can.

◊bio{◊strong{Stephen R. Foster}, Ph.D. is the CEO and Co-Founder of ThoughtSTEM, an NSF-funded ed-tech company whose mission is to teach computer science to every child in the world.  ThoughtSTEM has launched multiple educational titles -- LearnToMod and CodeSpells, both of which have been featured in venues such as Wired, the Guardian, and Rock, Paper, Shotgun.  ThoughtSTEM's software is used by over 100,000 people world-wide.  ThoughtSTEM also runs coding after-school programs and summer camps in San Diego, teaching over 5000 students face-to-face every year.}
}

◊folded{
◊speaker["" "David Storrs"]{Batteries Included: Commercializing Racket}

The Racket ecosystem (toolchain, libraries, documentation, etc) is surprisingly robust considering the comparatively small userbase of Racket as compared to more mainstream languages such as Java, Perl, and C++.  There are some holes in what's available, but there are some impressively esoteric items as well -- things that you might not need too often, but when you need them you *really* need them.

Biomantica Inc is a bioinformatics company built entirely on Racket.  This talk will cover our experience with Racket packages, documentation, etc, as it relates to our business.

◊bio{◊strong{David} started programming professionally in 1995 and has worked in fields as diverse as medical software, financial software, video games, publishing, and (of course) web programming.  Biomantica is his third (fourth, depending on how you count) startup.  He's been working with Racket since late 2015 and is dreading the idea of ever going back to a more mainstream language.}
}

◊folded{
◊speaker["" "Christopher Lemmer Webber and Morgan Lemmer-Webber"]{Racket for Everyone (Else)}

Racket currently historically targets two groups: young programmers (learn by building games) and programming language theorists.  We believe Racket is well positioned to meet a wide variety of people in-between these groups.  Morgan will present on our use of Racket for non-programmers.  Using Racket, Dr. Racket, and Scribble, we have run a series of successful workshops on “programmable publishing”.  Although these were marketed to humanities students, this method can be applied to a wide variety of disciplines including the social sciences and mathematics.  It can be applied outside of the university for creative writing, community outreach, and other populations with publishing needs.  The common thread through these communities is users who don’t view themselves as programmers or who would like to program but don’t think they have the ability or resources to learn.  Because of its accessibility, we use scribble as a gateway into basic programming skills where participants can apply their fledgling skills to a practical output that relates to their daily lives.  Chris will present on Racket for the general programmer.  Racket is as batteries-included as Python and is well set up to be just as general.  How can we reach a general audience?  What can we gain by doing so?

◊bio{◊link["https://dustycloud.org/"]{Christopher Lemmer Webber} is a user freedom advocate with who focuses on network freedom.  Chris is co-editor of the W3C ActivityPub protocol which provides federated communication for decentralized social networks.  They have been programming in lisp for some time and have recently come to love Racket and have been co-running the "Programmable Publishing: Digital Humanities for Everyone" workshops using Racket and Scribble with Morgan Lemmer-Webber.}

◊bio{◊link["https://mlemmer.org/"]{Morgan Lemmer-Webber} is a PhD Candidate in the department of Art History at the University of Wisconsin, Madison. Her dissertation focuses on the real and symbolic associations between women and textile production in the Roman Empire. She has been the lead developer for the Digital Humanities project "A colonial merchant: The ledger of William Ramsay" since 2015. She has recently developed materials for and co-taught a series of workshops to teach basic programming skills to humanities majors using Racket and Scribble with Christopher Lemmer Webber.}
}

◊folded{
◊speaker["" "Philip McGrath"]{Digital Ricœur: Racket in the Humanities}

Digital Ricœur is a project to digitize the works of philosopher Paul Ricœur and provide analysis tools for scholars, including those with no technical background. The project is built by a small team, all of whom have "day jobs" outside of software development. Racket's distinctive features have been a key to our success, both for our user-facing portal website and many internal tools. This talk will present our experience with Racket in practice, from the web server and cross-platform GUI toolkit to unexpected benefits of contracts and Scribble.

◊bio{◊strong{Philip McGrath} has been developing for the web for most of his life, and Racket has been his language of choice for the last several years. By day, Philip studies musicology and composition at the University of Chicago and performs as a singer and conductor, with a particular focus on medieval and Renaissance music.}
}

◊folded{
◊speaker["" "David Thrane Christiansen"]{Saving it for later: language-supported to-do lists in DrRacket}

While writing a program, much of the time is spent working with programs that are not yet complete. Frequently, however, this incompleteness is indicated only by a comment next to a call to error.  The DrRacket to-do list is a plugin for keeping track of which parts of a program are incomplete, providing a convenient list of what remains to be written. In some languages, such as Pie, the to-do list can even provide support for completing the task. The to-do list can work with any #lang, and I'll show you how to add support for it to your language.

◊bio{◊strong{David} is a programming languages and verification researcher/engineer at Galois, Inc. Previously, he was a core contributor to the Idris language and its interactive tooling. Together with Dan Friedman, he is a co-author of The Little Typer.}
}

◊folded{
◊speaker["" "Laurent Orseau"]{Quickscript, a tool to quickly write scripts to extend DrRacket's functionalities}

DrRacket is an amazing IDE. Written in Racket for Racket and more, one can write plugins in Racket to extend its functionalities. This plugin system is very general, but this comes at a cost: the development time is significant even for small functionalities, and requires to restart DrRacket at every change. For a large range of small scripts, Quickscript (formerly the Script Plugin) allows to write simple (and not so simple) scripts for DrRacket without needing to restart it and with much smaller development time, even making it possible to write short-lived scripts for single purposes. Sharing scripts written with Quickscript is as simple as sharing a code snippet.

◊bio{After years of self-taught programming in Pascal, BASIC and C, ◊strong{Laurent Orseau} learned PLT Scheme (Racket's name at the time) as an undergrad. But it wasn't until the end of his PhD in AI at the INSA of Rennes (France) that he realized the tremendous capabilities of the Lisp/Scheme family, and the amazing set of features that Racket offered. He stuck with it ever since, through 7 years in academia at AgroParisTech (France) and even now as a research scientist at DeepMind (London, UK), despite the supremacy of other mainstream languages in his environment. He developed several projects in Racket, notably RWind, MrEd Designer 3, Towers and Quickscript.}
}

◊folded{
◊speaker["" "Nadeem Abdul Hamid"]{Sinbad: Sailing the Waves of Data}

Sinbad is a software library, intended for use in introductory programming courses, that makes it very easy to uniformly access online data sources provided in standard formats. The library provides unified support for XML, JSON, and CSV/TSV data sources, automated caching facilities (useful for having offline access to data), sampling features (for development/testing on subsets of large data sources), and binding to instances of user-defined structures. On top of all of that, a primary pedagogical goal has been to make the interface as simple and intuitive as possible for novice programmers.

◊bio{◊strong{Nadeem Abdul Hamid} is an associate professor of computer science at Berry College.}
}

◊folded{
◊speaker["" "Pavel Panchekha"]{Verifying Web Pages with Racket and Z3}

Making a website accessible, usable, and attractive is important, but CSS's complexity makes it easy for layouts to break for some users, on some browsers, at some font sizes, or on some devices. Cassius is a new project for reasoning about all possible renderings of a web page. In this talk, I'll describe how Cassius uses the Z3 equation solver to look for mis-rendered web pages and how Racket makes it possible to implement the CSS standard accurately and concisely.

◊bio{◊strong{Pavel Panchekha} is a graduate student at the Paul G. Allen School for Computer Science and Engineering at the University of Washington, where he is a member of the Programming Languages and Software Engineering (PLSE) group. He received a BS in mathematics from MIT. His research focuses on domain-specific tools for non-expert developers, with other projects in floating-point computation.}
}

◊folded{
◊speaker["" "Jörgen Brandt"]{Petri Net Flavored Places: An Advanced Transition System for Distributed Computing in Racket}

Places provide a way to specify parallel and distributed computation in Racket. Using places we can set up independent services that communicate via channels. Often state machines are used in this setting to model session state, resource allocation, or service start order. However, plain state machines often suffer from the explosion of the state space as soon as multiple state variables appear in combination. Also some applications have only an infinite representation, if modeled as a state machine.

In this talk, we address these challenges by introducing pnet, a Racket library that allows to define Racket places as Petri nets. Petri nets are a class of transition systems representing state as tokens produced and consumed by transitions. We consider several examples for Petri nets as Racket places such as a worker pool and consider the possibility of driving the pnet library from a tailor-made DSL or using it to construct distributed programming languages.

◊bio{◊strong{Jörgen Brandt} is a PhD student based in Berlin. He is interested in programming languages and distributed systems and maintains the distributed programming language Cuneiform.}
}

◊folded{
◊speaker["" "Eric Griffis"]{Dataflow network programming with Neuron}

Neuron is a framework for high-level network programming in Racket. Its language-oriented approach builds upon familiar abstractions like channels, ports, threads, and evaluators with support for a simple but powerful form of exploratory dataflow network programming and information flow control. In this talk, I'll show how Neuron can be used to design composable networks of communicating processes, lift procedures and modules onto the network, and integrate with existing software infrastructure like the Web.

◊bio{◊strong{Eric Griffis} is a highly intuitive, life-long programmer with an acute sensitivity to the impact of software on society. His interests include programmable software infrastructure, social software ecosystems, and software as organism versus mechanism.}
}

}

◊h3{◊xtarget["schedule"]{Saturday September 29}}

◊schedule[
◊row{◊at{8:30–9:00} ◊desc{Breakfast & Registration}}
◊row{◊at{9:00} ◊desc{Welcome}}
◊row{◊at{9:05–10:15} ◊desc{Keynote: Kent Dybvig}}
◊row{◊at{10:15–10:40} ◊desc{Break}}
◊row{◊at{10:40–12:00} ◊desc{
Jesse Tov
Ryan Culpepper
Chris GauthierDickey and Jeffrey Edgington
Jesse Alama}}
◊row{◊at{12:00–12:05} ◊desc{Brief Announcements and Group Picture}}
◊row{◊at{12:05–14:00} ◊desc{Lunch at Landry's}}
◊row{◊at{14:00–15:00} ◊desc{
Jon Zeppieri
Stephen R. Foster
David Storrs}}
◊row{◊at{15:00–15:20} ◊desc{Break}}
◊row{◊at{15:20–16:40} ◊desc{
Christopher Lemmer Webber and Morgan Lemmer-Webber
Philip McGrath
David Thrane Christiansen
Laurent Orseau}}
◊row{◊at{16:40–17:00} ◊desc{Break}}
◊row{◊at{17:00–18:20} ◊desc{
Nadeem Abdul Hamid
Pavel Panchekha
Jörgen Brandt
Eric Griffis
}}
]

◊h3{Saturday evening}

◊schedule[
◊row{◊at{19:00–23:00} ◊desc{Food & drink at Morgan Street.}}
]

◊h3{Sunday September 30}

This year during Office Hours we are organizing a handful of small groups of Racketeers to lead focused sections. Groups will be loosely united around a common field such as "types" or "web programming". There are two different kinds of groups which we're calling "teams" and "workshops":

Teams are Racketeers who wish to work on community projects in a specific field. For example, a "testing team" may choose a set of Racket testing packages to improve with documentation, features, bug fixes, etc.

Workshops are Racketeers who wish to give Racket newcomers hands-on lessons in a specific field, such as with some sort of tutorial project.

Please see the coordination page on the wiki:
◊link["https://github.com/racket/racket/wiki/2018-RacketCon-Office-Hours-Teams-&-Workshops"]{2018 RacketCon Office Hours Teams & Workshops}.

◊schedule[
◊row{◊at{8:30–9:00} ◊desc{Breakfast & Registration}}
◊row{◊at{9:00} ◊desc{Welcome}}
◊row{◊at{9:05–10:00} ◊desc{State of Racket: Matthew Flatt}}
◊row{◊at{10:00–10:30} ◊desc{Break}}
◊row{◊at{10:30–12:00} ◊desc{Session 1: Workshops}}
◊row{◊at{12:00–14:00} ◊desc{Lunch at Maggie O'Brien's}}
◊row{◊at{14:00–15:30} ◊desc{Session 2: Teams & Workshops}}
◊row{◊at{15:30–15:45} ◊desc{Break}}
◊row{◊at{15:45–17:00} ◊desc{Session 3: Teams}}
]

◊h3{Sponsors (#%kernel tier)}

◊inline-list['sponsor]{
◊link["https://beautifulracket.com/about-the-author.html"]{Matthew Butterick}
}

◊h3{Sponsors (big-bang tier)}

◊inline-list['sponsor]{
◊link["https://github.com/david-vanderson/"]{David Vanderson}
◊link["https://lisp.sh"]{Jesse Alama}
◊link["http://blackswanlearning.com"]{Black Swan Learning}
}

◊h3{Friendly Environment Policy}

The Racket community aims to improve the world through programming. It started with the goal of introducing everyone to the wonderful world of program design, with a spirit of full inclusion and no emphasis on any specific group.  Over time it has grown into a full-fledged professional community with a well-known reputation for helpfulness and openness on its on-line communication channels. The organizers want to encourage an equally open exchange of ideas at RacketCon, the community's in-person meet-up.

This spirit requires an environment that enables all to participate without fear of personal harassment. We define harassment as unwelcome or hostile behavior, that is, behavior that focuses on people instead of ideas. ◊link["http://www.acm.org/sigs/volunteer_resources/officers_manual/anti-harassment-policy"]{The ACM's anti-harassment policy} lists a variety of specific unacceptable factors and behaviors. The organizers consider responses such as “just joking,” or “teasing,” or being “playful” as unacceptable.

Anyone witnessing or subject to unacceptable behavior should notify one of the RacketCon organizers (Matthew Flatt or Jay McCarthy.)

If a participant engages in harassing behavior, the conference organizers may take any action they deem appropriate, from a warning of the offender to an expulsion from the conference [without refund].

[The wording of this policy is directly derived from ◊link["http://snapl.org/2015/policy.html"]{that of the SNAPL conference}, with thanks.]

◊h3{Sponsoring}

If you would like to sponsor, please contact the ◊link["mailto:jay.mccarthy@gmail.com"]{organizers}. The ◊code{#%kernel} tier's names and logos will appear on all RacketCon printed materials and advertisements.

◊h3{Previous RacketCons}

◊(define (conlink year) 
  (link (format "con.racket-lang.org/~a" year) year))

◊inline-list['con]{
◊conlink{2017}
◊conlink{2016}
◊conlink{2015}
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}

◊gap[1]

◊em{"I was wandering through the darkness, groping for anything to hold on to. The heat and exhaustion was unbearable and I felt that I could not move any longer. At the last moment before collapse, my fingers just barely touched something cold and solid. I stumbled forward and grasped at it with all my being. It was a door knob. When I opened the door, I stumbled through and felt a cool comfort that was indescribable in the moment. When my eyes acclimatized to my new surroundings, I realized that I was at RacketCon."}

— Satisfied Customer, RacketCon 2017
