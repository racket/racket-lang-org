#lang pollen
◊(require racket/file)

◊h1{(fifth RacketCon)}
◊(define rc-2015-date "27 Sept 2015")
◊h2{◊xlink["venue"]{◊rc-2015-date}}
◊h2{◊xlink["venue"]{St. Louis}}
◊h2{◊xlink{Speakers}}
◊h2{◊xlink{Register}}

◊div['class: "codebox"]{◊div['class: "opacity-control"]{◊(file->string "eero-demo.rkt")}}

◊h3{◊xtarget["venue"]{◊rc-2015-date at the St. Louis Union Station hotel (the day after ◊link["http://thestrangeloop.com"]{Strange Loop})}}

RacketCon is a public meeting for everyone interested in Racket: developers, contributors, programmers, educators, and bystanders. It's an opportunity for all members of the community to come together to share plans, ideas, and enthusiasm. RacketCon will enable the entire Racket community to mingle: to update each other, to exchange ideas, to collaborate, and to help shape the future of Racket.

◊h3{◊xtarget["register"]{Register}}
◊link["https://eventbrite.com/event/16825218682/"]{Via Eventbrite.}

◊h3{◊xtarget["social"]{Social event}}
We're planning a social gathering the evening before RacketCon (26 Sept). Stay tuned for more details!

◊h3{◊xtarget["schedule"]{Schedule}}

◊schedule[
◊row{◊at{9:00–9:30} ◊desc{Registration}}
◊row{◊at{9:30} ◊desc{Welcome}}
◊row{◊at{9:30–10:00} ◊desc{Keynote: Matthias Felleisen}}
◊row{◊at{10:30–10:50} ◊desc{break}}
◊row{◊at{10:50–11:50} ◊desc{Matthew Flatt: Carry on Racketing
Daniel Prager: YouPatch
David Vanderson: Another marvelous talk}}
◊row{◊at{11:30–13:50} ◊desc{lunch}}
]

◊h3{◊xtarget["speakers"]{Keynote speaker}}
◊folded{
◊keynote-speaker["" "Matthias Felleisen"]{The Racket Manifesto}

The creation of a programming language calls for guiding principles that point its developers to goals. This talk will spell out the three basic principles behind the 20-year development of Racket. First, programming is about stating and solving problems, and this activity normally takes place in a context with its own language of discourse; good programmers ought to formulate this language as a programming language. Hence, Racket is a programming language for creating new programming languages. Second, by following this language-oriented approach to programming, systems become multi-lingual collections of interconnected components. Each language and component must be able to protect its specific invariants. In support, Racket offers protection mechanisms to implement a full language spectrum, from C-level bit manipulation to soundly typed extensions. Third, because Racket considers programming as problem solving in the correct language, Racket also turns extra-linguistic mechanisms into linguistic constructs, especially mechanisms for managing resources and projects. This talk will explain these principles and how Racket lives up to them, present the evaluation framework behind the design process, and conclude with a sketch of Racket's imperfections and opportunities for future improvements.

◊bio{◊link["www.ccs.neu.edu/home/matthias"]{Matthias Felleisen} is the Chief Philosopher and Shepherd (CPS) of PLT Design, Inc. As such he is responsible for thinking deep thoughts about Racket, pronouncing his insights in obscure emails to the mailing list, and herding cats while increasing their numbers. He bas been around since forever and considers this presentation his half-time show.}}

◊h3{◊xtarget["speakers"]{Speakers}}
◊folded{
◊speaker["" "Spenser Bauman"]{Tracing Comes to Racket}
While object-oriented languages have embraced use of advanced just-in-time compilers for fast execution, functional languages typically rely on ahead of time compilation/optmization.  Currently, there are no good JIT compilers for the Scheme family of languages, despite their successful application to other dynamic languages.  In this talk, I will present Pycket; an implementation of Racket in RPython which makes use of the meta-tracing JIT infrastructure developed for the PyPy project.  The goal of Pycket is to provide a fast Racket implementation which can eliminate the overhead of features which are difficult to optimize statically such as gradual typing, continuations, and contracts.

◊bio{◊link["https://github.com/sabauma"]{Spenser Bauman} is a Ph.D. student at Indiana University where he studies programming language theory and implementation with Sam Tobin-Hoschstadt and Jeremy Siek. Before that, he did his undergraduate work at Penn State University studying computer science and statistics.}}

◊folded{
◊speaker["" "Marc Burns"]{Rocking with Racket}
We made an in-browser music production and exploration platform. The backend was developed in Racket and C++; we just finished porting it to Typed Racket. I will talk about how using (typed) Racket has informed our design, some pitfalls we encountered, and what we're doing to make the backend both performant and reliable now that we have a few users.

◊bio{◊link["https://github.com/m4burns"]{Marc Burns} studied math and computer science at the University of Waterloo. He likes to tinker with functional languages and language design, sometimes managing to put his strange creations to work in industry. He works at an early-stage startup in Montreal, Canada creating music production software.}}

◊folded{
◊speaker["" "Byron Davies"]{Rexcel: A Racket-Based Spreadsheet Processing System}
I developed Rexcel for analyzing standardized test data for schools. Although I did if for a specific application, the approach I took was very general, making it easy for people to perform complex spreadsheet processing in a language other than VBA.

◊bio{After his education at Caltech, MIT, Texas Instruments, Stanford (EE Ph.D), and Motorola, and after teaching medical informatics for five years at the Arizona School of Health Sciences, ◊link["https://twitter.com/daviesaz"]{Byron Davies} worked at several startups before settling into a dual career at StarShine Academy, a K-12 charter school and laboratory for learning, and Ontopilot, a company using mathematical logic and specialized hardware to develop software that debugs other software.  Exclusively a Lisper since 1973, he developed the first general-purpose “twist-down” data browser, one of the first web applications for manufacturing (published in CACM), and — in Racket — the proof-of-concept prototype for OntoPilot’s Systems Integrity Analysis System.  His current goal is to implement, in Racket, the tablet-based software that will win the Global Learning XPRIZE.}}

◊folded{
◊speaker["" "Jack Firth"]{Generic Syntax Expanders and Extensible Macros}
Racket's powerful syntax system allows things like pattern matching not only to be implemented as macros, but to be user-extensible as well. Match expanders, for example, allows users to extend Racket's pattern matcher with arbitrary new patterns. This talk discusses a generalized approach to match-expanders and its uses in defining macros with embedded user-extensible syntax, and shows an application of this approach to Racket's command line parsing forms.

◊bio{◊link["http://codepen.io/Universalist/"]{Jack Firth} works at Mindjet in San Francisco as a developer of enterprise web applications and as a mentor to his colleagues on functional programming, particularly its applications to a Node / Angular stack. He has been a professional software engineer for a year and a half, but first began programming nine years ago. He attended college for mechanical engineering at a young age. His programming-related interests outside work include multi-language architectures, CI, testing, obscure languages, static verification, and type systems. He also used to be a hobbyist game developer, and still is once in a blue moon.}}

◊folded{
◊speaker["" "Matthew Flatt"]{Binding as Sets of Scopes}
Racket's macro expander evolved from a prototype circa 2000 that combined the `syntax-case` macro system with an idea for modules and phases. It has served Racket well, but the implementation is complex and difficult to explain in full, and it's starting to fray around the edges with the addition of submodules. The new macro expander starts over using a new way of representing and tracking bindings. The resulting model of macro expansion is simpler and more uniform than the current one based on renaming, but it's mostly compatible with the old expander.

◊bio{◊link["http://www.cs.utah.edu/~mflatt/"]{Matthew Flatt} is a professor at the University of Utah and one of the main developers of Racket. He works primarily on Racket's run-time system, compiler, macro system, build system, package system, documentation language, and graphics/GUI libraries.}}


◊folded{
◊speaker["" "Spencer Florence"]{Code Coverage Outside of DrRacket}
Cover is a code coverage tool for Racket. It supports project level code coverage with various output formats. It supports the same configuration options as `raco test` making it easy to integrate into your project. This talk will cover the basics of using and configuring cover, as well as more advanced usage via the racket API.

◊bio{◊link["https://github.com/florence"]{I am a PhD student} at Northwestern University. I primarily use Racket to create medical programming languages, animate algorithms for wikipedia, and make insane twitter bots.}}

◊folded{
◊speaker["" "Ben Greenman"]{A #lang for All Seasons}
Racket's ability to create languages is extremely versatile and powerful. No matter the challenge or problem domain, Racket's language tools provide a foundation for directly expressing and solving issues. We demonstrate by taming the age-old challenge of crafting a well-formed sonnet.

◊bio{◊link["http://www.ccs.neu.edu/home/types/"]{Ben Greenman} is earning his PhD
over at Northeastern University.
He's interested in leveraging machines
to do more intelligent processes,
leaving humans free to do fun things like
writing in iambic pentameter.}}

 
◊folded{
◊speaker["" "Andrew Kent"]{Practical Dependently Typed Racket}
Typed Racket has enabled developers to enrich and optimize existing dynamically Typed Racket programs with static types. We have extended this system with practical dependent types that can verify a wider range of patterns commonly found in Racket code. I will demonstrate how this extension can help users remove more bugs and optimize more operations in their programs with little (if any) additional effort.

◊bio{◊link["http://andmkent.com/"]{Andrew} is a PhD student at Indiana University. He works with Sam Tobin-Hochstadt on exciting projects related to Typed Racket.}}

◊folded{
◊speaker["" "Alexis King"]{Generic Collections: One Interface to Rule Them All}
Racket encourages a functional style, but aside from immutable pairs, the collections library remains unsuitable for rich functional programming with a complement of functional data structures. Racket programmers may reach for linked lists when better structures would do to avoid the list-ref/vector-ref/stream-ref function zoo. This talk discusses the implementation of a generic collections system that encourages immutability by default, provides a more consistent and comprehensive interface across all data structures, and provides that interface in a way that can be leveraged by new, user-defined structures to feel as integrated with the language as Racket's built-ins.

◊bio{◊link["https://lexi-lambda.github.io/"]{Alexis King} works as a full-stack web developer at Philosophie in Los Angeles building rich web applications with Ruby on Rails and Angular. When not tinkering with different models to create expressive RESTful APIs, she is enthusiastic about finding ways to build programming interfaces that are expressive and easy to use. In Racket, that means figuring out how to build flexible DSLs and extensible language constructs that integrate well into the rest of the Racket ecosystem without sacrificing elegance or hygiene.}}

◊folded{
◊speaker["" "Jay McCarthy"]{Bithoven and the NES Chamber Orchestra}
 Bithoven is a prolific composer of approximately
107936338584579028906476999435802819420152571696145967835629469168
256054600102650823583510099033608338153987460306459613902701999676
787394921157801398029216877779737562970220998606832732608453952094
357479307728375868180711548797277461020672 different compositions based on three-part harmony and basic chord progressions. The NES Chamber Orchestra is an engine for playing these (and other) compositions on the Ricoh RP2A03 (a.k.a. the NES sound chip) in one of approximately
422234004059019268090786172918417043456000 different arrangements or "NEStrations".

In randomized trials, we've found that audiences familiar with retro-style video game music can't tell Bithoven's compositions apart from "real" NES-era music. Yet, Bithoven contains almost no tuning for producing plausible retro-music and has fewer lines of code than the implementation of /bin/ls. In this talk, we'll discuss the beautiful Racket programming, elegant mathematics, and basic music theory that makes Bithoven work so well.

◊bio{◊link["https://jeapostrophe.github.io"]{Jay McCarthy} has no artistic talent so he seeks to suck the human spirit out of art through algorithmic assimilation. He is also a core Racket developer and associate professor at UMass Lowell.}}

◊folded{
◊speaker["" "Samuel Rebelsky"]{Scripting GIMP with Racket}
The GNU Image Manipulation Program, or GIMP, is an open-source alternative to Photoshop.  In the early days of GIMP, the designers added a scripting language, Script-Fu, based on Scheme.  Over the years, Script-Fu has been superseded by Python-Fu, although Script-Fu remains an option, albeit with a primitive IDE.  Over the past few years, my students and I have built a library that allows programmers to script GIMP using Racket and the DrRacket IDE.  In addition to providing the “glue” between GIMP and Racket, we also added a library of routines more amenable to novice programmers.  In this talk, we will discuss the design of both the “glue” and the broader library.  We will also discuss a related introductory course in CS that uses multiple models of image making as the motivating problem domain.

◊bio{◊link["http://www.math.grin.edu/~rebelsky/"]{Samuel A. Rebelsky} is Professor and Chair of Computer Science at Grinnell College, a small liberal arts college in the middle of Iowa. Although he teaches courses at every level of the curriculum, Sam is particularly passionate about teaching at the introductory level, where he has developed a workshop-style curriculum that combines image making and functional programming in Racket. Sam's research projects have included Web tools, software for functional image making, and a system for combining imperative and lazy functional languages.}}

◊folded{
◊speaker["" "Vishesh Yadav"]{The Big Bang Universe on the World Wide Web}
Each semester, thousands of students learn how to design programs using Racket's "universe" library, a no-boilerplate framework for easily writing interactive graphical programs. However, sharing their creations with family and friends is not as easy. I'll present my one-click solution, aided by the Whalesong Racket-to-JS compiler, that enables students to effortlessly go from IDE to sharable, interactive web-hosted program.

◊bio{◊link["https://www.twistedplane.com/"]{Vishesh Yadav} is a Masters student at Northeastern University, working on ideas to improve Whalesong. Before coming to Northeastern to study Programming Languages, he hacked on some open source projects such as KDE and the DragonFly BSD kernel.}}


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
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}
