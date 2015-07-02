#lang pollen
◊(require racket/file)

◊h1{(fifth RacketCon)}
◊(define rc-2015-date "27 Sept 2015")
◊h2{◊xlink["venue"]{◊rc-2015-date}}
◊h2{◊xlink["venue"]{St. Louis}}
◊h2{◊xlink{Speakers}}
◊h2{◊xlink{Register}}

◊div['class: "codebox"]{◊div['class: "opacity-control"]{◊(file->string "eero-demo.rkt")}}

◊h3{◊xtarget["venue"]{◊rc-2015-date in St. Louis (the day after ◊link["http://thestrangeloop.com"]{Strange Loop})}}

RacketCon is a public meeting for everyone interested in Racket: developers, contributors, programmers, educators, and bystanders. It's an opportunity for all members of the community to come together to share plans, ideas, and enthusiasm. RacketCon will enable the entire Racket community to mingle: to update each other, to exchange ideas, to collaborate, and to help shape the future of Racket.

◊h3{◊xtarget["register"]{Register}}
◊link["https://eventbrite.com/event/16825218682/"]{Via Eventbrite.}

◊h3{◊xtarget["speakers"]{Keynote speaker}}
◊keynote-speaker["" ◊link["www.ccs.neu.edu/home/matthias"]{Matthias Felleisen}]{The Racket Manifesto}

◊h3{◊xtarget["speakers"]{Confirmed speakers}}
◊speaker["" ◊link["https://github.com/sabauma"]{Spenser Bauman}]{JIT Compilation for Racket}
◊speaker["" ◊link["https://github.com/m4burns"]{Marc Burns}]{Rocking with Racket}
◊speaker["" ◊link["https://twitter.com/daviesaz"]{Byron Davies}]{Rexcel: A Racket-based spreadsheet processing system}

◊folded{
◊speaker["" "Jack Firth"]{Generic Syntax Expanders and Extensible Macros}
Racket's powerful syntax system allows things like pattern matching not only to be implemented as macros, but to be user-extensible as well. Match expanders, for example, allows users to extend Racket's pattern matcher with arbitrary new patterns. This talk discusses a generalized approach to match-expanders and its uses in defining macros with embedded user-extensible syntax, and shows an application of this approach to Racket's command line parsing forms.

◊link["http://codepen.io/Universalist/"]{Jack Firth} works at Mindjet in San Francisco as a developer of enterprise web applications and as a mentor to his colleagues on functional programming, particularly its applications to a Node / Angular stack. He has been a professional software engineer for a year and a half, but first began programming nine years ago. He attended college for mechanical engineering at a young age. His programming-related interests outside work include multi-language architectures, CI, testing, obscure languages, static verification, and type systems. He also used to be a hobbyist game developer, and still is once in a blue moon.
}


◊speaker["" ◊link["http://www.cs.utah.edu/~mflatt/"]{Matthew Flatt}]{Binding as Sets of Scopes}

◊folded{
◊speaker["" "Spencer Florence"]{Code Coverage Outside of DrRacket}
Cover is a code coverage tool for Racket. It supports project level code coverage with various output formats. It supports the same configuration options as `raco test` making it easy to integrate into your project. This talk will cover the basics of using and configuring cover, as well as more advanced usage via the racket API.

◊link["https://github.com/florence"]{I am a PhD student} at Northwestern University. I primarily use Racket to
create medical programming languages, animate algorithms for wikipedia,
and make insane twitter bots.
 }

◊folded{
◊speaker["" "Ben Greenman"]{A #lang for All Seasons}
Racket's ability to create languages is extremely versatile and powerful. No matter the challenge or problem domain, Racket's language tools provide a foundation for directly expressing and solving issues. We demonstrate by taming the age-old challenge of crafting a well-formed sonnet.

◊link["http://www.ccs.neu.edu/home/types/"]{Ben Greenman} is earning his PhD
over at Northeastern University.
He's interested in leveraging machines
to do more intelligent processes,
leaving humans free to do fun things like
writing in iambic pentameter.
}

 
◊folded{
◊speaker["" "Andrew Kent"]{Practical Dependently Typed Racket}
Typed Racket has enabled developers to enrich and optimize existing dynamically Typed Racket programs with static types. We have extended this system with practical dependent types that can verify a wider range of patterns commonly found in Racket code. I will demonstrate how this extension can help users remove more bugs and optimize more operations in their programs with little (if any) additional effort.

◊link["http://andmkent.com/"]{Andrew} is a PhD student at Indiana University. He works with Sam Tobin-Hochstadt on exciting projects related to Typed Racket.
}

◊speaker["" ◊link["https://github.com/lexi-lambda"]{Alexis King}]{TBD}

◊folded{
◊speaker["" "Jay McCarthy"]{Bithoven and the NES Chamber Orchestra}
 Bithoven is a prolific composer of approximately
107936338584579028906476999435802819420152571696145967835629469168
256054600102650823583510099033608338153987460306459613902701999676
787394921157801398029216877779737562970220998606832732608453952094
357479307728375868180711548797277461020672 different compositions based on three-part harmony and basic chord progressions. The NES Chamber Orchestra is an engine for playing these (and other) compositions on the Ricoh RP2A03 (a.k.a. the NES sound chip) in one of approximately
422234004059019268090786172918417043456000 different arrangements or "NEStrations".

In randomized trials, we've found that audiences familiar with
retro-style video game music can't tell Bithoven's compositions apart
from "real" NES-era music. Yet, Bithoven contains almost no tuning for
producing plausible retro-music and has fewer lines of code than the
implementation of /bin/ls. In this talk, we'll discuss the beautiful
Racket programming, elegant mathematics, and basic music theory that
makes Bithoven work so well.

◊link["https://jeapostrophe.github.io"]{Jay McCarthy} has no artistic talent so he seeks to suck the human
spirit out of art through algorithmic assimilation. He is also a core
Racket developer and associate professor at UMass Lowell.
}
◊speaker["" ◊link["http://www.math.grin.edu/~rebelsky/"]{Samuel Rebelsky}]{TBD}
◊speaker["" ◊link["https://www.twistedplane.com/"]{Vishesh Yadav}]{TBD}
And more to be announced!


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
