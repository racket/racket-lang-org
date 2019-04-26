#lang pollen


◊top-section{
◊span[#:id "logo" #:style "font-size:2.3rem;white-space:nowrap;"]{◊img[#:src "img/racket-logo.svg" #:class "logo"] Racket ◊span[#:id "tagline" #:class "disappearing" #:style "font-size:70%;color:gray;white-space:nowrap;margin-left:0.2rem;"]{}}

◊div{
◊link[#:class "top-button disappearing-late" #:id "download" "https://download.racket-lang.org/"]{download}

◊link[#:class "top-button disappearing-late" #:id "docs" "https://docs.racket-lang.org/"]{docs}

◊link[#:class "top-button disappearing-late" #:id "packages" "https://pkgs.racket-lang.org/"]{packages}

◊link[#:class "top-button disappearing-late" #:id "download" "sfc.html"]{donate}
}}


◊special-section[#:class "one-column-body-text" #:style "background:lightgray;padding:0.4rem" #:id "pull-quote"]{◊div[#:style "font-size:1.0rem"]{Racket is a general-purpose programming language. Got a programming problem? Use Racket. 

More importantly, Racket is a ◊link["http://felleisen.org/matthias/manifesto/"]{programming language for language-oriented programming}.
Got a real problem? ◊link["https://docs.racket-lang.org/guide/languages.html"]{Make a language} to solve it. 
Got many real problems? Make ◊link["languages.html"]{many languages} and link them together. 
Got a problem making languages? Use ◊link["index.html"]{Racket}.

Take a quick look at some ◊link["sample-languages.html"]{sample languages} first.}}

◊special-section{

◊feature["Powerful Macros, Rich DSLs" #:id "f6"]{
Racket is the first general-purpose programming language that empowers programmers to make and use domain-specific languages as if they were plain code. No external tools, no make files required.

◊doclinks{
◊link["https://docs.racket-lang.org/guide/macros.html"]{Intro to macros}
◊link["https://docs.racket-lang.org/reference/Macros.html"]{Macros in depth}
◊link["https://docs.racket-lang.org/guide/hash-languages.html"]{Making new languages}
◊link["languages.html"]{A list of sample #langs}
}
}

◊feature["Mature, Stable, Cross-Platform" #:id "f2"]{
Racket was designed from the beginning to be a cross-platform language (Windows, macOS, Linux). Over the past two decades it has matured into a stable product.
 
◊doclinks{
◊link["https://docs.racket-lang.org/pkg/index.html"]{Package system}
◊link["https://docs.racket-lang.org/framework/index.html"]{GUI framework}
◊link["https://docs.racket-lang.org/raco/exe.html"]{Standalone binaries}
◊link["https://docs.racket-lang.org/foreign/index.html"]{Foreign interface}
}
}

◊feature["Batteries Included" #:id "f1"]{
Racket includes with a rich set of libraries, covering the full range from web server apps to mathematics and science software. 

◊doclinks{
◊doclink["web-server"]{Web applications}
◊doclink["db"]{Database}
◊doclink["math"]{Math & statistics}
◊link["https://docs.racket-lang.org"]{full list ◊begin['rarr]}
}
}

◊feature["Gradual Typing, With Safety" #:id "f4"]{Racket is the first language to offer a safe gradual typing system. Programmers may add types to any of their modules, and their code will continue to work---mostly as is. The runtime will catch all conflicts between typed and untyped modules.

◊doclinks{
◊link["https://docs.racket-lang.org/ts-guide/index.html"]{The Typed Racket Guide}
◊link["https://www2.ccs.neu.edu/racket/pubs/typed-racket.pdf"]{Migratory Typing}
}
}

◊feature["IDE Support, Integrated Documentation" #:id "f3"]{
Racket comes with its own innovative and extensible interactive development environment. It has inspired an Emacs mode and a Vim plug-in. 
                                                            
◊doclinks{
◊doclink["drracket"]{DrRacket guide}
◊doclink["drracket-tools"]{DrRacket tools}
◊link["https://docs.racket-lang.org/guide/Vim.html"]{Vim}
◊link["https://docs.racket-lang.org/guide/Emacs.html"]{Emacs}
}
}

◊feature["Software Contracts, Beyond Types" #:id "f5"]{
Racket supports the first higher-order software contract system. Programmers may describe the services of their modules up to any desired precision. If a module fails to live up to those contracts, the runtime pinpoints the exact problem.                                         

◊doclinks{
◊link["https://docs.racket-lang.org/guide/contracts.html"]{Software Contracts}
}
}
}

◊section[#:style "background:lightgray;padding:0.5rem"]{
Software

◊link["https://download.racket-lang.org/"]{Download Racket v7.2}

◊link["https://github.com/racket/racket/"]{Source code}

◊link["https://github.com/racket/racket/issues"]{Bug reports}

◊link["https://pre.racket-lang.org/installers/"]{Nightly snapshot builds}

◊link["https://pkgs.racket-lang.org/"]{Packages}
}

◊section{
Documentation & tutorials

◊link["https://docs.racket-lang.org/quick/"]{Quick introduction}

◊link["https://docs.racket-lang.org/more/"]{Systems programming}

◊link["https://docs.racket-lang.org/guide/"]{The Racket guide}

◊link["https://docs.racket-lang.org/reference/"]{The Racket reference}

◊link["https://docs.racket-lang.org/continue/"]{Web applications}

◊link["https://docs.racket-lang.org"]{All documentation}
}

◊section[#:style "background:lightgray;padding:0.5rem"]{
News

◊link["https://blog.racket-lang.org/2019/01/racket-v7-2.html"]{Racket version 7.2} is available.

◊link["https://school.racket-lang.org"]{Racket School 2019} and ◊link["https://con.racket-lang.org"]{ninth RacketCon} will happen in Salt Lake City in July 2019.

◊link["https://devswag.com/products/racket-t-shirt"]{Racket t-shirts} — the perfect way to meet friends, influence people, and stay warm.

◊link["https://devswag.com/products/racket"]{Racket stickers} — the indispensable accessory for laptops and textbooks.

}

◊section{
Community

◊link["https://lists.racket-lang.org/"]{Mailing list} and ◊link["https://blog.racket-lang.org/"]{blog}

◊link["https://botbot.me/freenode/racket/"]{#racket IRC} on freenode.net

◊link["https://racket.slack.com/"]{Slack channel} (visit ◊link["http://racket-slack.herokuapp.com/"]{this link} to sign up)

◊link["https://twitter.com/racketlang"]{@racketlang} on Twitter

◊link["team.html"]{Team}
Racket's development benefits from a large distributed pool of
contributors. 

◊link["sfc.html"]{Software Freedom Conservancy}
Make a tax-deductible contribution to support our work.

}


◊section[#:style "background:lightgray;padding:0.5rem"]{
Books


◊link["https://www.realmofracket.com/"]{Realm of Racket}
Learn to program with Racket, one game at a time.

◊link["https://beautifulracket.com/"]{Beautiful Racket}
Make your own programming languages with Racket.

◊link["http://serverracket.com"]{Server: Racket}
Develop a web application with Racket.


◊link["books.html"]{All Racket books}


}


◊section{
Education

◊link["https://www.htdp.org/"]{How to Design Programs}
A principled approach to programming.


◊link["http://programbydesign.org/"]{Program by Design}
A workshop to train teachers using ◊link["http://htdp.org/" #:style "color:gray"]{How to Design Programs} in the classroom

◊link["http://www.bootstrapworld.org/"]{Bootstrap}
A curriculum for middle-school students

}



◊section[#:id "bottom" #:class "one-column-body-text" #:style "background:lightgray;padding:0.5rem"]{
Thank you

To ◊link["http://www.nsf.gov/"]{the NSF}, ◊link["http://www.darpa.mil/"]{DARPA}, the ◊link["http://www.ed.gov/FIPSE/"]{Fund for the Improvement of Postsecondary Education (FIPSE)} at the ◊link["http://www.ed.gov/"]{US Department of Education}, the ◊link["http://www.exxonmobil.com/Corporate/community_foundation.aspx"]{Exxon Foundation}, CORD, partners of the Academy of Information Technology, ◊link["http://microsoft.com/"]{Microsoft}, ◊link["http://mozilla.org/"]{Mozilla}, and ◊link["http://google.com/"]{Google} for their generous support over the years.}
