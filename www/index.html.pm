#lang pollen


◊top-section{
◊span[#:id "logo" #:style "font-size:2.3rem;white-space:nowrap;"]{◊img[#:src "img/racket-logo.svg" #:class "logo"] Racket ◊span[#:id "tagline" #:class "disappearing" #:style "font-size:70%;color:gray;white-space:nowrap;margin-left:0.2rem;"]{solve problems · make languages}}

◊div{

◊link[#:class "top-button disappearing-late" #:id "docs" "https://docs.racket-lang.org/"]{docs}

◊link[#:class "top-button disappearing-late" #:id "packages" "https://pkgs.racket-lang.org/"]{packages}

◊link[#:class "top-button disappearing-late" #:id "download" "https://download.racket-lang.org/"]{download}
}

}


◊special-section{

◊feature["Batteries included" #:id "f1"]{Racket's extensive standard library gets your projects off the ground quickly.
◊doclinks{
◊doclink["web-server"]{Web applications}
◊doclink["db"]{Database}
◊doclink["math"]{Math & statistics}
◊link["https://docs.racket-lang.org"]{full list ◊begin['rarr]}
}
}


◊feature["Cross-platform" #:id "f6"]{Racket runs on Linux, macOS, and Windows. Develop on one; deploy to all three.
◊doclinks{
◊link["https://docs.racket-lang.org/pkg/index.html"]{Package system}
◊link["https://docs.racket-lang.org/framework/index.html"]{GUI framework}
◊link["https://docs.racket-lang.org/raco/exe.html"]{Standalone binaries}
◊link["https://docs.racket-lang.org/foreign/index.html"]{Foreign interface}
}
}


◊feature["Powerful macros & languages" #:id "f5"]{Racket's crown jewel is its macro system, which lets you freely extend the language.
◊doclinks{
◊link["https://docs.racket-lang.org/guide/macros.html"]{Intro to macros}
◊link["https://docs.racket-lang.org/reference/Macros.html"]{Macros in depth}
◊link["https://docs.racket-lang.org/reference/syntax-model.html"]{Racket syntax model}
◊link["https://docs.racket-lang.org/guide/hash-languages.html"]{Making new languages}
}
}



◊feature["Mature, stable, open source" #:id "f2"]{Racket is a mature LGPL project that's actively developed and maintained.
◊doclinks{
◊link["https://github.com/racket"]{Racket repositories}
◊link["https://github.com/racket/racket"]{Main repository}
◊link["plt.html"]{The PLT Group}
◊link["https://groups.google.com/forum/#!forum/racket-users"]{Racket mailing list}
}
}



◊feature["DrRacket IDE & tons of documentation" #:id "f3"]{DrRacket is a graphical IDE that's integrated with Racket's vast documentation.
◊doclinks{
◊doclink["quick"]{DrRacket tutorial}
◊doclink["drracket"]{DrRacket guide}
◊doclink["drracket-tools"]{DrRacket tools}
◊doclink["scribble"]{Scribble}
}
}


◊feature["The best of Scheme and Lisp" #:id "f4"]{Racket started life as a Scheme implementation, but then grew into new areas.
◊doclinks{
◊link["http://felleisen.org/matthias/manifesto/"]{The Racket Manifesto}
◊link["https://docs.racket-lang.org/guide/to-scheme.html"]{Racket essentials}
◊link["https://docs.racket-lang.org/style/index.html"]{How to program Racket}
◊doclink["r6rs"]{Using R6RS Scheme}
}
}
}

◊special-section[#:class "one-column-body-text" #:id "pull-quote"]{Racket is a ◊link["https://docs.racket-lang.org/quick/index.html"]{general-purpose programming language} as well as the ◊link["http://felleisen.org/matthias/manifesto/"]{world's first ecosystem} for language-oriented programming. Make your ◊link["https://docs.racket-lang.org/guide/languages.html"]{dream language}, or use one of the dozens ◊link["http://docs.racket-lang.org/search/index.html?q=H%3A"]{already available}, including these —}


◊special-section{

◊lang["#lang racket" #:id "lang1"]{
◊pre{(◊docs{require} ◊link["https://docs.racket-lang.org/teachpack/2htdpimage.html?q=2htdp%2Fimage"]{2htdp/image}) ; draw a picture
(◊docs{let} sierpinski ([n 8])
  (◊docs{cond}
    [(◊docs{zero?} n) (◊docs{triangle} 2 'solid 'red)]
    [else (◊docs{define} t (sierpinski (- n 1)))
          (◊docs{freeze} (◊docs{above} t (◊docs{beside} t t)))]))}

The ◊code{2htdp/image} library provides easy-to-use functions for making images.

◊doclink["quick"]{Racket tutorial ◊(begin 'rarr)}}

◊lang["#lang scribble/base" #:id "lang2"]{
◊pre{@; Generate a PDF or HTML document
@◊docs{title}{Bottles: @◊docs{italic}{Abridged}}
@(◊docs{apply}
  ◊docs{itemlist}
  (◊docs{for/list} ([n (◊docs{in-range} 100 0 -1)])
    @◊docs{item}{@(◊docs{format} "~a" n) bottles.}))}

The ◊code{scribble/base} language generates documents using a prose-friendly syntax.

◊link["https://docs.racket-lang.org/scribble/getting-started.html"]{Scribble tutorial ◊(begin 'rarr)}
}

◊lang["#lang typed/racket" #:id "lang3"]{
◊pre{;; Using higher-order occurrence typing
(◊docs{define-type} SrN (◊docs{U} ◊docs{String} ◊docs{Number}))
(◊docs{:} tog ((◊docs{Listof} SrN) -> ◊docs{String}))
(◊docs{define} (tog l)
  (◊docs{apply} ◊docs{string-append} (◊docs{filter} ◊docs{string?} l)))
(tog (◊docs{list} 5 "hello " 1/2 "world" (◊docs{sqrt} -1)))}

Typed Racket's "gradual" typing lets you add types after you've been working in untyped mode.

◊link["https://docs.racket-lang.org/ts-guide/quick.html?q=typed%20racket"]{Typed Racket tutorial ◊(begin 'rarr)}

}

◊lang["#lang datalog" #:id "lang4"]{
◊pre{ancestor(A, B) :- parent(A, B).
ancestor(A, B) :-
  parent(A, C), ancestor(C, B).
parent(john, douglas).
parent(bob, john).
ancestor(A, B)?}

You can use Racket to build other languages — like ◊code{datalog}, a logic-programming language.

◊link["https://docs.racket-lang.org/datalog/Tutorial.html?q=datalog"]{Datalog tutorial ◊(begin 'rarr)}}

◊lang["#lang racket/gui" #:id "lang5"]{
◊pre{(◊docs{define} f (◊docs{new} frame% [label "Guess"]))
(◊docs{define} n (◊docs{random} 5)) (◊docs{send} f show #t)
(◊docs{define} ((check i) btn evt)
  (◊docs{message-box} "." (◊docs{if} (◊docs{=} i n) "Yes" "No")))
(◊docs{for} ([i (◊docs{in-range} 5)])
  (◊docs{make-object} ◊docs{button%} (◊docs{~a} i) f (check i)))}

Racket's GUI language and libraries makes cross-platform applications easy.

◊link["https://docs.racket-lang.org/gui/windowing-overview.html?q=gui"]{GUI tutorial ◊(begin 'rarr)}
}

◊lang["#lang web-server/insta" #:id "lang6"]{
◊pre{;; A "hello world" web server
(◊docs{define} (start request)
  (◊docs{response/xexpr}
   '(html
     (head (title "Racket"))
     (body "Hello World"))))}

The ◊code{web-server/insta} language lets you quickly prototype server-side web applications.

◊link["https://docs.racket-lang.org/continue/index.html?q=web%20applications"]{Web-application tutorial ◊(begin 'rarr)}
}
}


◊section{
Software

◊link["https://download.racket-lang.org/"]{Download Racket v7.5}

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




◊section{
News

◊link["https://blog.racket-lang.org/2019/11/racket-v7-5.html"]{Racket version 7.5} is available.

◊link["https://racketfest.com"]{Racketfest}, a new Racket event, was held in Berlin, 17 August 2019.

◊link["https://school.racket-lang.org"]{Racket School 2019} and ◊link["https://con.racket-lang.org"]{ninth RacketCon} were held in Salt Lake City in July 2019.

◊link["https://devswag.com/products/racket-t-shirt"]{Racket t-shirts} — the perfect way to meet friends, influence people, and stay warm.

◊link["https://devswag.com/products/racket"]{Racket stickers} — the indispensable accessory for laptops and textbooks.

}

◊section{
Community

◊link["https://lists.racket-lang.org/"]{Mailing list} and ◊link["https://blog.racket-lang.org/"]{blog}

◊link["https://botbot.me/freenode/racket/"]{#racket IRC} on freenode.net

◊link["https://racket.slack.com/"]{Slack channel} (visit ◊link["http://racket-slack.herokuapp.com/"]{this link} to sign up)

◊link["https://github.com/racket/racket/wiki"]{Wiki}

◊link["https://twitter.com/racketlang"]{@racketlang} on Twitter

◊link["team.html"]{Team}

◊link["sfc.html"]{Software Freedom Conservancy}

◊link["friendly.html"]{Friendly Environment policy}

◊link["sfc.html"]{Donate to Racket}
Make a tax-deductible contribution to support our work.

}


◊section{
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
A workshop to train teachers using ◊link["http://htdp.org/" #:style "color:gray"]{How to Design Programs} in the classroom.

◊link["http://www.bootstrapworld.org/"]{Bootstrap}
A curriculum for middle-school students.

}



◊section[#:id "bottom" #:class "one-column-body-text"]{
Thank you

To ◊link["http://www.nsf.gov/"]{the NSF}, ◊link["http://www.darpa.mil/"]{DARPA}, the ◊link["http://www.ed.gov/FIPSE/"]{Fund for the Improvement of Postsecondary Education (FIPSE)} at the ◊link["http://www.ed.gov/"]{US Department of Education}, the ◊link["http://www.exxonmobil.com/Corporate/community_foundation.aspx"]{Exxon Foundation}, CORD, partners of the Academy of Information Technology, ◊link["http://microsoft.com/"]{Microsoft}, ◊link["http://mozilla.org/"]{Mozilla}, and ◊link["http://google.com/"]{Google} for their generous support over the years.}
