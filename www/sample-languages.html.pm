#lang pollen

◊(require (only-in "index.html.pm" top))

◊(top)

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

◊lang["#lang typed/racket" #:id "lang3"]{
◊pre{;; Using higher-order occurrence typing
(◊docs{define-type} SrN (◊docs{U} ◊docs{String} ◊docs{Number}))
(◊docs{:} tog ((◊docs{Listof} SrN) -> ◊docs{String})
(◊docs{define} (tog l)
  (◊docs{apply} ◊docs{string-append} (◊docs{filter} ◊docs{string?} l)))
(tog (◊docs{list} 5 "hello " 1/2 "world" (◊docs{sqrt} -1)))}

Typed Racket's type system accommodates the idioms you know and love from working in plain Racket.

◊link["https://docs.racket-lang.org/ts-guide/quick.html?q=typed%20racket"]{Typed Racket tutorial ◊(begin 'rarr)}
}

◊lang["#lang scribble/base" #:id "lang2"]{
◊pre{@; Generate a PDF or HTML document
@◊docs{title}{Bottles: @◊docs{italic}{Abridged}}
@(◊docs{apply} 
  ◊docs{itemlist}
  (◊docs{for/list} ([n (◊docs{in-range} 100 0 -1)])
    @◊docs{item}{@(◊docs{format} "~a" n) bottles.}))}

The ◊code{scribble/base} language generates documents from a prose-friendly syntax and with full integration into the lexcial scope of Racket. 

◊link["https://docs.racket-lang.org/scribble/getting-started.html"]{Scribble tutorial ◊(begin 'rarr)}
}

◊lang["#lang datalog" #:id "lang4"]{
◊pre{ancestor(A, B) :- parent(A, B).
ancestor(A, B) :-
  parent(A, C), ancestor(C, B).
parent(john, douglas).
parent(bob, john).
ancestor(A, B)?}

You can use Racket to build totally unrelated languages, even if they have non-parenthetical syntax — like ◊code{datalog}, a logic-programming language.

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
