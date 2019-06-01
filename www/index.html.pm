#lang pollen

◊top-section{
 ◊span[#:id "logo" #:style "font-size:2.3rem;white-space:nowrap;"]{
  ◊img[#:alt "small logo" #:src "img/racket-logo.svg" #:class "logo"] Racket 
  ◊span[#:id "tagline" #:class "disappearing" #:style "font-size:70%;color:gray;white-space:nowrap;margin-left:0.2rem;"]{}}

◊div{

◊link[#:class "top-button disappearing-late" #:id "packages" "sfc.html"]{donate}

◊link[#:class "top-button disappearing-late" #:id "docs" "https://docs.racket-lang.org/"]{docs}

◊link[#:class "top-button disappearing-late" #:id "packages" "https://pkgs.racket-lang.org/"]{packages}

◊link[#:class "top-button disappearing-late" #:id "download" "https://download.racket-lang.org/"]{download}
}}

◊special-section[#:class "one-column-body-text" #:style "font-size:77%" #:id "pull-quote"]{

◊ul[#:class "doclinks"]{
 ◊li{◊link["https://blog.racket-lang.org/2019/05/racket-v7-3.html"]{Racket version 7.3} is available.}

  ◊li{◊link["https://thestrangeloop.com/2019/number-lang-wishful-thinking.html"]{Vlad Kozin will speak at Strange Loop} on language-oriented programming in Racket}

 ◊li{◊link["https://school.racket-lang.org"]{Racket School 2019}---Learn how to make programming languages, July 8–-12 in Salt Lake City. Students, ask about Financial aid.} 

 ◊li{◊link["https://con.racket-lang.org/"]{ninth RacketCon} immediately follows the Racket School.}

 ◊li{Join us for both events in Salt Lake City!}}}


◊div[#:class "w3-card"]{
◊special-section[#:class "one-column-body-text w3-purple lop-system" #:style "padding:0.5rem;align:center" #:id "pull-quote"]{
 ◊span[#:id "lop-line" #:class "disappearing" #:style "cursor:pointer;font-size:130%;color:white;white-space:nowrap;text-align:center;"
       #:onclick "openTab('the-language',event,'it',false)"]{
  Racket, the Programming Language}}
◊div[#:class "frontpage-bar"]{
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex2 w3-button tablink" #:onclick "openTab('the-language',event,'mature',true)"]{◊div[#:style "mitem"]{Mature}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex4 w3-button tablink" #:onclick "openTab('the-language',event,'batteries',true)"]{◊div[#:style "mitem"]{Batteries Included}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex3 w3-button tablink" #:onclick "openTab('the-language',event,'extensible',true)"]{◊div[#:style "mitem"]{Extensible}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex2 w3-button tablink" #:onclick "openTab('the-language',event,'strong',true)"]{◊div[#:style "mitem"]{Strong}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex2 w3-button tablink" #:onclick "openTab('the-language',event,'drracket',true)"]{◊div[#:style "mitem"]{Polished}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex5 w3-button tablink" #:onclick "openTab('the-language',event,'open',true)"]{◊div[#:style "mitem"]{With a Vibrant Community}}
}}

◊div[#:id "extensible" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

Racket is the first general-purpose programming language that empowers programmers to make ◊strong{domain-specific languages}  as libraries from ◊strong{powerful macros}. No external tools, no make files required. 

◊doclinks{
◊link["https://docs.racket-lang.org/guide/macros.html"]{Intro To Macros}
◊link["https://docs.racket-lang.org/reference/Macros.html"]{Macros In Depth}
◊link["https://docs.racket-lang.org/guide/hash-languages.html"]{Making New Languages}
◊link["languages.html"]{Sample #Langs}}
}}

◊div[#:id "strong" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

Racket is the first language to support ◊strong{higher-order software contracts} and ◊strong{safe gradual typing}. Programmers can easily deploy these tools to harden their software. 

◊doclinks{
◊link["https://docs.racket-lang.org/guide/contracts.html"]{The Contract Guide}
◊link["https://www2.ccs.neu.edu/racket/pubs/icfp2002-ff.pdf"]{High-Order Contracts}
◊link["https://docs.racket-lang.org/ts-guide/index.html"]{The Typed Racket Guide}
◊link["https://www2.ccs.neu.edu/racket/pubs/typed-racket.pdf"]{Gradual Typing}}
}}

◊div[#:id "mature" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

Racket is a mature and stable product. From the beginning, it has  supported cross-platform graphical programming (Windows, macOS, Linux). 
 
◊doclinks{
◊link["https://docs.racket-lang.org/pkg/index.html"]{Package System}
◊link["https://docs.racket-lang.org/framework/index.html"]{GUI Framework}
◊link["https://docs.racket-lang.org/raco/exe.html"]{Standalone Binaries}
◊link["https://docs.racket-lang.org/foreign/index.html"]{Foreign Interface}}
}}

◊div[#:id "batteries" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

Racket includes a rich set of libraries, covering the full range from web server apps to mathematics and scientific simulation software. 

◊doclinks{
◊doclink["web-server"]{Web Applications}
◊doclink["db"]{Database}
◊doclink["math"]{Math & Statistics}
◊link["https://docs.racket-lang.org"]{Full List ◊begin['rarr]}}
}}

◊div[#:id "drracket" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

Racket comes with support for major editors. The main bundle includes an innovative and extensible interactive development environment that has inspired other IDE projects.
                                                            
◊doclinks{
◊doclink["drracket"]{DrRacket Guide}
◊doclink["drracket-tools"]{DrRacket Tools}
◊link["https://docs.racket-lang.org/guide/Emacs.html"]{Emacs Integration}
◊link["https://docs.racket-lang.org/guide/Vim.html"]{VIM Integration}
}
}}

◊div[#:id "open" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

Newcomers describe the on-line Racket community as extremely ◊strong{friendly and helpful}. Everyone is welcome to ask any question and everybody is welcome to contribute to the ◊strong{open-source} code base.

◊doclinks{
◊link["https://lists.racket-lang.org/"]{Mailing List} 
◊link["https://blog.racket-lang.org/2017/09/tutorial-contributing-to-racket.html"]{Contributing} 
◊link["https://twitter.com/racketlang"]{Twitter}
◊link["https://github.com/racket/racket/"]{Github}}
}}

◊div[#:id "it" #:class "the-language" #:style "display:block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{
   ◊div[#:style "display:flex; justify-content: space-around; align-items: center;"]{
    ◊div{}
    ◊div[#:style "display:flex"]{
      ◊img[#:alt "large logo"
           #:src "img/racket-logo.svg"
           #:style "margin-right: 40px; width: 200px;height: 200px;"]{}

      ◊div{◊langwww["#lang racket/gui"]{
◊pre{;; let's play a guessing game
(◊docs{define} frame (◊docs{new} frame% [label "Guess"]))
(◊docs{define} n (◊docs{random} 5)) 
(◊docs{define} ((check i) btn evt)
  (◊docs{message-box} "?" (◊docs{if} (◊docs{=} i n) "Yes" "No")))
(◊docs{for} ([i (◊docs{in-range} 5)]) 
  (◊docs{make-object} ◊docs{button%} (◊docs{~a} i) frame (check i)))
(◊docs{send} frame show #t)}}}}

    ◊div{}}}}}

◊div[#:class "w3-card"]{
◊special-section[#:class "one-column-body-text w3-purple lop-system" #:style "padding:0.5rem;align:center" #:id "pull-quote"]{
 ◊span[#:class "disappearing" #:style "cursor:pointer;font-size:130%;color:white;white-space:nowrap;text-align:center;"
       #:onclick "openTab('lop',event,'more-it',false)"]{
  Racket, the Language-Oriented Programming Language}}
◊div[#:class "frontpage-bar"]{
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex3 w3-button tablink" #:onclick "openTab('lop',event,'little-macros',true)"]{◊div[#:style "mitem"]{Little Macros}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex4 w3-button tablink" #:onclick "openTab('lop',event,'general-purpose'),true"]{◊div[#:style "mitem"]{General Purpose}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex3 w3-button tablink" #:onclick "openTab('lop',event,'big-macros'),true"]{◊div[#:style "mitem"]{Big Macros}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex3 w3-button tablink" #:onclick "openTab('lop',event,'hash-langs'),true"]{◊div[#:style "mitem"]{Easy DSLs}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex3 w3-button tablink" #:onclick "openTab('lop',event,'ide-support'),true"]{◊div[#:style "mitem"]{IDE Support}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex3 w3-button tablink" #:onclick "openTab('lop',event,'any-syntax'),true"]{◊div[#:style "mitem"]{Any Syntax}}
}}

◊div[#:id "little-macros" #:class "lop" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

  ◊div[#:class "container-fluid"]{

   ◊table{
    ◊tr[#:valign "top"]{

    ◊td{ 
◊langwww["#lang racket" #:style "font-size:46%"]{
◊pre{;; Defining and using simple syntactic extensions 
(◊docs{require} racket/splicing)

;; define it ...
(◊docs{define-syntax-rule}
  (where definition 
         ((define (locally-defined-id x ...) body-id)
           ...))
  (◊docs{splicing-letrec} 
         ((locally-defined-id (lambda (x ...) body-id)) 
           ...)
         definition))

;; ... and use it immediately in the same module 
(where (define is-5-odd (odd? 5))
       {(define (odd? n)
           (if (= n 1) #t (even? (- n 1))))
        (define (even? n)
            (if (= n 0) #t (even? (- n 1))))})
  ;; { ... } are equivalent to ( ... )


;; then use the newly defined identifier
(if is-5-odd "five is odd" "five is not odd")}}}

    ◊td{ }

    ◊td{ 

     ◊p{Racket allows programmers to add new syntactic constructs
     in the same way that other languages permit the formulation
     of procedures, methods, or classes.  All you need to do is
     formulate a simple rule that rewrites a custom syntax to a
     Racket expression or definition.}

     ◊p{ Little macros can particularly help programmers
     with DRY (Don't Repeat Yourself) where other
     features can't. The example on the left shows how to
     define a ◊tt{where} construct for definitions with a
     few lines of code and how to use it immediately, in
     the same module.}}

   } } }}}

◊div[#:id "general-purpose" #:class "lop"  #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

   ◊div[#:class "container-fluid"]{
     ◊table{
      ◊tr[#:valign "top"]{

    ◊td{ 

◊langwww["#lang racket/gui" #:style "font-size:46%"]{
◊pre{;;a bi-dorectional temperature converter (Fahrenheit vs Celsius)

(◊docs{define} *C 0)
(◊docs{define} *F 0)

(◊docs{define-syntax-rule}
  (def-callback (name field *from convert *to))
  (◊docs{define} (name . x)
    (◊docs{define} field:val (◊docs{if} (◊docs{empty?} x) "0" (◊docs{send} (car x)get-value)))
    (◊docs{define} field:num (◊docs{string-number} field:val))
    (◊docs{when} field:num 
      (◊docs{set!-values} (*from *to) (◊docs{values} field:num (convert field:num)))
      (◊docs{send} C-field set-value (~a *C))
      (◊docs{send} F-field set-value (~a *F)))))

(◊docs{define} (field lbl cb)
  (◊docs{new} text-field% [parent pane] [label lbl] [init-value ""] [callback cb]))

(def-callback (C-2-F C-field *C (λ (c) (+ (* c 9/5) 32)) *F))
(def-callback (F-2-C F-field *F (λ (f) (* (- f 32) 5/9)) *C))

(◊docs{define} frame   (new frame% [label "temperature converter"]))
(◊docs{define} pane    (new horizontal-pane% [parent frame]))
(◊docs{define} C-field (field "celsius:"       C-2-F))
(◊docs{define} F-field (field " = fahrenheit:" F-2-C))

(C-2-F)
(◊docs{send} frame show #t)
}}}
  
    ◊td{ 

    ◊p{Racket comes with a comprehensive suite of
    libraries: a cross-platform GUI toolbox, a
    web server, data visualization, and more.
    Thousands of additional packages are a single
    command away: from video editing to scientific 
    simulations, from web testing to 3D graphics.}

    ◊p{Macros work with these tools. The example
    on the left shows how to use a little macro
    to define a macro for defining GUI callbacks,
    abstracting where a function or method
    couldn't. }}

}}}}}


◊div[#:id "big-macros" #:class "lop" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

   ◊table{
    ◊tr[#:valign "top"]{

    ◊td{ ◊img[#:src "img/big-macros.png" #:class "lop-image"]{} }

    ◊td{ ◊p{Getting to know the full Racket macro system will
       feel liberating, empowering, dazzling---like a whole
       new level of enlightenment. Developers can easily
       create a collection of co-operating macros to
       implement algebraic pattern matching, event-handling,
       or a logic-constraint solver.}

    ◊p{While Racket is a functional language, it has offered
      a sub-language of ◊link["http://www.cs.utah.edu/plt/publications/aplas06-fff.pdf"]{classes and objects, mixins 
      and traits}, from the beginning.  A Racket programmer
      can therefore combine functional with object-oriented
      components as needed. To mimic Java's class system,
      6,000-line macro library suffices. }}
}}}}}

◊div[#:id "hash-langs" #:class "lop" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

   ◊table{
    ◊tr[#:valign "top"]{

    ◊td{ ◊img[#:src "img/lazy-racket.png" #:class "lop-image"]{} }

    ◊td{ ◊p{Some languages convey ideas more easily
   than others. And some programming languages convey
   solutions better than others.  Therefore Racket is a
   language for making languages, so that a programmer
   can write every module in a well-suited languages.}

   ◊p{Often ◊link["https://lang.video"]{an application domain} comes with several
   languages.  When you need a new language, you make it---
   on the fly. Open an IDE window; create a language right
   there, with just a few keystrokes; and run a module in 
   this new language in a second IDE window.  Making new 
   languages really requires no setup, no project files, 
   no external tools, no nothing.}  }
}}}}}

◊div[#:id "ide-support" #:class "lop" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{
   ◊table{
    ◊tr[#:valign "top"]{

    ◊td{  ◊img[#:src "img/ide-support.png" #:class "lop-image"]{} }

    ◊td{
  ◊p{Racket comes with its own IDE, DrRacket, and it sports
   some unique features . For example, when a programmer 
   mouses over an identifier, the IDE draws an arrow back 
   to where it was defined.}

   ◊p{A programmer immediately benefits from DrRacket while
    using an alternative language, say Typed Racket. Racket
    macros, even complex ones and those used to make new
    languages, record and propagate a sufficient amount of
    source information for DrRacket to act as if it understood
    the features of the new language.  }}
}}}}}

◊div[#:id "any-syntax" #:class "lop" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{
   ◊table{
    ◊tr[#:valign "top"]{

    ◊td{ ◊img[#:src "img/ugly-syntax.png" #:class "lop-image"]{}}

    ◊td{

   ◊p{Real Racket programmers love parentheses, but they
      have empathy for those few who need commas and
      braces.  Hence, building languages with
      conventional surface syntax is almost as easy as
      building beautiful languages.}

  ◊p{Racket's ecosystem comes with parsing packages that
  allow developers to easily map any syntax to a
  parenthesized language, which is then compiled to
  ordinary Racket with the help of Racket's macro system.
  Such a language can also exploit the hooks of the IDE
  framework, so that its programmers may take advantage
  of Racket's IDE.}}
}}}}}

◊div[#:id "more-it" #:class "lop" #:style "display:block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{
   ◊table{
    ◊tr[#:valign "top"]{
    ◊td{ 
◊langwww["#lang typed/racket" #:id "lang3"]{
◊pre{;; Using higher-order occurrence typing
(◊docs{define-type} SrN (◊docs{U} ◊docs{String} ◊docs{Number}))
(◊docs{:} tog ((◊docs{Listof} SrN) -> ◊docs{String})
(◊docs{define} (tog l)
  (◊docs{apply} ◊docs{string-append} (◊docs{filter} ◊docs{string?} l)))
(tog (◊docs{list} 5 "hello " 1/2 "world" (◊docs{sqrt} -1)))}}}

    ◊td{ 
◊langwww["#lang scribble/base" #:id "lang2"]{
◊pre{@; Generate a PDF or HTML document
@(◊docs{require} (◊docs{only-in} racket ~a))
@(◊docs{define} N 99)
@◊docs{title}{Bottles: @◊docs{italic}{Abridged}}
@(◊docs{apply} 
  ◊docs{itemlist}
  (◊docs{for/list} ([n (◊docs{in-range} N 0 -1)])
    @◊docs{item}{@(◊docs{~a} n) bottles.}))}}}

    ◊td{ 
◊langwww["#lang datalog" #:id "lang4"]{
◊pre{ancestor(A, B) :- parent(A, B).
ancestor(A, B) :-
  parent(A, C), ancestor(C, B).
parent(john, douglas).
parent(bob, john).
ancestor(A, B)?}}}

}}}}}

◊div[#:class "w3-card"]{
◊special-section[#:class "one-column-body-text w3-purple lop-system" #:style "padding:0.5rem;align:center" #:id "pull-quote"]{
 ◊span[#:id "lop-line" #:class "disappearing" #:style "cursor:pointer;font-size:130%;color:white;white-space:nowrap;text-align:center;"
       #:onclick "openTab('eco',event,'us',false)"]{
  Racket, the Ecosystem}}
◊div[#:class "frontpage-bar"]{
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex3 w3-button tablink" #:onclick "openTab('eco',event,'software',true)"]{◊div[#:style "mitem"]{Software}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex6 w3-button tablink" #:onclick "openTab('eco',event,'tutorials',true)"]{◊div[#:style "mitem"]{Tutorials & Documentation}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex3 w3-button tablink" #:onclick "openTab('eco',event,'community',true)"]{◊div[#:style "mitem"]{Community}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex2 w3-button tablink" #:onclick "openTab('eco',event,'books',true)"]{◊div[#:style "mitem"]{Books}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex3 w3-button tablink" #:onclick "openTab('eco',event,'education',true)"]{◊div[#:style "mitem"]{Education}}
  ◊button[#:class "frontpage-bar-item frontpage-bar-flex3 w3-button tablink" #:onclick "openTab('eco',event,'stuff',true)"]{◊div[#:style "mitem"]{Gear & Stuff}}
}}

◊div[#:id "software" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

  ◊table{
    ◊tr{

    ◊td{ ◊p{

      ◊link["https://download.racket-lang.org/"]{Download Racket v7.3}

      ◊link["https://github.com/racket/racket/"]{Source Code}

      ◊link["https://github.com/racket/racket/issues"]{Bug Reports}

      ◊link["https://pre.racket-lang.org/installers/"]{Nightly Snapshot Builds}

      ◊link["https://pkgs.racket-lang.org/"]{Packages}}}

    ◊td{ }

    ◊td{◊img[#:alt "Il Grande Racket" #:src "img/il-grande-racket.png" #:class "lop-image"]{} }}}}}}

◊div[#:id "tutorials" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

  ◊table{
    ◊tr{

    ◊td{ ◊p{

      ◊link["https://docs.racket-lang.org/quick/"]{Quick Introduction}

      ◊link["https://docs.racket-lang.org/more/"]{Systems Programming}

      ◊link["https://docs.racket-lang.org/guide/"]{The Racket Guide}

      ◊link["https://docs.racket-lang.org/reference/"]{The Racket Reference}

      ◊link["https://docs.racket-lang.org/continue/"]{Web Applications}

      ◊link["https://docs.racket-lang.org"]{All Documentation}}}

    ◊td{ }

    ◊td{◊img[#:alt "The Guide" #:src "img/racket-guide.png" #:class "lop-image-rs"]{} }}}}}}

◊div[#:id "stuff" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

   ◊table{
    ◊tr{

    ◊td{ ◊p{

      ◊link["https://devswag.com/products/racket-t-shirt"]{Racket T-Shirts} — the perfect way to meet friends, influence people, and stay warm.

      ◊link["https://devswag.com/products/racket"]{Racket Stickers} — the indispensable accessory for laptops and textbooks.}}

    ◊td{ }

    ◊td{◊img[#:alt "gear" #:src "img/gear-and-stuff.png" #:class "lop-image"]{} }}}}}}

◊div[#:id "community" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

   ◊table{
    ◊tr{

    ◊td{ ◊p{

      ◊link["https://lists.racket-lang.org/"]{Mailing List} and ◊link["https://blog.racket-lang.org/"]{Blog}

      ◊link["https://botbot.me/freenode/racket/"]{#racket IRC} on freenode.net

      ◊link["https://racket.slack.com/"]{Slack channel} (Visit ◊link["http://racket-slack.herokuapp.com/"]{this link} to sign up)

      ◊link["https://twitter.com/racketlang"]{@racketlang} on Twitter

      ◊link["team.html"]{Team}
      Racket's development benefits from a large distributed pool of contributors. 

      ◊link["sfc.html"]{Software Freedom Conservancy}
      Make a tax-deductible contribution to support our work.}}

    ◊td{ }

    ◊td{◊img[#:alt "Racket School 2018" #:src "img/racket-school-2018.png" #:class "lop-image-rs"]{} }}}}}}


◊div[#:id "books" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

   ◊table{
    ◊tr{

    ◊td{ ◊p{

      ◊link["https://www.realmofracket.com/"]{Realm of Racket}
      Learn to program with Racket, one game at a time.

      ◊link["https://beautifulracket.com/"]{Beautiful Racket}
      Make your own programming languages with Racket.

      ◊link["http://serverracket.com"]{Server: Racket}
      Develop a web application with Racket.

      ◊link["books.html"]{All Racket Books}}}

    ◊td{ }

    ◊td{◊img[#:alt "Beautiful Racket" #:src "img/beautiful-racket-cover.svg" #:class "lop-image"]{} }}}}}}

◊div[#:id "education" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

   ◊table{
    ◊tr{

    ◊td{ ◊p{

      ◊link["https://school.racket-lang.org"]{The Racket Summer School}
      a summer school for researchers, professionals, and (under)graduate students to the Racket philosophy of programming languages 

      ◊link["http://programbydesign.org/"]{Program by Design (aka TeachScheme!)}
      a curriculum and training programfor high school teachers and college faculty
      
      ◊link["http://www.bootstrapworld.org/"]{Bootstrap}
      a curriculum and training program for middle-school and high-school teachers}}

    ◊td{ }

    ◊td{◊img[#:alt "The Four Amigos" #:src "img/four.png" #:class "lop-image-rs"]{} }}}}}}

◊div[#:id "us" #:class "eco" #:style "display:block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"]{
   ◊img[#:style "margin-left:auto;margin-right:auto;display:block;text-align:center"
        #:alt "eighth RacketCon, 2018"
        #:src "img/racket-con-2018.png" #:class "lop-image-rc8"]{}}
}}

◊section[#:id "bottom" #:class "one-column-body-text" #:style "background:white;padding:0.5rem"]{
Thank you

To ◊link["http://www.nsf.gov/"]{the NSF}, ◊link["http://www.darpa.mil/"]{DARPA}, the ◊link["http://www.ed.gov/FIPSE/"]{Fund for the Improvement of Postsecondary Education (FIPSE)} at the ◊link["http://www.ed.gov/"]{US Department of Education}, the ◊link["http://www.exxonmobil.com/Corporate/community_foundation.aspx"]{Exxon Foundation}, CORD, partners of the Academy of Information Technology, ◊link["http://microsoft.com/"]{Microsoft}, ◊link["http://mozilla.org/"]{Mozilla}, and ◊link["http://google.com/"]{Google} for their generous support over the years.}

◊script[#:src "js/jquery.min.js"]{}

◊script{
function openTab(classname, evt, elementname, turnredp) {
  var i;
 
  // mark last selected link in red, globally 
  var tablinks;
  tablinks = document.getElementsByClassName("tablink");
  for (i = 0; i < tablinks.length; i++) {
    tablinks[i].className = tablinks[i].className.replace(" w3-red", "");
  }
  if (turnredp) { evt.currentTarget.className += " w3-red"; }

  // show selected block, on a per section basis 
  var classes; 
  classes = document.getElementsByClassName(classname);
  for (i = 0; i < classes.length; i++) {
    classes[i].style.display = "none";
  }
  document.getElementById(elementname).style.display = "block";
}

}
