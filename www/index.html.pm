#lang pollen

◊top-section{
 ◊span[#:id "logo" #:style "font-size:2.3rem;white-space:nowrap;"]{
  ◊img[#:src "img/racket-logo.svg" #:class "logo"] Racket 
  ◊span[#:id "tagline" #:class "disappearing" #:style "font-size:70%;color:gray;white-space:nowrap;margin-left:0.2rem;"]{}}

◊div{

◊link[#:class "top-button disappearing-late" #:id "packages" "sfc.html"]{donate}

◊link[#:class "top-button disappearing-late" #:id "docs" "https://docs.racket-lang.org/"]{docs}

◊link[#:class "top-button disappearing-late" #:id "packages" "https://pkgs.racket-lang.org/"]{packages}

◊link[#:class "top-button disappearing-late" #:id "download" "https://download.racket-lang.org/"]{download}
}}

◊special-section[#:class "one-column-body-text" #:style "font-size:77%" #:id "pull-quote"]{

  ◊h4{News}

◊ul[#:class "doclinks"]{
 ◊li{◊link["https://blog.racket-lang.org/2019/05/racket-v7-3.html"]{Racket version 7.3} is available.}

 ◊li{◊link["https://school.racket-lang.org"]{Racket School 2019}---Learn how to make programming languages, July 8–-12 in Salt Lake City. Students, ask about Financial aid.} 

 ◊li{◊link["https://con.racket-lang.org/"]{ninth RacketCon} immediately follows the Racket School.}

 ◊li{Join us for both events in Salt Lake City!}}}


◊special-section[#:class "one-column-body-text w3-purple lop-system" #:style "padding:0.5rem;align:center" #:id "pull-quote"]{
 ◊span[#:id "lop-line" #:class "disappearing" #:style "font-size:110%;color:white;white-space:nowrap;text-align:center;"]{
  Racket, the Programming Language}}

◊div[#:class "w3-bar w3-card"]{
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('the-language',event,'mature')"]{◊div[#:style "mitem"]{Mature}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('the-language',event,'batteries')"]{◊div[#:style "mitem"]{With Batteries}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('the-language',event,'extensible')"]{◊div[#:style "mitem"]{Extensible}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('the-language',event,'strong')"]{◊div[#:style "mitem"]{Strong}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('the-language',event,'drracket')"]{◊div[#:style "mitem"]{Polished}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('the-language',event,'open')"]{◊div[#:style "mitem"]{With a Vibrant Community}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('the-language',event,'it')"]{◊div[#:style "mitem"]{}}
}

◊div[#:id "extensible" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

  ◊h4{Powerful Macros, Rich Tools for Building DSLs }

Racket is the first general-purpose programming language that empowers programmers to make domain-specific languages as if
they were plain libraries. No external tools, no make files required. 

◊doclinks{
◊link["https://docs.racket-lang.org/guide/macros.html"]{intro to macros}
◊link["https://docs.racket-lang.org/reference/Macros.html"]{macros in depth}
◊link["https://docs.racket-lang.org/guide/hash-languages.html"]{making new languages}
◊link["languages.html"]{sample #langs}}
}}

◊div[#:id "strong" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

  ◊h4{Gradually Typed, Powerful Software Contracts}

Racket is the first language to support higher-order software contracts and safe gradual typing. Programmers can easily deploy these tools to harden their software. 

◊doclinks{
◊link["https://docs.racket-lang.org/guide/contracts.html"]{software contracts}
◊link["https://docs.racket-lang.org/ts-guide/index.html"]{the Typed Racket guide}
◊link["https://www2.ccs.neu.edu/racket/pubs/typed-racket.pdf"]{gradual typing}}
}}

◊div[#:id "mature" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

  ◊h4{Mature, Stable, Cross-Platform}

Racket is a mature and stable product. From the beginning, it has  supported cross-platform graphical programming (Windows, macOS, Linux). 
 
◊doclinks{
◊link["https://docs.racket-lang.org/pkg/index.html"]{package system}
◊link["https://docs.racket-lang.org/framework/index.html"]{GUI framework}
◊link["https://docs.racket-lang.org/raco/exe.html"]{standalone binaries}
◊link["https://docs.racket-lang.org/foreign/index.html"]{foreign interface}}
}}

◊div[#:id "batteries" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

  ◊h4{Batteries Included}

Racket includes a rich set of libraries, covering the full range from web server apps to mathematics and scientific simulation software. 

◊doclinks{
◊doclink["web-server"]{web applications}
◊doclink["db"]{database}
◊doclink["math"]{math & statistics}
◊link["https://docs.racket-lang.org"]{full list ◊begin['rarr]}}
}}

◊div[#:id "drracket" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

  ◊h4{Editor Included, Emacs and VI Support, Extensive Documentation}

Racket comes with support for major editors. The main bundle includes an innovative and extensible interactive development environment that has inspired other IDE projects.
                                                            
◊doclinks{
◊doclink["drracket"]{DrRacket guide}
◊doclink["drracket-tools"]{DrRacket tools}
◊link["https://docs.racket-lang.org/guide/Vim.html"]{vim}
◊link["https://docs.racket-lang.org/guide/Emacs.html"]{emacs}}
}}

◊div[#:id "open" #:class "the-language" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{

  ◊h4{Vibrant Community, Open-Source All The Way}

Newcomers describe the on-line Racket community as extremely friendly and helpful. Everyone is welcome to ask any question and everybody is welcome to contribute to the code base. 

◊doclinks{
◊link["https://lists.racket-lang.org/"]{mailing list} 
◊link["https://blog.racket-lang.org/2017/09/tutorial-contributing-to-racket.html"]{contributing} 
◊link["https://twitter.com/racketlang"]{twitter}
◊link["https://github.com/racket/racket/"]{github}}
}}

◊div[#:id "it" #:class "the-language" #:style "display:block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{
   ◊table{
    ◊tr{
    ◊td{ }    ◊td{ }    ◊td{ }
    ◊td{ ◊img[#:src "http://users.cs.northwestern.edu/~robby/logos/racket-logo.svg" #:class "lop-image"]{}}}}
  }
}}

◊special-section[#:class "one-column-body-text w3-purple lop-system" #:style "padding:0.5rem;align:center" #:id "pull-quote"]{
 ◊span[#:class "disappearing" #:style "font-size:110%;color:white;white-space:nowrap;text-align:center;"]{
  Racket, the Language-Oriented Programming Language}}

◊div[#:class "w3-bar w3-card"]{
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('lop',event,'little-macros')"]{◊div[#:style "mitem"]{Little Macros}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('lop',event,'general-purpose')"]{◊div[#:style "mitem"]{General Purpose}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('lop',event,'big-macros')"]{◊div[#:style "mitem"]{Big Macros}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('lop',event,'hash-langs')"]{◊div[#:style "mitem"]{Hash Langs}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('lop',event,'ide-support')"]{◊div[#:style "mitem"]{IDE Support}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('lop',event,'any-syntax')"]{◊div[#:style "mitem"]{Any Syntax}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('lop',event,'more-it')"]{◊div[#:style "mitem"]{}}
}

◊div[#:id "little-macros" #:class "lop" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊h4{Little Macros}
  ◊img[#:src "img/little-macros.png" #:class "lop-image"]{}
  ◊div[#:class "container-fluid" #:style "font-size:77%"]{
  ◊p{Everybody should be afraid of C macros, and nobody should
   fear Racket macros. Since the Racket world deals
   with concrete syntax trees, macros are tree-rewriting
   rules, which avoids many of the syntactic problems of
   text-based macros in ordinary programming languages.}

  ◊p{Simple macros allow programmers to abstract over patterns
   when functions and procedures can't. No programmer should
   be forced to repeatedly write to add a binding
   to a functional programming language; 
   that's a pattern, and good software is free of those. 
   Likewise, nobody should have to work hard to abstract 
   over patterns in unit tests. Macros free you from all of this.}}
}}

◊div[#:id "general-purpose" #:class "lop"  #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
   ◊h4{General Purpose}
   ◊img[#:src "img/general-purpose-2-plot.png" #:class "lop-image"]{}
    ◊div[#:class "container-fluid"  #:style "font-size:77%"]{
    ◊p{Racket is a general-purpose programming language. 
    It comes with a range of built-in libraries, including 
    a comprehensive GUI toolbox. The GUI programs are highly 
    portable among the major platforms.}

    ◊p{When the built-in libraries don't get the job done, 
    look through the vast on-line catalog of user-contributed 
    packages. It comes with libraries for scientific simulations,
    video scripting, and web APIs. In all likelihood, you will 
    find something that gets you started on your project. 
    If all else fails, Racket's FFI makes it easy to program as
    if Racket were a parenthesized C---so linking in foreign 
    libraries is as easy as PI.}}
}}

◊div[#:id "big-macros" #:class "lop" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
   ◊h4{Big Macros}
   ◊img[#:src "img/big-macros.png" #:class "lop-image"]{}
   ◊div[#:class "container-fluid"  #:style "font-size:77%"]{
   ◊p{Getting to know the full Racket macro system will feel
      liberating, empowering, dazzling---like a whole new level 
      of enlightenment. It is far more powerful than Clojure's, Lisp's, 
      or Scheme's. In Racket, modules can export and import 
      macros. Developers can easily turn a collection of co-
      operating macros that equip Racket with facilities that 
      other languages have in their core.}

    ◊p{While Racket is a functional language, it has offered a
      sub-language of classes and objects from the beginning. 
      A Racket programmer can thus easily combine functional
      with object-oriented components. To mimic Java's class 
      system, 6,000 lines of macros suffice. And in this setting,
      ◊link["http://www.cs.utah.edu/plt/publications/aplas06-fff.pdf"]{mixins and traits} are free.}}
}}

◊div[#:id "hash-langs" #:class "lop" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊h4{Languages. Just make them.}
  ◊img[#:src "img/lazy-racket.png" #:class "lop-image"]{}
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{
  ◊p{Languages convey ideas, and some languages convey 
   ideas more easily than others. Programming languages 
   convey solutions, and some do it better than others.
   Racket isn't just a language, it's a whole bundle of
   them. And so, a Racket programmer writes every module 
   in the best possible language.}

   ◊p{Often ◊link["https://lang.video"]{an application domain} comes with several
   languages.  When you need a new language, you make it---
   on the fly. Open an IDE window; create a language right
   there, with just a few keystrokes; and run a module in 
   this new language in a second IDE window.  Making new 
   languages really requires no setup, no project files, 
   no external tools, no nothing.}  }
}}

◊div[#:id "ide-support" #:class "lop" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊h4{IDE Support for Languages}
  ◊img[#:src "img/ide-support.png" #:class "lop-image"]{}
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{
  ◊p{Racket comes with its own IDE, DrRacket, and it sports
   some unique features . For example, when a programmer 
   mouses over an identifier, the IDE draws an arrow back 
   to where it was defined.}

   ◊p{A programmer immediately benefits from DrRacket while
    using an alternative language, say Typed Racket.  Macros
    realize the syntactic aspects and ordinary function,
    classes, etc. implement the run-time system. Racket
    macros, even complex ones and those used to make new
    languages, record and propagate a sufficient amount of
    source information for DrRacket to act as if it understood
    the features of the new language.  }}
}}

◊div[#:id "any-syntax" #:class "lop" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊h4{Any Syntax}
  ◊img[#:src "img/ugly-syntax.png" #:class "lop-image"]{}
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{
  ◊p{Real Racket programmers love parentheses, but they
   have empathy for those few who need commas and braces.
   Hence, building languages with conventional surface
   syntax is almost as easy as building beautiful languages.}

  ◊p{Racket's ecosystem comes with parsing packages that
  allow developers to easily map any syntax to a
  parenthesized language, which is then compiled to
  ordinary Racket with the help of Racket's macro system.
  Such a language can also exploit the hooks of the IDE
  framework, so that its programmers may take advantage
  of Racket's IDE.}}
}}

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

◊special-section[#:class "one-column-body-text w3-purple lop-system" #:style "padding:0.5rem;align:center" #:id "pull-quote"]{
 ◊span[#:id "lop-line" #:class "disappearing" #:style "font-size:110%;color:white;white-space:nowrap;text-align:center;"]{
  Racket, the Ecosystem}}

◊div[#:class "w3-bar w3-card"]{
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('eco',event,'software')"]{◊div[#:style "mitem"]{Software}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('eco',event,'tutorials')"]{◊div[#:style "mitem"]{Tutorials & Documentation}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('eco',event,'community')"]{◊div[#:style "mitem"]{Community}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('eco',event,'books')"]{◊div[#:style "mitem"]{Books}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('eco',event,'education')"]{◊div[#:style "mitem"]{Education}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('eco',event,'stuff')"]{◊div[#:style "mitem"]{Gear & Stuff}}
  ◊button[#:class "w3-bar-item w3-button tablink" #:onclick "openTab('eco',event,'us')"]{◊div[#:style "mitem"]{}}
}

◊div[#:id "software" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{

  ◊h4{Software}


  ◊table{
    ◊tr{

    ◊td{ ◊p{

      ◊link["https://download.racket-lang.org/"]{Download Racket v7.3}

      ◊link["https://github.com/racket/racket/"]{Source code}

      ◊link["https://github.com/racket/racket/issues"]{Bug reports}

      ◊link["https://pre.racket-lang.org/installers/"]{Nightly snapshot builds}

      ◊link["https://pkgs.racket-lang.org/"]{Packages}}}

    ◊td{ }

    ◊td{◊img[#:src "img/il-grande-racket.png" #:class "lop-image"]{} }}}}}}

◊div[#:id "tutorials" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{

  ◊h4{Documentation and Tutorials}

  ◊table{
    ◊tr{

    ◊td{ ◊p{

      ◊link["https://docs.racket-lang.org/quick/"]{Quick introduction}

      ◊link["https://docs.racket-lang.org/more/"]{Systems programming}

      ◊link["https://docs.racket-lang.org/guide/"]{The Racket guide}

      ◊link["https://docs.racket-lang.org/reference/"]{The Racket reference}

      ◊link["https://docs.racket-lang.org/continue/"]{Web applications}

      ◊link["https://docs.racket-lang.org"]{All documentation}}}

    ◊td{ }
    ◊td{ }
    ◊td{ }
    ◊td{ }
    ◊td{ }

    ◊td{◊img[#:src "img/racket-guide.png" #:class "lop-image"]{} }}}}}}

◊div[#:id "stuff" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{

  ◊h4{Gear and Stuff}

   ◊table{
    ◊tr{

    ◊td{ ◊p{

      ◊link["https://devswag.com/products/racket-t-shirt"]{Racket t-shirts} — the perfect way to meet friends, influence people, and stay warm.

      ◊link["https://devswag.com/products/racket"]{Racket stickers} — the indispensable accessory for laptops and textbooks.}}

    ◊td{ }

    ◊td{◊img[#:src "img/gear-and-stuff.png" #:class "lop-image"]{} }}}}}}

◊div[#:id "community" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{

  ◊h4{Community}

   ◊table{
    ◊tr{

    ◊td{ ◊p{

      ◊link["https://lists.racket-lang.org/"]{Mailing list} and ◊link["https://blog.racket-lang.org/"]{blog}

      ◊link["https://botbot.me/freenode/racket/"]{#racket IRC} on freenode.net

      ◊link["https://racket.slack.com/"]{Slack channel} (visit ◊link["http://racket-slack.herokuapp.com/"]{this link} to sign up)

      ◊link["https://twitter.com/racketlang"]{@racketlang} on Twitter

      ◊link["team.html"]{Team}
      Racket's development benefits from a large distributed pool of contributors. 

      ◊link["sfc.html"]{Software Freedom Conservancy}
      Make a tax-deductible contribution to support our work.}}

    ◊td{ }

    ◊td{◊img[#:src "img/racket-con-2018.png" #:class "lop-image"]{} }}}}}}


◊div[#:id "books" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{

  ◊h4{Books}

   ◊table{
    ◊tr{

    ◊td{ ◊p{

      ◊link["https://www.realmofracket.com/"]{Realm of Racket}
      Learn to program with Racket, one game at a time.

      ◊link["https://beautifulracket.com/"]{Beautiful Racket}
      Make your own programming languages with Racket.

      ◊link["http://serverracket.com"]{Server: Racket}
      Develop a web application with Racket.

      ◊link["books.html"]{All Racket books}}}

    ◊td{ }

    ◊td{◊img[#:src "img/beautiful-racket-cover.svg" #:class "lop-image"]{} }}}}}}

◊div[#:id "education" #:class "eco" #:style "display:none"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{

  ◊h4{Education}

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

    ◊td{◊img[#:src "img/four.png" #:class "lop-image"]{} }}}}}}

◊div[#:id "us" #:class "eco" #:style "display:block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{
   ◊table{
    ◊tr{
    ◊td{ }    ◊td{ }    ◊td{ }
    ◊td{ ◊img[#:src "img/racket-school-2018.png" #:class "lop-image-rs"]{}}}}}
}}

◊section[#:id "bottom" #:class "one-column-body-text" #:style "background:white;padding:0.5rem"]{
Thank you

To ◊link["http://www.nsf.gov/"]{the NSF}, ◊link["http://www.darpa.mil/"]{DARPA}, the ◊link["http://www.ed.gov/FIPSE/"]{Fund for the Improvement of Postsecondary Education (FIPSE)} at the ◊link["http://www.ed.gov/"]{US Department of Education}, the ◊link["http://www.exxonmobil.com/Corporate/community_foundation.aspx"]{Exxon Foundation}, CORD, partners of the Academy of Information Technology, ◊link["http://microsoft.com/"]{Microsoft}, ◊link["http://mozilla.org/"]{Mozilla}, and ◊link["http://google.com/"]{Google} for their generous support over the years.}

◊script[#:src "js/jquery.min.js"]{}

◊script{
function openTab(classname, evt, elementname) {
  var i;
 
  // mark last selected link in red, globally 
  var tablinks;
  tablinks = document.getElementsByClassName("tablink");
  for (i = 0; i < tablinks.length; i++) {
    tablinks[i].className = tablinks[i].className.replace(" w3-red", "");
  }
  evt.currentTarget.className += " w3-red";

  // show selected block, on a per section basis 
  var classes; 
  classes = document.getElementsByClassName(classname);
  for (i = 0; i < classes.length; i++) {
    classes[i].style.display = "none";
  }
  document.getElementById(elementname).style.display = "block";
}

}
