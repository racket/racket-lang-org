#lang pollen

◊(require racket-lang-org/www/util racket-lang-org/www/color racket/format)

◊provide[top]

◊(define (top)
  ◊top-section{
   ◊a[#:href "/" #:class "logo-anchor"]{
    ◊span[#:id "logo" #:style "font-size:2.3rem;white-space:nowrap;"]{
     ◊img[#:alt "small logo" #:src "img/racket-logo.svg" #:class "logo"] Racket}}

  ◊div[#:class "margin-above-if-smartphone"]{
   ◊link[#:class "top-button" #:id "packages" "sfc.html"]{donate}
   ◊link[#:class "top-button" #:id "docs" "https://docs.racket-lang.org/"]{docs}
   ◊link[#:class "top-button" #:id "packages" "https://pkgs.racket-lang.org/"]{packages}
   ◊link[#:class "top-button" #:id "download" "https://download.racket-lang.org/"]{download}
}})

◊[define drracket-jfp "https://www2.ccs.neu.edu/racket/pubs/jfp01-fcffksf.pdf"]
◊[define pattern-macros "https://docs.racket-lang.org/guide/pattern-macros.html"]
◊[define racket/gui "https://docs.racket-lang.org/gui/"]
◊[define drracket-docs "https://docs.racket-lang.org/drracket/"]
◊[define web-server "https://docs.racket-lang.org/web-server/index.html?q=%23lang%20web"]
◊[define packages "https://pkgs.racket-lang.org"]
◊[define raco-pkg "https://docs.racket-lang.org/pkg/getting-started.html?q=raco%20pkg#%28part._installing-packages%29"]
◊[define racket/match "https://docs.racket-lang.org/reference/match.html"]
◊[define 2htdp/big-bang "https://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=big-bang#%28part._world._interactive%29"]
◊[define miniKanren "https://pkgs.racket-lang.org/package/faster-minikanren"]
◊[define traits-aplas "http://www.cs.utah.edu/plt/publications/aplas06-fff.pdf"]
◊[define hash-langs "https://docs.racket-lang.org/guide/hash-languages.html"]
◊[define lang-video "https://lang.video"]
◊[define typed/racket "https://docs.racket-lang.org/ts-guide/index.html"]
◊[define datalog "https://docs.racket-lang.org/datalog/datalog.html?q=datalog"]
◊[define br-parsing "https://docs.racket-lang.org/br-parser-tools/index.html?q=parsing"]

◊; ---------------------------------------------------------------------------------------------------

◊(top)

◊special-section[#:class "one-column-body-text" #:style "font-size:77%" #:id "pull-quote"]{
◊div[#:class "container-fluid vertical-in-smartphones-horizontal-otherwise"]{
 ◊div[#:style "margin-left: 1em"]{
   ◊link["https://blog.racket-lang.org/2023/05/racket-v8-9.html"]{Racket version 8.9} is available.}

 ◊div[#:style "flex-grow:1"]{}

 ◊div[#:style "margin-right: 1em"]{◊link["https://con.racket-lang.org/"]{RacketCon 2023} is October 28-29 in Chicago}
}}

◊div[#:class "frontpage-card"]{
◊special-section[#:class "one-column-body-text lop-system"
                 #:style (~a "padding:0.5rem;align:center;"
                             "background-color: " tab-heading-color ";")
                 #:id "pull-quote"]{
 ◊span[#:id "lop-line" #:class "frontpage-bar-heading frontpage-bar-nowrap-unless-smartphone"
       #:onclick "openTab('the-language',event,'it','langtablink',false)"]{
  Racket, the Programming Language}}
◊div[#:class "frontpage-bar disappearing-late"]{
  ◊button[#:class "frontpage-bar-item frontpage-button unselected-tab langtablink" #:onclick "openTab('the-language',event,'mature','langtablink',true)"]{◊div[#:style "mitem"]{Mature}}
  ◊button[#:class "frontpage-bar-item frontpage-button unselected-tab langtablink" #:onclick "openTab('the-language',event,'batteries','langtablink',true)"]{◊div[#:style "mitem"]{Practical}}
  ◊button[#:class "frontpage-bar-item frontpage-button unselected-tab langtablink" #:onclick "openTab('the-language',event,'extensible','langtablink',true)"]{◊div[#:style "mitem"]{Extensible}}
  ◊button[#:class "frontpage-bar-item frontpage-button unselected-tab langtablink" #:onclick "openTab('the-language',event,'strong','langtablink',true)"]{◊div[#:style "mitem"]{Robust}}
  ◊button[#:class "frontpage-bar-item frontpage-button unselected-tab langtablink" #:onclick "openTab('the-language',event,'drracket','langtablink',true)"]{◊div[#:style "mitem"]{Polished}}
}}

◊div[#:id "it" #:class "the-language selected-block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:77%"]{
   ◊div[#:class "translations-code-example-container"]{
    ◊div[#:class "center-if-smartphone"]{
        ◊img[#:alt "large logo"
             #:src "img/racket-logo.svg"
             #:class "big-logo"]{}
       }
      ◊div[#:class "code-to-right-of-big-logo"]{◊langwww["#lang racket/gui"]{
◊pre{
(◊docs{define} my-language 'English)

(◊docs{define} translations
  #hash([Chinese . "你好 世界"]
        [English . "Hello world"]
        [French . "Bonjour le monde"]
        [German . "Hallo Welt"]
        [Greek . "Γειά σου, κόσμε"]
        [Portuguese . "Olá mundo"]
        [Spanish . "Hola mundo"]
        [Thai . "สวัสดีชาวโลก"]
        [Turkish . "Merhaba Dünya"]))

(◊docs{define} my-hello-world
  (◊docs{hash-ref} translations my-language
            "hello world"))

(◊docs{message-box} "" my-hello-world)}
}}}}}}

◊div[#:id "mature" #:class "the-language full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{

Racket is a mature and stable product. From the beginning, it has  supported cross-platform graphical programming (Windows, macOS, Linux).

◊doclinks{
◊link["https://docs.racket-lang.org/pkg/index.html"]{Package System}
◊link["https://docs.racket-lang.org/framework/index.html"]{GUI Framework}
◊link["https://docs.racket-lang.org/raco/exe.html"]{Standalone Binaries}
◊link["https://docs.racket-lang.org/foreign/index.html"]{Foreign Interface}}
}}

◊div[#:id "batteries" #:class "the-language full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{

Racket includes a rich set of libraries, covering the full range from web server apps to mathematics and scientific simulation software.

◊doclinks{
◊doclink["web-server"]{Web Applications}
◊doclink["db"]{Database}
◊doclink["math"]{Math & Statistics}
◊link["https://docs.racket-lang.org"]{Full List ◊begin['rarr]}}
}}

◊div[#:id "extensible" #:class "the-language full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{

In Racket, programmers define their own loops with ◊strong{powerful macros}. Indeed, these macros are so powerful that programmers make entire ◊strong{domain-specific languages} as libraries. No tools, no Makefiles required.

◊doclinks{
◊link["https://docs.racket-lang.org/guide/macros.html"]{Intro To Macros}
◊link["https://docs.racket-lang.org/reference/Macros.html"]{Macros In Depth}
◊link["https://docs.racket-lang.org/guide/hash-languages.html"]{Making New Languages}
◊link["languages.html"]{Sample #Langs}}
}}

◊div[#:id "strong" #:class "the-language full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{

Racket is the first language to support ◊strong{higher-order software contracts} and ◊strong{safe gradual typing}. Programmers can easily deploy these tools to harden their software.

◊doclinks{
◊link["https://docs.racket-lang.org/guide/contracts.html"]{The Contract Guide}
◊link["https://www2.ccs.neu.edu/racket/pubs/icfp2002-ff.pdf"]{High-Order Contracts}
◊link["https://docs.racket-lang.org/ts-guide/index.html"]{The Typed Racket Guide}
◊link["https://www2.ccs.neu.edu/racket/pubs/typed-racket.pdf"]{Gradual Typing}}
}}

◊div[#:id "drracket" #:class "the-language full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{

Racket comes with support for major editors. The main bundle includes an innovative and extensible interactive development environment that has inspired other IDE projects.

◊doclinks{
◊doclink["drracket"]{DrRacket Guide}
◊link["https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket"]{VS Code/Magic Racket}
◊link["https://docs.racket-lang.org/guide/Emacs.html"]{Emacs Integration}
◊link["https://docs.racket-lang.org/guide/Vim.html"]{Vim Integration}
}
}}

◊div[#:class "frontpage-card"]{
◊special-section[#:class "one-column-body-text lop-system"
                 #:style (~a "padding:0.5rem;align:center;background-color:" tab-heading-color ";")
                 #:id "pull-quote"]{
 ◊span[#:class "frontpage-bar-heading frontpage-bar-nowrap-unless-smartphone"
       #:onclick "openTab('lop',event,'more-it','loptablink',false)"]{
  Racket, the Language-Oriented Programming Language}}
◊div[#:class "frontpage-bar disappearing-late"]{
  ◊button[#:class "frontpage-bar-item frontpage-button unselected-tab loptablink" #:onclick "openTab('lop',event,'little-macros','loptablink',true)"]{◊div[#:style "mitem"]{Little Macros}}
  ◊button[#:class "frontpage-bar-item frontpage-button unselected-tab loptablink" #:onclick "openTab('lop',event,'general-purpose','loptablink',true)"]{◊div[#:style "mitem"]{General Purpose}}
  ◊button[#:class "frontpage-bar-item frontpage-button unselected-tab loptablink" #:onclick "openTab('lop',event,'big-macros','loptablink',true)"]{◊div[#:style "mitem"]{Big Macros}}
  ◊button[#:class "frontpage-bar-item frontpage-button unselected-tab loptablink" #:onclick "openTab('lop',event,'hash-langs','loptablink',true)"]{◊div[#:style "mitem"]{Easy DSLs}}
  ◊button[#:class "frontpage-bar-item frontpage-button unselected-tab loptablink" #:onclick "openTab('lop',event,'ide-support','loptablink',true)"]{◊div[#:style "mitem"]{IDE Support}}
  ◊button[#:class "frontpage-bar-item frontpage-button unselected-tab loptablink" #:onclick "openTab('lop',event,'any-syntax','loptablink',true)"]{◊div[#:style "mitem"]{Any Syntax}}
}}

◊div[#:id "more-it" #:class "lop selected-block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid vertical-in-smartphones-horizontal-otherwise"
       #:style "font-size:77%"]{
    ◊div[#:class "block-with-1em-margin"]{
◊langwww["#lang typed/racket" #:id "lang3"]{
◊pre{;; Using higher-order occurrence typing
(◊docs{define-type} SrN (◊docs{U} ◊docs{String} ◊docs{Number}))
(◊docs{:} tog ((◊docs{Listof} SrN) -> ◊docs{String}))
(◊docs{define} (tog l)
  (◊docs{apply} ◊docs{string-append}
         (◊docs{filter} ◊docs{string?} l)))
(tog (◊docs{list} 5 "hello "
           1/2 "world" (◊docs{sqrt} -1)))}}}

    ◊div[#:class "block-with-1em-margin"]{
◊langwww["#lang scribble/base" #:id "lang2"]{
◊pre{@; Generate a PDF or HTML document
@(◊docs{require} (◊docs{only-in} racket ~a))
@(◊docs{define} N 99)
@◊docs{title}{Bottles: @◊docs{italic}{Abridged}}
@(◊docs{apply}
  ◊docs{itemlist}
  (◊docs{for/list} ([n (◊docs{in-range} N 0 -1)])
    @◊docs{item}{@(◊docs{~a} n) bottles.}))}}}

    ◊div[#:class "block-with-1em-margin"]{
◊langwww["#lang datalog" #:id "lang4"]{
◊pre{ancestor(A, B) ◊link["https://docs.racket-lang.org/datalog/interop.html?q=%3A-#%28form._%28%28lib._datalog%2Fmain..rkt%29._~3a-%29%29"]{:-} parent(A, B).
ancestor(A, B) ◊link["https://docs.racket-lang.org/datalog/interop.html?q=%3A-#%28form._%28%28lib._datalog%2Fmain..rkt%29._~3a-%29%29"]{:-}
  parent(A, C), ancestor(C, B).
parent(john, douglas).
parent(bob, john).
ancestor(A, B)?}}}

}}}

◊div[#:id "little-macros" #:class "lop full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{

   ◊div[#:class "vertical-in-smartphones-horizontal-otherwise"]{
    ◊div[#:class "block-with-1em-margin"]{
◊langwww["#lang racket" #:style "font-size:75%"]{
◊pre{
(◊docs{provide} time-it)

(◊docs{require} (◊docs{for-syntax} syntax/parse))

(◊docs{define-syntax} (time-it stx)
  (◊docs{syntax-parse} stx
    [(_ task)
     #'(thunk-time-it (◊docs{λ} () task))]))

(◊docs{define} (thunk-time-it task)
  (◊docs{define} before (cim))
  (◊docs{define} answer (task))
  (◊docs{define} delta  (- (cim) before))
  (◊docs{printf} "time: ~a ms\n" delta)
  answer)

(◊docs{define} cim current-inexact-milliseconds)
}}}
    ◊; this needs to all be on one line because of the enclosing use of `special-section`
    ◊; which (eventually) calls `decode-paragraphs`. Not sure if this a good idea of not
    ◊; but I'll just go with it. This happens in more than a few places in the file.

    ◊div{
    ◊p[#:class "block-with-1em-margin"]{
     Racket allows programmers to ◊link[pattern-macros]{add new syntactic constructs} in the same way that other languages permit the formulation of procedures, methods, or classes.  All you need to do is formulate a simple rule that rewrites a custom syntax to a Racket expression or definition.
    }
◊;{the `abc` tag is a random one; I'm not sure what is the right thing to put there but `div` doesn't work.}
    ◊p[#:class "block-with-1em-margin"]{
     Little macros can particularly help programmers with DRY where other features can't. The example ◊abc[#:class "disappear-if-smartphone"]{on the left} ◊abc[#:class "smartphone-only"]{above} shows how to define a new syntax for measuring the time a task takes. The syntax avoids the repeated use of lambda. Note also how the macro is exported from this module as if it were an ordinary function.
}}}}}

◊div[#:id "general-purpose" #:class "lop full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{
   ◊div[#:class "vertical-in-smartphones-horizontal-otherwise"]{
    ◊div[#:class "block-with-1em-margin"]{

◊langwww["#lang racket/gui" #:style "font-size:75%"]{
◊pre{;; let's play a guessing game

(◊docs{define} frame (◊docs{new} ◊docs{frame%} [label "Guess"]))

(◊docs{define} secret (◊docs{random} 5))
(◊docs{define} ((check i) btn evt)
  (◊docs{define} found? (◊docs{if} (◊docs{=} i secret) "Yes" "No"))
  (◊docs{message-box} "?" found?)
  (◊docs{when} (◊docs{=} i secret)
    (◊docs{send} frame show #false)))

(◊docs{for} ([i (◊docs{in-range} 5)])
   (◊docs{new} ◊docs{button%}
	[label (◊docs{~a} i)]
	[parent frame]
	[callback (check i)]))

(◊docs{send} frame show #t)}
}}

  ◊div{
    ◊p[#:class "block-with-1em-margin"]{
       Racket comes with a comprehensive suite of libraries: ◊link[racket/gui]{a cross-platform GUI toolbox}, a ◊link[web-server]{web server}, and more. ◊link[packages]{Thousands of additional packages} are a ◊link[raco-pkg]{single command} away: 3D graphics, a bluetooth socket connector, color maps, data structures, educational software, games, a quantum-random number generator, scientific simulations, web script testing, and many more.}

    ◊p[#:class "block-with-1em-margin"]{
      Macros work with these tools. The example ◊abc[#:class "disappear-if-smartphone"]{on the left} ◊abc[#:class "smartphone-only"]{above} shows the implementation of a small number-guessing game. It is implemented in the GUI dialect of Racket, and demonstrates a number of language features.}}

}}}


◊div[#:id "big-macros" #:class "lop full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{

   ◊div[#:class "container-fluid vertical-in-smartphones-horizontal-otherwise"]{
    ◊div[#:class "block-with-1em-margin"]{
      ◊img/size["big-macros-class.png" #:alt "Big Macros" #:size (cons 350 262) #:class "lop-image"]{}}

    ◊div{
     ◊p[#:class "block-with-1em-margin"]{
       Getting to know the full Racket macro system will feel liberating, empowering, dazzling—like a whole new level of enlightenment. Developers can easily create a collection of co-operating macros to implement ◊link[racket/match]{algebraic pattern matching}, simple ◊link[2htdp/big-bang]{event-handling}, or a ◊link[miniKanren]{logic-constraint solver}.}

     ◊p[#:class "block-with-1em-margin"]{
        While Racket is a functional language, it has offered a sub-language of ◊link[traits-aplas]{classes and objects, mixins and traits}, from the beginning. The macro-based implementation of a Java-like class system lives in a library and does not need any support from the core language. A Racket programmer can thus combine functional with object-oriented components as needed. }}

}}}

◊div[#:id "hash-langs" #:class "lop full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{

   ◊div[#:class "container-fluid vertical-in-smartphones-horizontal-otherwise"]{
    ◊div[#:class "block-with-1em-margin"]{
      ◊img/size["lang-video.png" #:alt "#lang video Dependency Graph" #:size (cons 350 350) #:class "lop-image"]{}}

    ◊div{
     ◊p[#:class "block-with-1em-margin"]{
        Some languages convey ideas more easily than others. And some programming languages convey solutions better than others.  Therefore Racket is a language for ◊link[hash-langs]{making languages}, so that a programmer can write every module in a well-suited language.}

     ◊p[#:class "block-with-1em-margin"]{
        Often ◊link[lang-video]{an application domain} comes with several languages.  When you need a new language, you make it—on the fly. Open an IDE window; create a language right there, with just a few keystrokes; and run a module in this new language in a second IDE window.  Making new languages really requires no setup, no project files, no external tools, no nothing.}
}}}}

◊div[#:id "ide-support" #:class "lop full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{

   ◊div[#:class "container-fluid vertical-in-smartphones-horizontal-otherwise"]{
    ◊div[#:class "block-with-1em-margin"]{
      ◊img/size["ide-support.png" #:alt "IDE Support" #:scale .3 #:class "lop-image"]{}}

    ◊div{
     ◊p[#:class "block-with-1em-margin"]{
      Racket comes with its own IDE, ◊link[drracket-docs]{DrRacket} (◊link[drracket-jfp]{née DrScheme}), and it sports some unique features. For example, when a programmer mouses over an identifier, the IDE draws an arrow back to where it was defined.}

     ◊p[#:class "block-with-1em-margin"]{
      A programmer immediately benefits from DrRacket while using an alternative language, say ◊link[typed/racket]{Typed Racket}. Racket macros, even complex ones and those used to make new languages, record and propagate a sufficient amount of source information for DrRacket to act as if it understood the features of the new language.}}
}}}

◊div[#:id "any-syntax" #:class "lop full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{

   ◊div[#:class "container-fluid vertical-in-smartphones-horizontal-otherwise"]{
    ◊div[#:class "block-with-1em-margin"]{
      ◊img/size["ugly-syntax.png" #:alt "Dots and Colon-Pipes, too!" #:size (cons 350 280) #:class "lop-image"]{}}

    ◊div{
     ◊p[#:class "block-with-1em-margin"]{
       Racket programmers usually love parentheses, but they have empathy for those who need commas and braces.  Hence, building languages with conventional surface syntax, like that of ◊link[datalog]{datalog}, is almost as easy as building parenthetical languages.}

    ◊p[#:class "block-with-1em-margin"]{
      Racket's ecosystem comes with ◊link[br-parsing]{parsing packages} that allow developers to easily map any syntax to a parenthesized language, which is then compiled to ordinary Racket with the help of Racket's macro system. Such a language can also exploit the hooks of the IDE framework, so that its programmers may take advantage of Racket's IDE.}}
}}}

◊div[#:class "frontpage-card"]{
◊special-section[#:class "one-column-body-text lop-system"
                 #:style (~a "padding:0.5rem;align:center;background-color:" tab-heading-color ";")
                 #:id "pull-quote"]{
 ◊span[#:id "lop-line" #:class "frontpage-bar-heading frontpage-bar-nowrap-unless-smartphone"
       #:onclick "openTab('eco',event,'us','ecotablink',false)"]{
  Racket, the Ecosystem}}
◊div[#:class "frontpage-bar disappearing-late"]{
  ◊button[#:class "frontpage-bar-item unselected-tab frontpage-button ecotablink" #:onclick "openTab('eco',event,'software','ecotablink',true)"]{◊div[#:style "mitem"]{Software}}
  ◊button[#:class "frontpage-bar-item unselected-tab frontpage-button ecotablink" #:onclick "openTab('eco',event,'tutorials','ecotablink',true)"]{◊div[#:style "mitem"]{Tutorials & Documentation}}
  ◊button[#:class "frontpage-bar-item unselected-tab frontpage-button ecotablink" #:onclick "openTab('eco',event,'community','ecotablink',true)"]{◊div[#:style "mitem"]{Community}}
  ◊button[#:class "frontpage-bar-item unselected-tab frontpage-button ecotablink" #:onclick "openTab('eco',event,'books','ecotablink',true)"]{◊div[#:style "mitem"]{Books}}
  ◊button[#:class "frontpage-bar-item unselected-tab frontpage-button ecotablink" #:onclick "openTab('eco',event,'education','ecotablink',true)"]{◊div[#:style "mitem"]{Education}}
  ◊button[#:class "frontpage-bar-item unselected-tab frontpage-button ecotablink" #:onclick "openTab('eco',event,'stuff', 'ecotablink',true)"]{◊div[#:style "mitem"]{Swag}}
}}

◊div[#:id "us" #:class "eco selected-block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid scale-if-smartphone"]{
   ◊img[#:style "margin-left:auto;margin-right:auto;display:block;text-align:center"
        #:alt "eighth RacketCon, 2018"
        #:src "img/racket-con-2018.png" #:class "lop-image-rc8"]{}}
}}

◊div[#:id "software" #:class "eco full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

  ◊table{
    ◊tr{

    ◊td{ ◊div[#:class "smartphone-only"]{Software} ◊p{

      ◊link["https://download.racket-lang.org/"]{Download Racket}

      ◊link["https://github.com/racket/racket/"]{Source Code}

      ◊link["https://github.com/racket/racket/issues"]{Bug Reports}

      ◊link["https://pre.racket-lang.org/installers/"]{Nightly Snapshot Builds}

      ◊link["https://pkgs.racket-lang.org/"]{Packages}}}

    ◊td{ }

    ◊td{◊div[ #:class "disappear-if-smartphone"]{◊img/size["il-grande-racket.png" #:size (cons 350 350) #:alt "Il Grande Racket"]{}}}}}}}}

◊div[#:id "tutorials" #:class "eco full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

  ◊table{
    ◊tr{

    ◊td{ ◊div[#:class "smartphone-only"]{Tutorials & Documentation} ◊p{

      ◊link["https://docs.racket-lang.org/quick/"]{Quick Introduction}

      ◊link["https://docs.racket-lang.org/more/"]{Systems Programming}

      ◊link["https://docs.racket-lang.org/guide/"]{The Racket Guide}

      ◊link["https://docs.racket-lang.org/reference/"]{The Racket Reference}

      ◊link["https://docs.racket-lang.org/continue/"]{Web Applications}

      ◊link["https://docs.racket-lang.org"]{All Documentation}}}

    ◊td{ }

    ◊td{◊div[ #:class "disappear-if-smartphone"]{◊img/size["racket-guide.png" #:alt "The Guide"  #:size (cons 450 320)]{}}}}}}}}

◊div[#:id "community" #:class "eco full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

   ◊table{
    ◊tr{

    ◊td{ ◊div[#:class "smartphone-only"]{Community} ◊p{

      ◊link["https://racket.discourse.group"]{Discourse} and ◊link["https://discord.gg/6Zq8sH5"]{Discord}
      These are the most active places for Racketeers.

      ◊link["https://racket.slack.com/"]{Slack} (◊link["https://join.slack.com/t/racket/shared_invite/zt-1ionnhfs3-bhgf~jJctQVwTb_QnU_yzQ"]{sign up}), ◊link["https://kiwiirc.com/nextclient/irc.libera.chat/#racket"]{IRC}, ◊link["https://reddit.com/r/racket"]{Reddit}, and ◊link["https://lists.racket-lang.org/"]{Mailing lists}
      Racketeers are here, too!

      ◊link[#:rel "me" "https://functional.cafe/@racketlang"]{Mastodon}, ◊link["https://twitter.com/racketlang"]{Twitter}, and ◊link["https://blog.racket-lang.org/"]{Blog}
      Keep in touch.

      ◊link["https://github.com/racket/racket/wiki"]{Wiki} and ◊link["https://www.youtube.com/c/racketlang/playlists"]{YouTube}
      Learn more from articles and talks.

      ◊link["team.html"]{Team} and ◊link["https://docs.racket-lang.org/racket-build-guide/contribute.html"]{Contributing}
      Racket's development benefits from a large distributed pool of contributors.

      ◊link["friendly.html"]{Friendly Environment Policy}
      Applies to all Racket venues.

      ◊link["sfc.html"]{Software Freedom Conservancy}
      Make a tax-deductible contribution to support our work.}}

    ◊td{ }

    ◊td{◊div[ #:class "disappear-if-smartphone"]{◊img/size["racket-school-2018.png" #:alt "Racket School 2018" #:size (cons 450 322)]{}}}}}}}}


◊div[#:id "books" #:class "eco full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

   ◊table{
    ◊tr{

    ◊td{ ◊div[#:class "smartphone-only"]{Books} ◊p{

      ◊link["https://www.realmofracket.com/"]{Realm of Racket}
      Learn to program with Racket, one game at a time.

      ◊link["https://beautifulracket.com/"]{Beautiful Racket}
      Make your own programming languages with Racket.

      ◊link["https://serverracket.com"]{Server: Racket}
      Develop a web application with Racket.

      ◊link["books.html"]{All Racket Books}}}

    ◊td{ }

    ◊td{◊div[ #:class "disappear-if-smartphone"]{◊img/size["beautiful-racket-cover.svg" #:alt "Beautiful Racket" #:size (cons 350 350)]{}}}}}}}}

◊div[#:id "education" #:class "eco full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

   ◊table{
    ◊tr{

    ◊td{ ◊div[#:class "smartphone-only"]{Education} ◊p{

      ◊link["https://school.racket-lang.org"]{The Racket Summer School}
      a summer school for researchers, professionals, and (under)graduate students to the Racket philosophy of programming languages

      ◊link["http://programbydesign.org/"]{Program by Design (aka TeachScheme!)}
      a curriculum and training program for high school teachers and college faculty

      ◊link["http://www.bootstrapworld.org/"]{Bootstrap}
      a curriculum and training program for middle-school and high-school teachers}}

    ◊td{ }

    ◊td{◊div[#:class "disappear-if-smartphone"]{◊img/size["four.png" #:alt "The Four Amigos" #:size (cons 450 322)]{}}}}}}}}


◊div[#:id "stuff" #:class "eco full-mode-menu-content unselected-block"]{
 ◊special-section[#:class "one-column-body-text"]{
  ◊div[#:class "container-fluid"  #:style "font-size:90%"]{

   ◊table{
    ◊tr{

    ◊td{ ◊div[#:class "smartphone-only"]{Swag}◊p{◊link["https://devswag.com/products/racket-t-shirt"]{Racket T-Shirts} — the perfect way to meet friends, influence people, and stay warm.

      ◊link["https://devswag.com/products/racket"]{Racket Stickers} — the indispensable accessory for laptops and textbooks.}}

    ◊td[#:class "disappear-if-smartphone"]{ }
    ◊td[#:class "disappear-if-smartphone"]{◊div[#:class "disappear-if-smartphone"]{◊img/size["gear-and-stuff.jpg" #:alt "gear" #:scale .4]{}}}}}}}}

◊section[#:id "bottom" #:class "one-column-body-text" #:style "padding:0.5rem"]{
◊div{Thank you}
◊div{To ◊link["http://www.nsf.gov/"]{the NSF},
◊link["http://www.darpa.mil/"]{DARPA},
the ◊link["http://www.ed.gov/FIPSE/"]{Fund for the Improvement of Postsecondary Education (FIPSE)} at the ◊link["http://www.ed.gov/"]{US Department of Education},
the ◊link["http://www.exxonmobil.com/Corporate/community_foundation.aspx"]{Exxon Foundation},
CORD, partners of the Academy of Information Technology,
◊link["http://microsoft.com/"]{Microsoft},
◊link["http://mozilla.org/"]{Mozilla},
◊link["http://google.com/"]{Google},
and many ◊link["individuals.html"]{individuals}
for their generous support over the years.}}

◊script[#:src "js/jquery.min.js"]{}

◊script{
function openTab(classname, evt, elementname, othertabclassnames, turnredp) {
  var i;

  var tablinks;
  tablinks = document.getElementsByClassName(othertabclassnames);
  for (i = 0; i < tablinks.length; i++) {
    tablinks[i].className = tablinks[i].className
      .replace(" selected-tab", "")
      .replace(" unselected-tab","")
      + " unselected-tab";
  }
  if (turnredp) {
    evt.currentTarget.className = evt.currentTarget.className
      .replace(" unselected-tab", "") + " selected-tab";
  }

  // show selected block, on a per section basis
  var classes;
  classes = document.getElementsByClassName(classname);
  for (i = 0; i < classes.length; i++) {
    classes[i].className = classes[i].className
      .replace(" selected-block","")
      .replace(" unselected-block","") +
      " unselected-block";
  }
  document.getElementById(elementname).className =
    document.getElementById(elementname).className
      .replace(" selected-block","")
      .replace(" unselected-block","") +
      " selected-block";
}
}
