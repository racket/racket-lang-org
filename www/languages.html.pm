#lang pollen


◊top-section{
◊span[#:id "logo" #:style "font-size:2.3rem;white-space:nowrap;"]{◊link["index.html"]{◊img[#:src "img/racket-logo.svg" #:class "logo"] Racket} ◊span[#:id "tagline" #:class "disappearing" #:style "font-size:70%;color:gray;white-space:nowrap;margin-left:0.2rem;"]{}}

◊div{
◊link[#:class "top-button disappearing-late" #:id "docs" "https://docs.racket-lang.org/"]{docs}

◊link[#:class "top-button disappearing-late" #:id "packages" "https://pkgs.racket-lang.org/"]{packages}

◊link[#:class "top-button disappearing-late" #:id "download" "https://download.racket-lang.org/"]{download}
}
}

◊section{
◊div{Racket Languages}
◊p[#:style "font-size: 80%;margin-top: 1rem;color:gray;width:80%;line-height:1.5"]{
As the ◊link["http://felleisen.org/matthias/manifesto/"]{world's first ecosystem} for language-oriented programming, Racket has been used to create dozens of programming languages.}
}
◊section{How to find them?
Languages added to the Racket package system are automatically added to the Racket Documentation.
}
◊section{What languages are here?
The following is a selection of of languages built with Racket including languages with different syntax (Java, Algol 60), semantics(Racklog and Datalog), tooling (ffmpeg for Video and solvers for Rosette), and targets other than the racket platform (Javascript, 6502).
}
◊section{
◊div{Languages}

◊link["https://docs.racket-lang.org/algol60/"]{Algol 60}

The “Algol 60” language for DrRacket implements the language defined by the “Revised Report on the Algorithmic Language Algol 60,” edited by Peter Naur.

◊link["https://docs.racket-lang.org/asi64/"]{Asi64}

Asi64 is a cross-platform 6502 assembler. Primarily aimed at programming the Commodore 64, with VICE emulator support.

◊link["https://docs.racket-lang.org/datalog/"]{Datalog: Deductive Database Programming}

Datalog is a declarative logic language in which each formula is a function-free Horn clause, and every variable in the head of a clause must appear in the body of the clause. Datalog is also a lightweight deductive database system where queries and database updates are expressed in the logic language.

◊link["https://lexi-lambda.github.io/hackett/"]{The Hackett Programming Language}

Hackett is a purely functional, statically typed, lazily evaluated programming language. It provides powerful, bidirectional type inference, algebraic datatypes, pattern matching, typeclasses, and higher-rank polymorphism.


 ◊link["https://docs.racket-lang.org/heresy/"]{The Heresy Programming Language}

The Heresy language is a functional Lisp/Scheme dialect implemented in Racket, with syntax inspired by the BASIC family of programming languages.


 ◊link["https://docs.racket-lang.org/lindenmayer/"]{#lang lindenmayer, a language for L-Systems}

The Lindenmayer language provides a language for running and interpreting Lindenmayer Systems.


 ◊link["https://docs.racket-lang.org/parenlog/"]{Parenlog}

Parenlog in an implementation of a language very similar to pure Prolog, except with parenthetical notation.


 ◊link["https://docs.racket-lang.org/pie/"]{Parenlog}

Pie is a little language with dependent types that accompanies The Little Typer.


 ◊link["https://docs.racket-lang.org/pollen/"]{Pollen}

The Pollen language is markup-based, so you can write & edit text naturally. But when you want to automate repetitive tasks, add cross-references, or pull in data from other sources, you can access a full programming language from within the text.


 ◊link["https://docs.racket-lang.org/profj/"]{ProfessorJ: Java in Racket}

ProfessorJ is a plug-in for DrRacket that implements variants of Java, especially for teaching purposes.


 ◊link["https://github.com/vishesh/racketscript"]{Racketscript}

A lightweight Racket to JavaScript compiler.


 ◊link["https://docs.racket-lang.org/rash/"]{Rash: The Reckless Racket Shell}

Rash is a shell language embedded in Racket. It has a concrete syntax that is amenable to quick and easy interactions without lots of punctuation overhead. It aims to allow shell-style interaction and programming to be freely mixed with more general-purpose Racket code.


 ◊link["https://docs.racket-lang.org/riposte/"]{Riposte}

Riposte is a scripting language for evaluating JSON-bearing HTTP responses.


 ◊link["https://docs.racket-lang.org/scratchy/"]{Scratchy: A Scratch-like Toy}

Scratchy provides a Scratch-like runtime environment plus a simple textual programming language. It was developed as an example of creating a language in Racket.


 ◊link["http://syndicate-lang.org/"]{Syndicate}

Syndicate is an Actor-based programming language for interactive programs.


 ◊link["https://lang.video"]{Video: A Language for Making Movies}

Video is a language for making movies. It combines the power of a traditional video editor with the capabilities of a full programming language. Video integrates with the Racket ecosystem and extensions for DrRacket to transform it into a non-linear video editor.


}

◊section{
◊div{Meta-DSL's}
}

◊section{
 


◊p[#:style "font-size: 80%;margin-top: 1rem;color:gray;width:80%;line-height:1.5"]{'Meta-DSL's' are languages that can be used to create languages.}


  ◊link["https://docs.racket-lang.org/turnstile/"]{Turnstile}

The Turnstile language aims to help Racket programmers create typed languages. It does so with extensions of Racket’s macro-definition forms that facilitate implementation of type rules alongside normal macro code.


  ◊link["https://emina.github.io/rosette/index.html"]{Rosette}

"Rosette is a solver-aided programming language that extends Racket with language constructs for program synthesis, verification, and more. To verify or synthesize code, Rosette compiles it to logical constraints solved with off-the-shelf SMT solvers. By combining virtualized access to solvers with Racket’s metaprogramming, Rosette makes it easy to develop synthesis and verification tools for new languages. You simply write an interpreter for your language in Rosette, and you get the tools for free!".  See ◊link["https://homes.cs.washington.edu/~emina/pubs/rosette.onward13.pdf"]{Growing Solver-Aided Languages with Rosette} by Emina Torlak & Rastislav Bodik.
}

