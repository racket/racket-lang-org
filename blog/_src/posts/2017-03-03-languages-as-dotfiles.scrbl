#lang scribble/manual
Title: Languages as Dotfiles
Date: 2017-03-03T13:18:04
Tags:

@require[
  (prefix-in s: (only-in scribble/manual hash-lang))
  (for-label racket/base racket/format racket/list syntax/parse
             reprovide/reprovide)
]

@(define-syntax-rule (lang id)
   (racket-pygments @elem{@s:hash-lang[] @racket[id]}))

@(define (inset . content)
   (apply nested #:style 'inset content))

@(define-syntax-rule (rkt x)
   (racket-pygments (racket x)))

@(define-syntax-rule (hash-lang)
   (racket-pygments (s:hash-lang)))

@(define-syntax-rule (racket-pygments x)
   (elem #:style "RktWrap" x))

@(define-syntax-rule (tt x)
   (code x))

@; =============================================================================

@emph{posted by Leif Andersen and Ben Greenman}

Tired of writing @rkt[(require (for-syntax syntax/parse))] at the top of your
Racket programs?
This post shows how to make a @hash-lang[] to customize your default programming
environment.

<!-- more -->

Let's build a language @lang[scratch]:

@itemlist[
@item{
  that loads the @rkt[racket/base], @rkt[racket/format],
  @rkt[racket/list], and @rkt[syntax/parse] (at @seclink["phases" #:doc
  '(lib "scribblings/guide/guide.scrbl")]{phase 1}) libraries;
}
@item{
  and enables Scribble's @seclink["reader"
  #:doc '(lib "scribblings/scribble/scribble.scrbl")]{@"@"-syntax}.
}
]

We'll follow a three-step recipe:

@inset[
 @inset[
  @itemlist[#:style 'ordered
  @item{
    build an empty @rkt[scratch] library (@secref{setup})
  }
  @item{
    load the libraries (@secref{require})
  }
  @item{
    change the reader (@secref{reader})
  }
]]]

and at the end, show how to make @lang[scratch] your default language in
DrRacket (@secref{drracket}).


@section[#:tag "setup"]{Getting Started}

First we need to make a @tt{scratch/} directory with two files: an @tt{info.rkt}
file and a @tt{main.rkt} file.

@inset[@verbatim{
  $ mkdir scratch; cd scratch

  $ touch info.rkt

  $ touch main.rkt
}]

Inside the @tt{info.rkt} file, write:

@inset[@codeblock|{
    #lang info
    (define name "scratch")
    (define deps '("base"))
    (define version "0.0")
}|]

Inside the @tt{main.rkt} file, write:

@inset[@codeblock|{
    #lang racket/base
}|]

Now from inside the @tt{scratch/} directory, install the package:

@inset[
  @exec{$ raco pkg install}
]

You are now the proud parent of a new Racket package.

@margin-note{
  @secref["raco-pkg-new" #:doc '(lib "pkg/scribblings/pkg.scrbl")] is a
  shortcut for starting a new package.
}
@margin-note{
  For more information on the @tt{info.rkt} file format, see
  @secref["metadata" #:doc '(lib "pkg/scribblings/pkg.scrbl")] and
  @secref["setup-info" #:doc '(lib "scribblings/raco/raco.scrbl")].}


@section[#:tag "require"]{Combining Libraries}

Any program can now @rkt[(require scratch)] to import all bindings provided
by the @tt{main.rkt} file.
Our next step is to reprovide bindings from other libraries in @tt{main.rkt}.

Since we want to use @tt{scratch} as a language, we also need to specify how to
@seclink["reader" #:doc '(lib "scribblings/reference/reference.scrbl")]{read}
a @lang[scratch] program.
The @rkt[syntax/module-reader] language provides a shorthand for doing so.

Updated @tt{main.rkt}:

@inset[
  @codeblock|{
    #lang racket/base

    (require racket/format racket/list
             (for-syntax racket/base syntax/parse))

    (provide (all-from-out racket/format racket/list racket/base)
             (for-syntax (all-from-out racket/base syntax/parse)))

    (module* reader syntax/module-reader
      scratch)
}|]


The @rkt[provide] form declares the exports of the @rkt[scratch] module.
In other words, if another module contains the form @rkt[(require scratch)]
then that module will import bindings from @rkt[racket/base],
@rkt[racket/format], @rkt[racket/list], and @rkt[syntax/parse].

The @rkt[reader] submodule is written in the @rkt[syntax/module-reader]
language.
This submodule imports all bindings from its enclosing module
(@rkt[scratch], or to be slightly more precise ``the toplevel module in the
file @tt{scratch/main.rkt}'') and defines a language that provides those
bindings and uses the reader from @rkt[racket/base].

In short, this code does what we want.

@inset[
  @codeblock|{
    #lang scratch

    (define-syntax (did-it-work? stx)
      (syntax-parse stx
       [_ #'(first '(yes it did))]))

    (did-it-work?)
}|]

Yes it does.

@margin-note{
  Annoyed that the @rkt[require] and @rkt[provide] forms are so similar?
  There's a library for that: @other-doc['(lib
  "reprovide/scribblings/reprovide.scrbl")].
}


@section[#:tag "reader"]{Changing the Reader}

Next, we want to enable the @seclink["reader"
#:doc '(lib "scribblings/scribble/scribble.scrbl")]{@"@"-expression} reader.

Updated @tt{main.rkt}:

@inset[
  @codeblock|{
    #lang racket/base

    (require racket/list
             (for-syntax racket/base syntax/parse))

    (provide (all-from-out racket/list racket/base)
             (for-syntax (all-from-out racket/base syntax/parse)))

    (module* reader syntax/module-reader
      scratch
      #:read s:read
      #:read-syntax s:read-syntax
      (require (prefix-in s: scribble/reader)))
}|]

To test that it works, let's embed some C syntax in our Racket program:

@inset[
  @codeblock|{
    #lang scratch

    (define-syntax (did-it-work? stx)
      (syntax-parse stx
       [_ #'(first '(yes it did))]))

    (did-it-work?)

    @~a{
      int main() {
        return 0;
      }
    }
}|]

@margin-note{
  At this point, running @exec{$ raco setup --check-pkg-deps scratch} will
  report an undeclared dependency on @racket{at-exp-lib}.
  Make sure to add @racket{at-exp-lib} to the @tt{deps} list in your
  @tt{info.rkt} file.}
@margin-note{
  Using @rkt[prefix-in] is not necessary; it just clarifies where
   @rkt[read] and @rkt[read-syntax] come from.}
@margin-note{
  If you think inline C @emph{strings} are interesting, you should definitely
  watch Jay McCarthy's RacketCon 2016
  @hyperlink["https://www.youtube.com/watch?v=r1kE3R26dK0"]{talk} on
  @hyperlink["https://github.com/jeapostrophe/remix"]{remix}.
}


@section[#:tag "drracket"]{DrRacket's Automatic @hash-lang[] Line}

To make @lang[scratch] the default language for new files in DrRacket:

@inset[
  @itemlist[#:style 'ordered
  @item{
    Click ``Language'' in the menu bar.
  }
  @item{
    Click ``Choose Language'' in the drop-down menu.
  }
  @item{
    Click the radio button for ``The Racket Language'', then click the ``Show Details'' button at the bottom of the window.
  }
  @item{
    @; TODO 
    Type @tt{Hlang scratch} in the text box labeled ``Automatic @tt{H}lang line''.
  }
]]

Click ``Ok'', and that's the end. Enjoy.


@section[#:tag "end"]{The End}

You can and should engineer the @hash-lang[] line of your Racket programs to
remove unnecessary boilerplate and/or enforce a project-specific development
environment.

Notes:
@inset[
  @itemize[
  @item{
    Feel free to pubish your custom language on the Racket
    @hyperlink["https://pkgn.racket-lang.org"]{package server}.

    Make sure to run @exec{$ raco setup --check-pkg-deps scratch} beforehand!
  }
  @item{
    Our personal ``dotfiles'' are
    @hyperlink["https://github.com/LeifAndersen/racket-scratch"]{racket-scratch}
    and @hyperlink["https://github.com/bennn/agile"]{agile}.
  }
  @item{
    The title ``Languages as Dotfiles'' is a reference to
    @hyperlink["/2011/03/languages-as-libraries-pldi-2011.html"]{@emph{Languages as Libraries}}
  }
]]

