#lang scribble/manual

Title: Racket-on-Chez Status: January 2018
Date: 2018-01-05T00:00:00
Tags: Chez Scheme

@(require (only-in scribble/core
                   style
                   color-property
                   background-color-property
                   target-element
                   link-element
                   plain)
          racket/list
          racket/runtime-path
          pict
          (only-in scribble/html-properties
                   alt-tag)
          "racket-on-chez-status/stack.rkt"
          (for-label racket/base
                     ffi/unsafe
                     racket/unsafe/ops))

@(define extended-mode? #f)
@(define blog-mode? #t)

@(define (extended . pre-flow)
   (if extended-mode? pre-flow null))
@(define (non-extended . pre-flow)
   (if extended-mode? null pre-flow))

@(define-runtime-path racket-on-chez-status "racket-on-chez-status")

@(cond
  [blog-mode?
   @italic{posted by Matthew Flatt with thanks to collaborators
           Kent Dybvig, Andy Keep, Gustavo Massaccesi, Sarah Spall, Sam Tobin-Hochstadt, and Jon Zeppieri}]
  [else
   (list
    @title[#:version ""]{Racket-on-Chez Status: January 2018@extended{@smaller{ [extended version]}}}
    @author[@elem{Matthew Flatt@elem[#:style 'newline]
            @smaller{with thanks to collaborators: Kent Dybvig, Andy Keep, Gustavo Massaccesi, Sarah Spall,
                     @elem[#:style 'newline]@hspace[2]Sam Tobin-Hochstadt, and Jon Zeppieri}}])])

@(define bg-color "beige")
@(define label-bg-color "lightgray")

@(define-values (racket-v6-all racket-v6-all-stretched
                 racket-v6-core
                 racket-v7
                 chez-scheme
                 basic-chez-scheme
                 racket-on-chez
                 detailed-racket-on-chez)
   (make-stacks #:block-sep 1
                #:stack-h 300
                #:block-w 100
                #:all-block-w 40
                #:r (lambda (p)
                      (let ([p (inset (panorama p) 10)])
                        (cc-superimpose (colorize (filled-rectangle (pict-width p) (pict-height p))
                                                  bg-color)
                                        p)))))

@(define explain-style
   (if blog-mode?
       (style #f (list (alt-tag "p")))
       #f))

@(define (explain p . pre-flow)
   (tabular #:sep @hspace[1]
            (list (list (nested #:style explain-style pre-flow) p))))

@(define (color c . content) @elem[#:style (style #f (list (color-property c))) @bold[content]])

@(define (label . content) @elem[content])
@(define (hd s) @bold[s])

@(define (not-yet . content) @elem[#:style (style #f (list (color-property "darkred"))) content])

@(define (subsec . lbl) @larger{@bold[lbl]})

@(define (log-image path) @image[#:scale 0.5 path])

@(define (chez-change #:merged? [merged? #f] #:partly? [partly? #f] . content)
   @elem{@elem[#:style (style #f (list (color-property "darkgreen"))) content]
         @(if merged? @smaller{ [@(if partly? "partly " "")merged]} "")})

@(define (todo . content) @elem[#:style (style #f (list (color-property "darkblue"))) content])

@(define label-bg (background-color-property label-bg-color))
@(define cell-bg (background-color-property bg-color))

@(define (dupl v l) (for/list ([i (in-list l)]) v))

@(define (bm-tabular l
                     #:sep [sep @hspace[2]]
                     #:wide? [wide? #f])
   (define (wrap t)
     (if (or blog-mode? wide?)
         t
         @nested[#:style 'inset t]))
   (wrap
    (tabular
     #:cell-properties (cons
                        (append (list label-bg) (dupl (list 'center label-bg) (car l)) (list label-bg))
                        (map
                         (lambda (r) (append (list cell-bg) r (list cell-bg)))
                         (list*
                          (dupl (list 'left cell-bg) (cadr l))
                          (for/list ([r (in-list (cddr l))])
                            (cons (list 'left cell-bg) (dupl (list 'right cell-bg) (cdr r)))))))
     #:sep sep
     (map (lambda (r) (append (list 'nbsp) r (list 'nbsp)))
          l))))

@(define (bar fraction #:width width)
   (hc-append (colorize (filled-rectangle (* fraction width) 10) "forestgreen")
              (blank (* (- 1 fraction) width) 10)))

@(define (bars l #:width [width 50])
   (define maxes (let loop ([l (map cdr (cddr l))])
                   (cond
                     [(null? (car l)) null]
                     [else (cons (apply max (map (lambda (v) (or (string->number v) 0))
                                                 (map car l)))
                                 (loop (map cdr l)))])))
   (list*
    (add-between (car l) 'cont)
    (cons (caadr l) (append (add-between (cdadr l) 'cont) (list 'cont)))
    (for/list ([r (in-list (cddr l))])
      (cons (car r)
            (apply
             append
             (for/list ([c (in-list (cdr r))]
                        [m (in-list maxes)])
               (list c
                     (bar (/ (or (string->number c) 0) m) #:width width))))))))

@; --------------------------------------------------------------------------------

@extended{@margin-note{Unless youre especially interested in implementation details, see
          the @hyperlink["http://blog.racket-lang.org/2018/01/racket-on-chez-status.html"]{shorter
          version of the report on the Racket blog}. Additions here are marked with ``[extended].''}}

It's been almost a year since we
@hyperlink["https://groups.google.com/d/msg/racket-dev/2BV3ElyfF8Y/4RSd3XbECAAJ"]{decided
to try} running @hyperlink["https://racket-lang.org/"]{Racket} on
@hyperlink["https://www.scheme.com/"]{Chez Scheme}, and it has been
almost six months since my last
@hyperlink["https://groups.google.com/d/msg/racket-dev/rkXuHNAmQaA/hjgPZHErAwAJ"]{status
report}. As of @hyperlink["https://con.racket-lang.org/"]{RacketCon
2018}, DrRacket could start up on Chez Scheme (but just barely, and
just in time for the demo).

The @hyperlink["https://github.com/racket/racket7/"]{current
implementation} is now @seclink["functionality"]{considerably more
complete}. DrRacket still doesn't run well, but Racket-on-Chez can
build a full Racket distribution in @seclink["build"]{reasonable time
and space}.

With the current Racket-on-Chez compilation strategy, runtime
performance is @seclink["benchmarks"]{plausible on traditional
benchmarks}, but cross-module optimization is needed to bring the
results fully in line with our expectations.
@seclink["startup"]{Startup and load times} are much slower than we
would like. @seclink["build"]{Compile/build times} are a factor of 4
slower than the current version of Racket.

@margin-note{Glossary of implementations:
             @itemlist[
              @item{@bold{Racket} --- the current Racket release, version 6.x}
              @item{@bold{@tt{racket7}} --- mostly the same as Racket, but using a new,
                                            Racket-implemented macro expander;
                                            the source is in the
                                            @url{https://github.com/racket/racket7} repo}
              @item{@bold{Chez Scheme} --- a completely different implementation of
                                           Scheme that we're trying to use as a replacement
                                           for part of Racket}
              @item{@bold{Racket-on-Chez} --- Chez Scheme plus additional layers to make
                                              it implement the same language as Racket;
                                              the source is also in the
                                              @url{https://github.com/racket/racket7} repo}
            ]}

@; --------------------------------------------------------------------------------

@section[#:style 'unnumbered]{Approach}

Our overall goal is to improve Racket's implementation and
make it more portable to different runtime systems. To a first
approximation, improving Racket and making it more portable means
``less C code.'' Building on a Chez Scheme is a promising means toward
that end.

@explain[racket-v6-all]{

The picture on the right illustrates the current Racket distribution.
The parts in blue are implemented in @color[racket-color]{Racket},
while the part in red is implemented in @color[c-color]{C}. The green
block is @color[scrbl-color]{Scribble} documentation source.

@hspace[1]

The numbers next to each block are a rough lines-of-code counts, and
the blocks are scaled to those counts. The
@color[racket-color]{collects} block represents the content of the
main @filepath{collects} directory; along with the
@color[c-color]{core} block, it's roughly the code in a Minimal Racket
distribution. The @color[racket-color]{pkgs} block is the part of the
main distribution that is available in separate packages but bundled
with the main distribution. The @color[scrbl-color]{docs} block is
spread across the same packages as the @color[racket-color]{pkgs}
code.

@hspace[1]

Porting Racket to Chez Scheme is all about the @color[c-color]{core}
part. The goal is to make no changes to existing
@color[racket-color]{Racket} and @color[scrbl-color]{Scribble} code,
although we'll inevitably have to make small changes to support
multiple Racket implementations.

}

@explain[racket-v6-core]{

Let's zoom into the @color[c-color]{core} part. (From now on, all the
pictures on the right are at this scale.)
@;
The picture breaks the C code in Racket's current
implementation into several major subsystems. In reality, the
implementation is not organized into such tidy layers, but
the picture is conceptually close to the truth.

@hspace[1]

The large @color[c-color]{builtins} block implements primitive data
structures and their operations, such as numbers (fixnums, bignums,
flonums, exact rationals, complex numbers), strings, characters, byte
strings, lists, vectors, and hash tables. Most other blocks are
self-explanatory.

@hspace[1]

The @color[c-color]{expander} block implements the macro expander,
reader, and module system. More than any other part of the current
stack, the @color[c-color]{expander} part is better implemented in
Racket.

}

@explain[racket-v7]{

That was exactly the work of 2016: building a new
@color[racket-color]{expander} to replace the C-implemented
@color[c-color]{expander}. You can use that @tt{racket7} variant of
Racket now by building from the @url{http://github/racket/racket7}
repo.

@hspace[1]

This new @color[racket-color]{expander} hasn't made it into the
release, because a few interactions haven't been sorted out: the macro
stepper, the demodularizer, and to some degree the decompiler. The
expander also runs at about half the speed of the C-implemented
expander, and that performance difference was part of the motivation
to investigate a new runtime system.

@hspace[1]

Although the @color[c-color]{compiler+JIT} block is drawn the same as
before in this picture, it's a slightly different compiler, because it
doesn't know about modules or syntax objects. Instead, the compiler
works in terms of @defterm{linklets}, which are @tt{lambda}-like
blocks of code that import and export variables. That separation of
expansion/modules from compilation helps enable the transition to a
different compiler and runtime system.

}

@explain[chez-scheme]{

And that brings us to Chez Scheme. The most obvious feature in the
picture on the right is that Chez Scheme's C-implemented
@color[c-color]{kernel} is a fraction of the implementation, while most
of Chez Scheme is more sensibly written in
@color[scheme-color]{Scheme}.

@hspace[1]

Chez Scheme's @color[scheme-color]{expander} is considerably simpler
than Racket's. Although Chez Scheme's expander is hygienic and hugely
influential on Racket's expander, Racket's macro and module system
does a lot more.

@hspace[1]

Chez Scheme's @color[scheme-color]{compiler}, meanwhile, does a lot
more than Racket's @color[c-color]{compiler+JIT}. The
@color[scheme-color]{builtins} functionality in Chez Scheme is similar
to Racket's @color[c-color]{builtins}.

@hspace[1]

Chez Scheme's @color[c-color]{GC} (at the bottom) is tiny and elegant.
It lacks some functionality of Racket's @color[c-color]{GC}, but
mostly it lacks Racket's historical complexity for trying to cooperate
directly with C.

}

@explain[detailed-racket-on-chez]{

That brings us, finally, to the Racket-on-Chez picture. It starts with
the Chez Scheme stack and adds a @color[scheme-color]{rumble} block,
which extends Chez Scheme to match the current Racket's
@color[c-color]{GC} through @color[c-color]{control+structs} blocks.

@hspace[1]

Mostly, @color[scheme-color]{rumble} is about immutable hash tables,
delimited continuations, applicable structures, structure types
properties, and impersonators/chaperones. We've given this layer of
functionality the name @bold{Rumble}, because it's useful to have a
name for the language that hosts other layers that are built using
@color[racket-color]{Racket}.

@hspace[1]

The @color[racket-color]{threads}, @color[racket-color]{io}, and
@color[racket-color]{regexp} blocks are entirely new implementations
of the corresponding blocks in the current Racket implementation. The
@color[c-color]{rktio} block is used by @color[racket-color]{io}, and
it's the same as in current Racket: a portability layer over OS
facilities that is similar to (but more extensive than) code that is
in Chez Scheme's @color[c-color]{kernel}.

}

The @color[racket-color]{expander} block (the same as for
@tt{racket7}) sits on a small @color[racket-color]{schemify} adaptor.
The @color[racket-color]{schemify} layer converts Racket linklets into
plain Scheme functions, adjusting expressions as needed to support
applicable structures, to enforce left-to-right evaluation, and to
implement Racket's allocation model for local functions (i.e., lift to
avoid closure allocation whenever possible).

Naturally, the Racket-on-Chez implementation is bootstrapped by
applying the @color[racket-color]{expander} and
@color[racket-color]{schemify} layers to themselves and the other
Racket-implemented parts of the stack, and then compiling the
schemified linklets using the Chez Scheme
@color[scheme-color]{compiler}. A thin @color[scheme-color]{main}
driver on top handles command-line arguments to load modules and start
the @racket[read]--@racket[eval]--@racket[print] loop.

@; --------------------------------------------------------------------------------

@section[#:style 'unnumbered #:tag "functionality"]{Current Functionality}

DrRacket starts up in Chez Scheme, and just starting DrRacket
exercises many Racket language constructs and libraries. The largest
Racket-on-Chez demonstration to date is building all of the packages
and documentation of the main Racket distribution. Building packages
and documentation, again, covers more Racket functionality than you
may expect, because our macros do interesting things, such as
typechecking and generating bitmap images. Also, the build process is
too long and complex to tolerate leaks or major inefficiencies.

Still, many pieces are buggy, and closing the gap is mostly a matter
of plowing through the Racket test suites. @non-extended{The main
missing or incomplete pieces are: futures, places, and a @tt{racket}
executable that starts Racket-on-Chez.}

@extended{

A few large pieces are still missing:@margin-note*{[extended]}

@itemlist[

  @item{@not-yet{There's no @tt{racket} program that starts Racket-on-Chez,
        there's no @tt{DrRacket} executable to start DrRacket on Chez
        Scheme, and so on.}

        For now, Racket-on-Chez runs via @tt{make run ARGS="..."} in
        the Racket-on-Chez build directory. In the near future, there
        will be a way to generate @tt{racketcs}, @tt{DrRacketCS},
        etc., alongside @tt{racket}. In the more distant future, if
        things work out, @tt{racketcs} will become @tt{racket} by
        default.}

 @item{@not-yet{Futures are not fully supported.}

       Sarah continues to work on futures.}

 @item{@not-yet{Places are not supported.}

       Adding places still seems straightforward, but I haven't tried.}

 @item{@not-yet{Windows is not yet supported.}

       ... almost certainly, although I haven't tried it.}

 @item{@not-yet{Unicode support at the level of character
       classifications and string conversions needs to be sorted out.}

       I don't know how well Racket and Chez Scheme line up already,
       so this may be easy or difficult.}

 @item{@not-yet{Primitive error messages are not good.}

       There's a basic bridge in place to convert Chez scheme errors
       into Racket errors, but more work is needed to make error
       messages consistent and detailed.}

 @item{@not-yet{Custodian-based memory accounting is not yet supported.}

       Support for memory accounting looks tractable in the long run.}

 @item{@not-yet{Incremental garbage collection is not supported.}

       Incremental GC remains a low priority. Chez Scheme's garbage
       collector may or may not work well enough as-is for the
       programs that motivated incremental GC in Racket.}

 @item{@not-yet{Single-precision and extended-precision floating-point
       numbers are not supported.}

       These remain a low priority, and supporting them appears to
       require significant changes to Chez Scheme.}

]

Besides unsupported functionality, there are currently a few
incompatibilities with Racket. Most incompatibilities are in the
foreign-function interface. The following is an incomplete list, but
it illustrates the kinds of differences that remain.

@itemlist[

 @item{@not-yet{The @racket[eq?] operation is not defined on flonums,
       and @racket[(eq? x x)] does not hold in general.}

       I'm not sure, but we may just have to compromise compatibility
       and definedness on this point. It's worth noting that an
       @racket[eq?]-based hash table does not work for associating
       values to floating-point numbers, and I've changed one or two
       libraries to use @racket[eqv?]-based hash tables.}

 @item{@not-yet{The @racket[letrec] form does not always behave as
       Racket's model specifies when the right-hand side returns
       multiple times (through a captured continuation).}

       I haven't thought enough about this one, but I worry that it
       will be difficult to change.}

 @item{@not-yet{A foreign pointer cannot be turned into a Racket byte
       string using @racket[make-sized-byte-string].}

       Chez Scheme's representation of byte strings doesn't point to a
       separately allocated byte array in the way that Racket's
       representation does. This incompatibility has affected several
       libraries, but so far, I was able to adapt them easily to use
       an approach that works in both Racket implementations.}

 @item{@not-yet{The @racket['atomic-interior] allocation mode for
        @racket[malloc] returns memory that is allowed to move after
        the cpointer returned by allocation becomes unreachable.}

       Ditto.}

 @item{@not-yet{Memory allocated with @racket[malloc]'s
        @racket['nonatomic] mode works only in limited ways.}

       Memory allocated as @racket['nonatomic] is not actually an
       array of pointers, so it cannot be usefully passed to foreign
       functions. I have not yet encountered libraries where this is a
       problem.}

]

}

@;----------------------------------------

@section[#:style 'unnumbered #:tag "benchmarks"]{Performance: Benchmark Sanity Check}

The overall compilation story is not yet right in Racket-on-Chez,
partly because there's no cross-module optimization. Functions like
@racket[map], @racket[memq], and @racket[assv] need cross-module
optimization to be compiled well in Racket-on-Chez. Benchmarks
sometimes do not rely on cross-module optimization, however, and
preliminary benchmark measurements can be useful as a sanity
check on the ways that @color[racket-color]{schemify} mangles
individual modules.

@margin-note{Benchmarks are in the @tt{racket-benchmark} package
             whose source is in the @tt{racket7} repo.
             The benchmarks run in Chez Scheme as R6RS libraries.
             The difference between Racket and @tt{racket7} is in the noise.}

The table below shows a handful of rough benchmark results. You
should not take the results seriously, but they line up with my
overall impression of where we are.

@bm-tabular[#:wide? #t
(let ([bm (lambda l
             (map (lambda (s) (if (symbol? s) (bold (format "~a" s)) (format "~a" s))) l))]
      [rotate (lambda (l)
                (let loop ([l l])
                  (cond
                   [(null? (car l)) '()]
                   [else (cons (map car l) (loop (map cdr l)))])))])
 (bars
  #:width 20
  (cons
   (list @label{Rough and unscientific benchmark numbers} 'cont 'cont 'cont 'cont 'cont)
   (rotate
    (list
     (list @smaller{msecs} @elem{Chez Scheme} @elem{Racket-on-Chez} @elem{Racket})
     (bm 'deriv     770   1560  2030)
     (bm 'dynamic   360   1030   560)
     (bm 'sboyer    1270  1470  2230)
     (bm 'maze      930   1520  1330)
     #;(bm 'mazefun   2950  3400  5650)
     (bm 'nfa       2100  2200  4100)
     #;(bm 'nucleic2  5200  7500  5000))))))
]

All benchmarks were run in safe mode and @emph{without} Gustavo's
work-in-progress addition to the Chez Scheme compiler that can
eliminate some runtime checks and related dead code.

@;----------------------------------------

@section[#:style 'unnumbered #:tag "startup"]{Performance: Startup}

Startup time for Racket-on-Chez is determined by Chez Scheme's base
startup time (to load boot files) plus time to load the
@color[racket-color]{expander} and other parts of the Racket-on-Chez
stack. The load times shown below for @racketmodname[racket/base] and
@racketmodname[racket] include the startup time and add the additional
time needed to load the many compiled files that are parts of those
libraries.

For Racket and @tt{racket7}, bytecode parsing can be delayed at the
granularity of a procedure, so that a procedure's bytecode isn't
parsed until the procedure is called. Delayed parsing is enabled by
default, and the @Flag{d} flag disables it.

@bm-tabular[
(bars
 (list
  (list @label{Startup/load time} 'cont 'cont 'cont)
  (list @smaller{msecs}        @hd{startup} @hd{@tt{-l racket/base}}  @hd{@tt{-l racket}})
  (list "Racket"                "16"       "50"                   "220")
  (list @elem{Racket @Flag{d}}  "16"       "80"                   "500")
  (list (tt "racket7")          "50"      "110"                   "340")
  (list (tt "racket7 -d")       "50"      "190"                   "650")
  (list "Chez Scheme"           ""        "170"  "")
  (list "Racket-on-Chez"        "440"     "550"                   "1200")
  (list "Racket-on-Chez/JIT"    "440"     "550"                   "1600")
  ))
]


Compared to the current Racket version, starting up and loading takes
longer in @tt{racket7}, because @tt{racket7} has to load embedded
bytecode for the expander's implementation, and because the bytecode
encoding of linklets has not been refined as much.

The Chez Scheme row in the table doesn't actually load
@racketmodname[racket/base], but that's the closest reasonable column.
That row corresponds to just starting Chez Scheme with its
compiler---so it's a signficantly smaller language than
@racketmodname[racket/base], but it's more than the Racket
@hd{startup} rows. It's also a lower bound on the Racket-on-Chez
@hd{startup} time.

Loading Racket-on-Chez takes significantly longer, still, due to
larger and slower-to-load machine code for both the Racket-on-Chez
stack and libraries compiled with it. Although individual linklets
within a module (roughly, individual phases) are parsed on demand,
there is no delayed parsing at the granularity of procedures, so the
work performed by loading is closer to the Racket @Flag{d} and
@tt{racket7 @Flag{d}} rows.

The last row above, Racket-on-Chez/JIT, refers to a variant of
Racket-on-Chez that does not compile whole linklets. Instead, it keeps
individual @tt{lambda}s in source form until called, and it compiles
each called @tt{lambda} on demand. Load times go up, mostly because
code is compiled as needed, but also because the source-fragment
representation is even larger than machine code right now. The
Racket-on-Chez/JIT approach does not look the most promising, so far,
but it's also not crazy. With more effort and tuning (especially in
the representation of source fragments), it may be a viable approach
and more in line with the way the current Racket implementation works.

Clearly, load time is a direction for further effort.

@;----------------------------------------

@section[#:style 'unnumbered #:tag "expander"]{Performance: Expander and Compiler}

The @color[racket-color]{expander} runs at about the same speed in
Racket-on-Chez and @tt{racket7}, but the linklet-compilation step
takes much longer in Racket-on-Chez. As a result, compilation time for
Racket-on-Chez dominates expansion proper for a task like @tt{racket
@Flag{cl} racket/base}.

@margin-note{Reported times in this section use a Racket-on-Chez stack
that is compiled in unsafe mode and run on a 2013 MacBook Pro 2.6GHz
Core i5. Unsafe mode improves @hd{expand} and @hd{read} performance by about
10%.}

@bm-tabular[
 #:sep @hspace[1]
 (bars
  #:width 25
 (list
  (list @label{Steps within @tt{racket -cl racket/base}} 'cont 'cont 'cont 'cont 'cont)
  (list @smaller{msecs}          @hd{expand} @hd{schemify} @hd{compile} @hd{eval} @hd{read})
  (list (tt "racket7")            "2511"   ""    "662"     "235"  "275")
  (list "Racket-on-Chez"          "2398"   "917" "2852"    "390"  "227")
  (list "Racket-on-Chez/JIT"      "2441"   "974" "1044"    "515"  "229")))
]

For Racket-on-Chez/JIT, compile times go down, showing that about 1/3
of the code that is compiled during expansion is also run during
expansion. The fraction would be lower for most programs; the
@racketmodname[racket/base] library is unusual in building up layers
of macros to implement itself. The @hd{schemify} step is certainly a
target for improvement, since its job is much simpler than the
@hd{compile} step's job.

@extended{

@margin-note*{[extended]}Racket-on-Chez @hd{compile} times are longer
than @tt{racket7} @hd{compile} times because the Chez Scheme compiler
does more. About half of the Racket compiler's work corresponds to
Chez Scheme's @tt{cp0} pass, and Racket's compiler/JIT lacks a
general-purpose register allocator. Here's a table of cumulative times
spent in Chez Scheme compiler passes while loading
@racketmodname[racket/base] from source (summing roughly to the 2852
millisecond total in the previous table, although there's noise so
that the numbers won't match exactly):

@bm-tabular[
(let ([tt/ra (lambda (s) @elem{@tt[s] *})])
 (bars
 (list
   (list @label{Chez Scheme compiler passes during @tt{racket -cl racket/base}} 'nbsp)
   (list "" @smaller{msecs})
   (list @tt{cpletrec} "35")
   (list @tt{np-expand-primitives} "37")
   (list @tt{np-place-overflow-and-trap} "40")
   (list @tt{np-optimize-pred-in-value} "46")
   (list @tt{np-expose-allocation-pointer} "48")
   (list @tt{np-impose-calling-conventions} "51")
   (list @tt{expose-overflow-check-blocks!} "56")
   (list @tt{np-expand-hand-coded} "60")
   (list @tt/ra{do-live-analysis!} "87")
   (list @tt/ra{assign-new-frame!} "87")
   (list @tt{np-expose-basic-blocks} "101")
   (list @tt/ra{assign-registers!} "105")
   (list @tt/ra{do-unspillable-conflict!} "116")
   (list @tt/ra{finalize-frame-locations!} "116")
   (list @tt/ra{do-spillable-conflict!} "128")
   (list @tt/ra{finalize-register-locations!} "130")
   (list @tt{expand} "149")
   (list @tt{cp0} "200")
   (list @tt{select-instructions!} "304")
   (list @tt{np-generate-code} "351")
   (list "other short passes total" "504"))))]

The steps marked with * are primarily about register
allocation, and those total to 770 milliseconds.

}

That factor-of-2 slowdown relative to @tt{racket7} is
compounded by the Racket-implemented @color[racket-color]{expander}
being twice as slow as the current Racket's C-implemented
@color[c-color]{expander}:

@bm-tabular[
 (bars
 (list
  (list @label{Loading from source with @tt{racket -cl}} 'cont 'cont)
  (list @smaller{msecs}        @hd{@tt{racket/base}}   @hd{@tt{racket}})
  (list "Racket"               "1830"                "21700")
  (list (tt "racket7")         "3490"                "38140")
  (list "Racket-on-Chez"       "7060"                "70093")
  (list "Racket-on-Chez/JIT"   "5400"                "55050")))
]

The gap between the Racket-implemented @color[racket-color]{expander}
and the C-implemented @color[c-color]{expander} is the one that we
most hoped to close by moving to Chez Scheme. As part of the
Racket-on-Chez stack, the @color[racket-color]{expander} is already
compiled in a reasonable way (i.e., no cross-module optimization
issues), so it's not clear how to improve. Still, I still think the
@color[racket-color]{expander} can be made to run faster in Chez
Scheme, and we'll keep trying.

@; --------------------------------------------------------------------------------

@section[#:style 'unnumbered #:tag "build"]{Performance: Distribution Build}

The current version of Racket-on-Chez builds on a 64-bit machine with
a peak memory use around 1.25 GB, which is on a similar scale to the
current Racket release's peak memory use around 1 GB.

The following plots show, on the same scale, memory use over time when
building a distribution. Each plot has two black lines (easiest to
distinguish in the last plot): the top black line describes peak
memory use, since it's before a major GC, while the bottom black line
is closer to live-memory use, since it's after a major GC. The orange
region is for compiling libraries (each vertical line starts a
collection), the blue region is for running documentation, the green
region is for rendering documentation, and the final pink region is
for re-rendering documentation as needed to reach a fix-point for
cross references. Memory use is expected to grow modestly during the
blue region, since the builds collecting cross-reference information
for use in the green region. Memory use should grow minimally in the
orange and green regions (as a side-effect of caches).

@margin-note{These graphs report builds on a Linux 3.4GHz Core i7-2600
machine with the Racket parts of the Racket-on-Chez stack compiled in
safe mode.}

@nested[#:style 'inset]{
@tabular[
 #:sep @hspace[1]
 #:column-properties '(top right)
 (list
  (list @nested{@bold{Racket}

                C-implemented @color[c-color]{expander}}
        @log-image[(build-path racket-on-chez-status "build-log-racket.svg")])
  (list @smaller{} @smaller{})
  (list @nested{@bold{@tt{racket7}}

                Racket-implemented @color[racket-color]{expander}}
        @log-image[(build-path racket-on-chez-status "build-log-racket7.svg")])
  (list @smaller{} @smaller{})
  (list @nested{@bold{Racket-on-Chez}

                Same @color[racket-color]{expander} as @tt{racket7}}
        @log-image[(build-path racket-on-chez-status "build-log-cs.svg")]))]
}

The plots illustrate that, while memory use is similar, build times
are much longer. @seclink["expander"]{Expander and compiler
performance} largely explains the difference.

The build time certainly can be improved some. Maybe build times can
be improved significantly, or maybe slower build times will seem
worthwhile for a more maintainable implementation of Racket and/or
for faster generated code.

@; --------------------------------------------------------------------------------

@extended{

@section[#:style 'unnumbered]{Changes to Chez Scheme}

@margin-note*{[extended]}
@;
There are many good virtual machines in the world, but none starts
remotely as close to Racket as Chez Scheme---because they're both
Schemes, and because Racket historically has taken so many cues from
Chez Scheme (if not executed them as well). Still, we expected to make
modest changes to Chez Scheme to fully support Racket.

Currently, running Racket-on-Chez requires the fork at
@url{http://github.com/mflatt/ChezScheme}. With one exception that
will be remedied eventually, all of the changes in the fork have been
submitted as pull requests at
@url{http://github.com/cisco/ChezScheme}. Nine or so pull requests
have been merged, and eight or so are currently open. In addition,
Kent and Andy invested significant effort to improve the register
allocator to help support Racket.

For the record, here is a summary of the main changes:

@itemlist[

 @item{@chez-change[#:merged? #t]{Add immutable strings, byte strings,
       vectors, and boxes.}

       Adding wrappers to enforce immutability would be expensive, so
       built-in immutable variants help considerably.

       Racket's pairs differ from vectors, etc., in that mutable and
       immutable pairs are distinct types. Immutable pairs are Chez
       Scheme's pairs, made effectively immutable by just not exposing
       @tt{set-car!} and @tt{set-cdr!} at the Racket level.}

 @item{@chez-change[#:merged? #t]{Configurable equality and hashing
       for records.}

       Chez Scheme's built-in @tt{equal?} and @tt{equal-hash}
       functions now support configuration for record types. Although
       Racket-on-Chez ended up reimplementing @racket[equal?] and hashing to
       support impersonators and chaperones, the reimplementation uses
       the same hooks.}

 @item{@chez-change[#:merged? #t]{Record line and column information in
       source objects.}

       Chez Scheme's technique of on-demand line and column counting
       did not fit well with Racket's view of source locations.}

 @item{@chez-change[#:merged? #t]{Add
       @hyperlink["https://en.wikipedia.org/wiki/Ephemeron"]{ephemerons}.}

       Many parts of Racket rely on ephemerons to avoid key-in-value
       leaks.}

 @item{@chez-change{Ordered finalization via guardians.}

       Internally, the current Racket implementation has three levels
       of finalization, which allows system finalization to happen
       later than client-visible finalization. Ordered guardians
       achieve the same result in a more composable way.}

 @item{@chez-change{Support @tt{struct} and @tt{union} arguments
       and return values for foreign functions.}

       C @tt{struct} arguments and return values are a pain, but
       they're necessary for various Racket foreign-function bindings,
       including the drawing and GUI libraries.}

 @item{@chez-change{Add compiler optimizations based on type
       reconstruction.}

       Gustavo's work-in-progress addition to the Chez Scheme compiler
       introduces the kinds of optimizations that he has long built
       and maintained in the current Racket implementation. For
       example, @racket[(f (car x) (cdr x))] can be optimized to
       @racket[(f (car x) (unsafe-cdr x))]. These optimizations are
       particularly helpful for structure-field access and update, and
       preliminary measurements suggest that the optimizations can
       provide about half of the performance benefit of unsafe mode
       (so, typically 5-10%) without the unsafety.}

 @item{@chez-change{Add backreference reporting to the garbage collector.}

       This change is the one that doesn't have a pull request, yet.
       It extends the garbage collector to report, for each live
       object, a referencing object that kept it alive. This tool has
       been essential to tracking down memory leaks, but Chez Scheme's
       inspector provides features that might be extended instead of
       adjusting the garbage collector.

       The current Racket implementation provides this information
       only if you build in a special way and use a poorly documented
       interface, and it's somewhat unreliable. Chez Scheme's nicer
       garbage-collector implementation made the feature easy to add
       and to have always available.}

 @item{@chez-change[#:partly? #t #:merged? #t]{Make the register allocator more scalable.}

       Chez Scheme's compiler uses a graph-coloring register allocator
       that is by default quadratic in time and space. Changes by the
       Chez Scheme authors avoid quadratic behavior in most cases. A
       pending pull request covers additional cases that show up in
       practice for Racket.

       The more general repair for Racket-on-Chez has been to avoid
       sending overly large expressions to the Chez Scheme compiler.
       For example, the pending pull requests provides linear-time
       behavior for a real Racket module, but the constant factor is
       still too large. Large code fragments tend to correspond to top
       level of a Racket module, and a custom interpreter works well
       for those cases.}

]

}

@; --------------------------------------------------------------------------------

@section[#:style 'unnumbered #:tag "plan"]{Outlook@extended{ and Plans}}

It would be nice if porting to Chez Scheme made every aspect of Racket
magically faster. It hasn't done that, but we have plenty of room for
improvement; the performance results to date are a lower bound on
performance, not an upper bound.

Keep in mind that the original goal is not to have a faster Racket,
but a better-implemented Racket with acceptable performance. The
Racket-on-Chez implementation is far more maintainable and flexible
than the current Racket implementation, so maybe we're half-way there
after one year of work.

@extended{

Some immediate next steps are clear: @margin-note*{[extended]}

@itemlist[

 @item{@todo{Implement cross-module optimization.}

       Currently, I expect to implement cross-module optimization by
       adding inlining, constant folding, and constant propagation,
       and copy propagation to the @color[racket-color]{schemify}
       layer. Although that approach duplicates work that Chez
       Scheme's compiler can do already (if cross-module information
       were somehow communicated to the compiler), it's a relatively
       straightforward source-to-source transformation that fits
       naturally into work that the @color[racket-color]{schemify}
       layer performs already (I think). Also, putting high-level
       optimizations at the @color[racket-color]{schemify} layer will
       make them available to future Racket ports.}

 @item{@todo{Generate @tt{racket}, etc., executables that run
             Racket-on-Chez.}

       This task is mostly a matter of creating build scripts. In the
       end, building should be as easy as @tt{make cs} in the top
       level of the Racket repo.}

 @item{@todo{Implement places.}

       Obviously.}

 @item{@todo{Make all of the Racket test suites pass} (except for
             single-precision floating point, extended-precision
             floating point, and memory accounting).

      It might seem that passing tests would be step 0 instead of step
      4, but test suites turn out to be inconvenient to use on a slow
      and broken implementation, so I've interleaved test-suite work
      with everything else.}

]

That's a couple of months' more work, at least.

Suppose that the result has good runtime performance compared to the
current Racket implementation, but it's still slower to compile and
load by about a factor of 4. (I think that's a fairly likely result,
given the current trajectory.) Then what? I'm not sure, but I'll keep
trying and give it the rest of the second year.

}

You can try Racket-on-Chez from
@url{https://github.com/racket/racket7/}. See
@hyperlink["https://github.com/racket/racket7/blob/master/racket/src/cs/README.txt"]{racket/src/cs/README.txt}
in that repo for more information.

@non-extended{@margin-note{If you're interested in even more implementation details and plans, see
             the @hyperlink["http://www.cs.utah.edu/~mflatt/racket-on-chez-jan-2018/"]{extended version of this report}.}}
