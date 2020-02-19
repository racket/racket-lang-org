#lang scribble/manual

Title: Racket-on-Chez Status: February 2020
Date: 2020-02-19T00:00:00
Tags: Chez Scheme

@(require racket/runtime-path
          (only-in scribble/html-properties
                   render-convertible-as)
          (submod "racket-on-chez-status-feb2020/bm-jan20.rkt" pict)
          "racket-on-chez-status-feb2020/alog.rkt"
          "racket-on-chez-status-feb2020/color.rkt"
          (for-label ffi/unsafe
                     racket/base
                     racket/generator)
          (only-in scribble/core
                   style
                   color-property))

@(define blog-mode? #t)

@(define (section* . l) @section[#:style (style #f (list 'unnumbered
                                                         (render-convertible-as '(svg-bytes))))
                                 l])

@(define jan18-url "http://blog.racket-lang.org/2018/01/racket-on-chez-status.html")
@(define jan19-url "http://blog.racket-lang.org/2019/01/racket-on-chez-status.html")

@(define-runtime-path build-rcs.svg "racket-on-chez-status-feb2020/bmtimes-jan20/build-rcs.svg")
@(define-runtime-path build-r.svg "racket-on-chez-status-feb2020/bmtimes-jan20/build-r.svg")

@(define malloc-url
   (string-append "https://www.cs.utah.edu/plt/snapshots/current/doc/foreign/foreign_pointer-funcs.html"
                  "#%28def._%28%28quote._~23~25foreign%29._malloc%29%29"))

@(define fun-url
   (string-append "https://www.cs.utah.edu/plt/snapshots/current/doc/foreign/foreign_procedures.html"
                  "#%28def._%28%28lib._ffi%2Funsafe..rkt%29.__cprocedure%29%29"))

@(define inside-mz "http://docs.racket-lang.org/inside/index.html")

@(cond
  [blog-mode?
   @italic{posted by Matthew Flatt}]
  [else
   (list
    @title[#:version ""]{Racket-on-Chez Status: February 2020}
    @author[@elem{Matthew Flatt}])])

@(define (bmnotes . l) (smaller (italic (bold "About the measurements:") " " l)))
@(define (bmmorenotes . l) (smaller (italic l)))

@(define (color c . content) @elem[#:style (style #f (list (color-property c))) @bold[content]])

@(define RacketCS @color[racket-color]{Racket CS})
@(define RacketBC @color[c-color]{Racket BC})

@margin-note{For background information about Racket on Chez Scheme
(a.k.a. Racket CS), see
@hyperlink["https://groups.google.com/d/msg/racket-dev/2BV3ElyfF8Y/4RSd3XbECAAJ"]{the
original announcement}, @hyperlink[jan18-url]{the January 2018 report},
@hyperlink["https://www.youtube.com/watch?v=t09AJUK6IiM"]{the
report at Scheme Workshop 2018},
@hyperlink[jan19-url]{the January 2019 report},
@hyperlink["https://www.cs.utah.edu/plt/rkt-on-chez/"]{the ICFP experience report},
and @hyperlink["https://www.youtube.com/watch?v=dnz6y5U0tFs"]{the
report at RacketCon 2019}.}

Racket on Chez Scheme (@RacketCS) is ready for production use.
@RacketCS now passes all of the tests for the main Racket distribution
tests, and differences in compile and run times are much reduced.
Overall, @RacketCS tends to perform about the same as the traditional
Racket implementation (@RacketBC, ``before Chez'')---sometimes better
and sometimes worse, but typically using more memory due to larger
code sizes.

@RacketCS is not yet the default Racket implementation, but it is
available as a download option alongside the regular Racket release at
@url{https://download.racket-lang.org/} (select @onscreen{CS} from the
@onscreen{Variant} popup).

@; --------------------------------------------------------------------------------
@section*{Run-Time Performance}

Run-time performance for @RacketCS has continued to improve.
Benchmarks show the difference to some degree, but they understate the
difference for a typical Racket application. Alex Harsanyi shared his
@hyperlink["https://groups.google.com/d/msg/racket-users/yfQvM4Rf7mA/gcJJI-XbAAAJ"]{initial
experience} with @RacketCS in December 2018, and he has been kind enough to keep
taking measurements. The plots below show his results for
@RacketCS and @RacketBC, where
lower is better, and the overall trend here seems typical for
applications that I've measured.

@centerline[(alog-plots)]

As the trend lines may suggest, the overall improvement is from many
small changes that add up. The plateau around June to October 2019
coincides with a push on correctness and compatibility, as opposed to
performance, to make all Racket tests pass.

The plots below show current results for traditional Scheme
benchmarks. The @color[scheme-color]{top bar} is current and
unmodified @color[scheme-color]{Chez Scheme}, while the
@color[scheme-color]{second bar} is @color[scheme-color]{Chez Scheme}
as modified to support Racket. The @color[racket-color]{third bar} is
@RacketCS, and the @color[c-color]{bottom bar} is @|RacketBC|. The
@color["forestgreen"]{last two rows} of benchmarks rely on mutable
pairs, so they are run in Racket as @racket[@#,hash-lang[] r5rs]
programs. There's not a lot of difference here compared to one year
ago, except that a faster path for integer division in the Racket
variant of @color[scheme-color]{Chez Scheme} has eliminated the
@tt{collatz} outlier.

@centerline{@traditional-benchmarks-table*[]}

The next set of plots compare @RacketCS and @RacketBC for
Racket-specific implementations over years for
@hyperlink["https://benchmarksgame-team.pages.debian.net/benchmarksgame/"]{The
Computer Language Benchmarks Game}. Compared to one year ago, the
fraction of benchmarks where @RacketCS wins over @RacketBC is
reversed. Much of that improvement happened in the thread and I/O
layers that were newly implemented for @|RacketCS|.

@centerline{@shootout-benchmarks-table*[]}

@bmnotes{Alex Harsanyi's measurements are for parts of the
ActivityLog2 test suite; ActivityLog2 and the CI infrastructure it
runs on have both changed over time, so the plots are approximate, but
they are generally consistent with fresh runs with the current
ActivitlyLog2 implementation on Racket versions 7.3 through 7.6.0.12.
All other measurements use a Core i7-2600 at 3.4GHz running 64-bit
Linux. Benchmarks are in the
@hyperlink["https://github.com/racket/racket/tree/master/pkgs/racket-benchmarks/tests/racket/benchmarks"]{@filepath{racket-benchmark}}
package in the @filepath{common} and @filepath{shootout} directories.
We used commit @tt{a20e3f305c} of the Racket variant of Chez Scheme
and commit @tt{cdd0659438} of Racket.}

@; --------------------------------------------------------------------------------
@section*{Startup and Load Times}

@(define max-load 1094)

Load times have improved for @|RacketCS|. Loading the
@racketmodname[racket/base] library:

@centerline{@one-plot["racket -l racket/base" #:cs (+ 89 28) #:r (+ 84 8) #:max max-load
                                  #:suffix " msec"
                                  #:desc "startup+load time"
                                  #:R "R/BC"]}

Loading the full @racketmodname[racket] library:

@centerline{@one-plot["racket -l racket"         #:cs (+ 280 44) #:r (+ 237 16)
                                                 #:max max-load
                                  #:suffix " msec"
                                  #:desc "startup+load time"
                                  #:R "R/BC"]}

Load times are, of course, directly related to memory use, and
@RacketCS load times improved primarily through reduced memory use.

@bmnotes{These results were gathered by using @tt{time} in a
shell a few times and taking the median. The command was as shown.}

@; --------------------------------------------------------------------------------
@section*{Memory Use}

Differences in memory use between @RacketCS and @RacketBC are mostly
due to bytecode versus machine code, plus the fact that @RacketBC can
load bytecode more lazily. Various improvements, including changes to
the Chez Scheme compiler to reduce code size, have decreased memory
use in @RacketCS by around 20% for typical applications.

The following plots show memory use, including both code and data,
after loading @racketmodname[racket/base] or @racketmodname[racket],
but subtracting memory use at the end of a run that loads no libraries
(which reduces noise from different ways of counting code in the
initial heap). The gray portion of each bar is an estimate of memory
occupied by code, which may be in machine-code form, bytecode form,
or not yet unmarshaled.

@(define cs-base 45958)
@(define cs-base-code (+ 14 2)) @; code plus bytevector
@(define r-base (+ 4530 580))
@(define r-base-code (+ 2822 593))
@(define drmax-mem 883000)
@(define max-mem (- 119626 cs-base))

@centerline{@one-plot["racket -l racket/base" #:cs (- 54269 cs-base)
                                              #:cs-sub (- (+ 1936 849 240)
                                                          cs-base-code)
                                              #:r (- (+ 9831 1596) r-base)
                                              #:r-sub (- (+ 2494 1634 796) ; bytecode + JIT-generated + marshaled
                                                         r-base-code)
                                              #:max max-mem
                                              #:display-scale 1/1024
                                              #:suffix " MB"
                                              #:vertical-bars? #t
                                  #:desc-inset 300
                                  #:desc "memory use after load"
                                  #:R "R/BC"]}

@centerline{@one-plot["racket -l racket" #:cs (- 93259 cs-base)
                                         #:cs-sub (- (+ 14032 6851 1787) ; code + bytevector + reloc
                                                     cs-base-code)
                                         #:r (- (+ 32760 2868) r-base)
                                         #:r-sub (- (+ 4202 2936 5067) r-base-code) ;  = 3.5
                                         #:max max-mem
                                         #:display-scale 1/1024
                                         #:suffix " MB"
                                         #:vertical-bars? #t
                                  #:desc-inset 300
                                  #:desc "memory use after load"
                                  #:R "R/BC"]}

@RacketBC heap sizes here are larger compared to previous reports by
about 5@|~|MB. That difference reflects a more accurate measurement of
the initial @RacketBC heap.

On a different scale and measuring peak memory use instead of final
memory use for DrRacket start up and exit:

@centerline{@one-plot["drracket" #:cs (- 503660 cs-base)
                                 #:cs-sub (- (+ 95138 45577 12569) cs-base-code)
                                 #:r (- (+ 328973 31280) r-base)
                                 #:r-sub (- (+ 33005 22731 31528) r-base-code)
                                 #:max drmax-mem
                                 #:display-scale 1/1024
                                 #:suffix " MB"
                                 #:vertical-bars? #t
                                 #:desc-inset 300
                                 #:desc "peak memory use for startup+exit"
                                 #:R "R/BC"]}

@bmnotes{These results were gathered by running @exec{racket} with the
arguments @exec{-l racket/base}, @exec{-l racket}, or @exec{-l
drracket}. The command further included @tt|{-W "debug@GC" -e
'(collect-garbage)' -e '(collect-garbage)'}|, and reported sizes are
based on the logged memory use before the second collection. For the
@RacketBC bar, the reported memory use includes the first number that is
printed by logging in square brackets, which is the memory occupied by
code outside of the garbage collector's directly managed space.
Baseline memory use was measured by setting the
@envvar{PLT_GCS_ON_EXIT} environment variable and running with
@exec{-n}, which is has the same effect as @exec{-e
'(collect-garbage)' -e '(collect-garbage)'}. DrRacket was initialized
with @racketmodname[racket/base] as the default language; also,
background expansion was disabled, because measuring memory use is
tricky on @|RacketBC|. Code size was estimate using
@racket[dump-memory-stats] counting bytecode, JIT-generated native
code, and marshaled code for @RacketBC and machine code, relocations,
and bytevectors (which are mostly marshaled code) for @|RacketCS|.}

@; --------------------------------------------------------------------------------
@section*{Expand and Compile Times}

Compile times have improved substantially for @|RacketCS|. The main
change was to use an interpreter for compile-time code within the
module currently being compiled, because that's a better trade-off in
(meta) compilation time and (meta) run time. Compile-time code that is
exported for use by other modules is compiled and optimized normally.

@centerline{@compile-time-plot[]}

While the @RacketCS interpreter and the one in @RacketBC are both
intended to be safe for space, the @RacketCS one stands a much better
chance of achieving that goal.

@bmnotes{These results were gathered by using @tt{time} in a
shell a few times and taking the median. The command was as shown.}

@; --------------------------------------------------------------------------------
@section*{Build Time}

The time and memory used to build a Racket distribution using
@RacketCS is now much closer to the time and memory used by
@|RacketBC|. The following plots are all on the same scale, and they
show memory use plotted against time for building the Racket
distribution from source:

@margin-note{In each plot, there are two lines, although they are
often smashed together. The top line is memory use just before a
major garbage collection, and the bottom line is memory use just after
a major garbage collection.}

@tabular[
 #:sep @hspace[1]
 #:column-properties '(top right)
(list
 (list @color[racket-color]{Racket CS} @image[#:scale 0.5 build-rcs.svg])
 (list @smaller{} @smaller{})
 (list @color[c-color]{Racket BC} @image[#:scale 0.5 build-r.svg]))
 ]

The @RacketCS plot used to be more than twice as wide as the @RacketBC
plot. About half of the improvement came from fixing a cache that
interacted badly with @RacketCS's more frequent minor garbage
collections. The rest of the improvement was due to many small
improvements.

@bmnotes{These plots were generated using the
@filepath{plt-build-plot} package, which drives a build from source
and plots the results.}

@; --------------------------------------------------------------------------------
@section*{Run-Time Performance Redux}

In some ways, the @RacketCS project has validated the @RacketBC
implementation, because it turns out that @RacketBC performs pretty
well. With the notable exception of first-class continuations (where
@RacketBC use a poor strategy), the traditional, JIT-based Racket
engine performs close to Chez Scheme.

Then again, development to date has been aimed at making @RacketCS
match the performance of @RacketBC on a code base that was developed
and tuned for @RacketBC, and that obviously gives @RacketBC an
advantage. For example, @RacketBC makes dormant code relatively cheap, so
Racket libraries generate a lot of code. Future Racket libraries will
likely shift to take advantage of @RacketCS's cheaper function calls
and dramatically cheaper continuations. One day, probably, @RacketBC
will no longer be a viable alternative to @RacketCS for most programs.

To make that prediction more concrete, consider these three ways of
counting to 10 million (if @racket[N] is @racket[10000000]):

@racketblock[
 (code:comment "self-contained loop")
 (for/fold ([v #f]) ([i (in-range N)])
   i)

 (code:comment "indirect function calls for loop control")
 (for/fold ([v #f]) ([i N]) (code:comment "no `in-range`")
   i)

 (code:comment "continuations")
 (let ([g (generator ()
            (for ([i (in-range N)])
              (yield i))
            #f)])
   (for/fold ([v #f]) ([i (in-producer g #f)])
     i))
]

Here are run times normalized to the first one, which is about the
same in @RacketCS and @|RacketBC|:

@(define left-space @hspace[5])

@tabular[
#:sep @hspace[2]
#:column-properties '(left right right)
(list
 (list left-space ""                 (color racket-color "R/CS") (color c-color "R/BC"))
 (list left-space @racket[in-range]               "×1"            "×1")
 (list left-space @elem{no @racket[in-range]}     "×5"            "×25")
 (list left-space @racket[generator]              "×335"          "×2672"))
 ]

While no one will start writing trivial loops the slow way, there's a
big difference between a ×5 and ×25 overhead when choosing how to
implement a new abstraction and deciding how much to make static
versus dynamic. There's an even bigger difference between ×335 and
×2672---both irrelevant for a toy loop, but ×335 becomes relevant
sooner than ×2672 for more interesting calculations. Overall, when
implementation choices start to rely on the @color[racket-color]{R/CS}
column, the @color[c-color]{R/BC} column will sometimes be
unacceptable.

@; --------------------------------------------------------------------------------
@section*{Reflection and Outlook}

It took three years to get Racket on Chez Scheme running well enough
for production use, and it will take yet more time for @RacketCS to
fully replace @|RacketBC|. But a certain amount of optimism is necessary
to take on a large project like this, and if the timeline gets
stretched beyond
@hyperlink["https://groups.google.com/d/msg/racket-dev/2BV3ElyfF8Y/4RSd3XbECAAJ"]{initial}
and
@hyperlink["https://blog.racket-lang.org/2018/01/racket-on-chez-status.html"]{early}
projections, then that's only to be expected.

@RacketCS will eventually outpace @RacketBC for the reason that
originally motivated porting Racket to Chez Scheme: it's put together
in a better way, so it's easier to modify and improve. Maintainability
is difficult to capture with the same clarity as performance
benchmarks, but after spending one more year modifying both
implementations, I remain as convinced as ever that @RacketCS is much
better.

This report is the last one for @|RacketCS|. Here are a few things that
are on the @RacketCS roadmap---but, increasingly, we'll just call it
the Racket roadmap:

@itemlist[

 @item{Support for embedding @RacketCS in a larger application.
       Probably the C API here will start with the Chez Scheme C API,
       which will make it different from @RacketBC's C API: providing
       similar functionality, but with simpler rules for cooperating
       with the memory manager.}

 @item{Improved garbage collection, especially for large heap sizes,
       including support for incremental collection.}

 @item{Unboxed floating-point arithmetic, especially for local
       compositions of floating-point operations.}

]
