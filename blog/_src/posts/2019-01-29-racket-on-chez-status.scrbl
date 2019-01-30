#lang scribble/manual

Title: Racket-on-Chez Status: January 2019
Date: 2019-01-29T00:00:00
Tags: Chez Scheme

@(require (submod "racket-on-chez-status-jan2019/bm-jan19.rkt" pict)
          "racket-on-chez-status-jan2019/color.rkt"
          (for-label ffi/unsafe)
          (only-in scribble/core
                   style
                   color-property)
          (only-in scribble/html-properties
                   render-convertible-as)
          racket/runtime-path)

@(define blog-mode? #t)

@(define (section* . l) @section[#:style (style #f (list 'unnumbered (render-convertible-as '(svg-bytes)))) l])

@(define jan18-url "http://blog.racket-lang.org/2018/01/racket-on-chez-status.html")

@(define insize-mz "http://docs.racket-lang.org/inside/index.html")

@(cond
  [blog-mode?
   @italic{posted by Matthew Flatt}]
  [else
   (list
    @title[#:version ""]{Racket-on-Chez Status: January 2019}
    @author[@elem{Matthew Flatt}])])

@(define (bmnotes . l) (smaller (italic (bold "About the measurements:") " " l)))
@(define (bmmorenotes . l) (smaller (italic l)))

@(define (color c . content) @elem[#:style (style #f (list (color-property c))) @bold[content]])

@(define-runtime-path build-rcs.svg "racket-on-chez-status-jan2019/bmtimes-jan19/build-rcs.svg")
@(define-runtime-path build-r.svg "racket-on-chez-status-jan2019/bmtimes-jan19/build-r.svg")
@(define-runtime-path build-rl.svg "racket-on-chez-status-jan2019/bmtimes-jan19/build-rl.svg")
@(define-runtime-path build-rlcs.svg "racket-on-chez-status-jan2019/bmtimes-jan19/build-rlcs.svg")
@(define-runtime-path build-rlr.svg "racket-on-chez-status-jan2019/bmtimes-jan19/build-rlr.svg")

@margin-note{For background information about Racket on Chez Scheme
(a.k.a. Racket CS), see
@hyperlink["https://groups.google.com/d/msg/racket-dev/2BV3ElyfF8Y/4RSd3XbECAAJ"]{the
original announcement}, @hyperlink[jan18-url]{last January's report},
and @hyperlink["https://www.youtube.com/watch?v=t09AJUK6IiM"]{the
report at Scheme Workshop 2018}.}

Racket on Chez Scheme is @emph{done} in a useful sense. All
functionality is in place, DrRacket CS works fully, the main Racket CS
distribution can build itself, and 99.95% of the core Racket test
suite passes.

You can download a build for Windows, Linux, or Mac OS from the Utah
snapshot site (look for ``Racket CS''):

@centerline{@url{https://www.cs.utah.edu/plt/snapshots/}}

While code generally runs as fast as it should, end-to-end performance
is not yet good enough to make Racket CS the default
implementation of Racket. We'll let the implementation settle and
gradually improve, with the expectation that it will eventually be
good enough to switch over---and better in the long run.

@; --------------------------------------------------------------------------------
@section*{Compatibility with the Current Racket Implementation}

Racket CS is intended to behave the same as the existing Racket
implementation with a few exceptions:

@itemlist[

 @item{no single-precision or extended-precision flonums;}

 @item{some differences in the FFI related to
       @racketlink[malloc]{memory management} and
       @racketlink[_cprocedure]{blocking functions}; and}

 @item{no support for Racket's @hyperlink[insize-mz]{C API}.}

]

There are still a few internal gaps related to handling large numbers
of file descriptors (needed by servers with lots of connections, for
example) and support for file-change events. Those should be easy to
fill in, but our focus right now is on flushing out the remaining bugs
that are exposed by test suites.

Another kind of incompatibility is that the compiled form of Racket
code with the current implementation is platform-independent bytecode,
while Racket CS's compiled form is platform-specific machine code.
This difference can sometimes affect a development workflow, and it
required adjustments to the distribution-build process. Racket CS does
not yet support cross compilation.

Here's an incomplete list of things that @bold{are} compatible between
the current Racket implementation and Racket CS and that required
some specific effort:

@nested[#:style 'inset]{Macros, modules, threads, futures, places,
       custodians, events, ports, networking, string encodings, paths,
       regular expressions, mutable and immutable hash tables,
       structure properties and applicable structures, procedure
       arities and names, chaperones and impersonators, delimited
       continuations, continuation marks, parameters, exceptions,
       logging, security guards, inspectors, plumbers,
       reachability-based memory accounting, ephemerons, ordered and
       unordered finalization, foreign-function interface (mostly),
       phantom byte strings, source locations, left-to-right
       evaluation, result-arity checking, left-associative arithmetic,
       @racket[eqv?] on NaNs, and @racket[eq?] and flonums.}

@; --------------------------------------------------------------------------------
@section*{Outlook}

The rest of this report will provide lots of numbers, but none of them
expose the main benefit of Racket CS over the current Racket
implementation: it's more flexible and maintainable.

Putting a number on maintainability is less easy than measuring
benchmark performance. Anecdotally, as the person who has worked on
both systems, I can report that it's no contest. The current Racket
implementation is fundamentally put together in the wrong way (except
for the macro expander), while Racket CS is fundamentally put together
in the right way. Time and again, correcting a Racket CS bug or adding
a feature has turned out to be easier than expected.

To maximize the maintenance benefits of Racket CS, it's better to make
it the default Racket variant sooner rather than later---and, ideally,
discard the current Racket implementation. But while Racket CS is
compatible with Racket to a high percentage, it's never going to be
100%. From here, it's some combination of patching differences and
migrating away from irreconcilable differences, and that will take a
little time. Given that both implementations need to exist for a
while, anyway, we can given some weight to end-to-end performance when
deciding on the right point to switch.

Many plots in this report are intended to tease out reasons for the
performance difference between Racket CS and current Racket. From the
explorations, so far, its does @emph{not} appear that the performance
difference is an inevitable trade-off from putting Racket together in
a better way. Part of the problem is that some new code on top of Chez
Scheme needs to be refined. Perhaps more significantly, there are some
trade-offs in the space of compilation timing (ahead-of-time or
just-in-time) and code representation (machine code versus bytecode)
that we can adjust with more work.

Although the current Racket implementation and Racket CS will both
exist for a while, we do not anticipate the dueling implementations to
create problems for the Racket community. The question of which to use
will be more analogous to ``which browser works best for your
application?'' than ``does this library need Python 2 or Python 3?''

Meanwhile, there's even more code to maintain, and accommodating
multiple Racket variants creates some extra complexity by itself
(e.g., in the distribution builds). It still looks like a good deal in
the long run.

@; --------------------------------------------------------------------------------
@section*{Performance of Compiled Code}

The plots below show timings for Chez Scheme (purple), Racket CS
(blue), and current Racket (red) on traditional Scheme benchmarks.
Shorter is better. The results are sorted by Chez Scheme's time over
Racket's time, except that benchmarks that rely on mutable pairs are
in a second group with green labels.

@centerline{@traditional-benchmarks*[]}

@itemlist[

 @item{Note that the break-even point between Chez Scheme and Racket
       is toward the end of the set of benchmarks with black lables,
       which reflects that Chez Scheme is usually faster than current
       Racket.}

 @item{The main result is that the blue bar tracks the purple bar
       fairly well for the benchmarks without mutable pairs: Racket
       CS's layers on top of Chez Scheme are not interfering too much
       with Chez Scheme's base performance, even though Racket CS
       wraps and constrains Chez Scheme in various ways (e.g.,
       enforcing left-to-right evaluation of application arguments).}

 @item{For the benchmarks that use mutable pairs (green labels),
       Racket CS loses some of Chez Scheme's performance by
       redirecting mutable-pair operations away from the built-in pair
       datatype, since built-in pairs are used only for immutable
       pairs in Racket CS.}

 @item{The @tt{tak} variants where the blue bar is shortest may be due
       to an extra layer of function inlining. The @tt{collatz} test
       is effectively a test of exact-rational arithmetic on large
       fractions.}

]

The next set of plots compare Racket CS and current Racket on the
Racket implementations of benchmarks that were written over the years
for
@hyperlink["https://benchmarksgame-team.pages.debian.net/benchmarksgame/"]{The
Computer Language Benchmarks Game}. These rely more heavily on
Racket-specific language features. Racket CS's slowness toward the end
of the list is often due to the I/O implementation, which is newly
implemented for Racket CS and will take time to refine.

@centerline{@shootout-benchmarks*[]}

Aside from the fact that I/O needs work in Racket CS, the takeaway
here is that there are no huge problems nor huge performance benefits
with the Racket CS implementation. Longer term, the red lines probably
aren't going to move, but because so much new code is involved with
the blue lines, there's reason to think that some blue lines can get
shorter.

@bmnotes{These benchmarks are in the
@hyperlink["https://github.com/racket/racket/tree/master/pkgs/racket-benchmarks/tests/racket/benchmarks"]{@filepath{racket-benchmark}}
package in the @filepath{common} and @filepath{shootout} directories.
We used commit @tt{f6b6f03401} of the Racket fork of Chez Scheme and
commit @tt{c9e3788d42} of Racket. The Chez Scheme fork includes
Gustavo Massaccesi's ``cptypes'' pass, which improves Chez Scheme's
performance on a few benchmarks. The test machine was a Core i7-2600
3.4GHz running 64-bit Linux.}

@; --------------------------------------------------------------------------------
@section*{Startup and Load Times}

Startup and load time have improved since previous reports, but
Racket CS remains slower.

Startup for just the runtime system without any libraries (still on a
Core i7-2600 3.4GHz running 64-bit Linux):

@(define max-load 1094)

@centerline{@one-plot["racket -n" #:cs 73 #:r 49 #:max max-load
                                  #:suffix " msec"
                                  #:desc "startup time"]}

The difference here is that the Racket CS startup image has much
more Scheme and Racket code that is dynamically loaded and linked,
instead of loaded as a read-only code segment like the compiled C code
that dominates the current Racket implementation. We can illustrate
that effect by building the current Racket implementation in a mode
where its Racket-implemented macro expander is compiled to C code
instead of bytecode, too, shown below as ``R/cify.'' We can also
compare to Racket v6, which had an expander that was written directly
in C:

@centerline{@one-plot["racket -n" #:cs 73 #:r 49
                                  #:r-cify 19
                                  #:r6 14
                                  #:max max-load
                                  #:suffix " msec"
                                  #:desc "startup time"]}

The gap widens if we load compiled Racket code. Loading the
@racketmodname[racket/base] library:

@centerline{@one-plot["racket -l racket/base" #:cs 132 #:r 93 #:max max-load
                                  #:suffix " msec"
                                  #:desc "startup+load time"]}

The additional difference here is that Racket CS's machine code
is bigger than current Racket's bytecode representation. Furthermore,
the current Racket implementation is lazy about parsing some bytecode.
We can tease out the latter effect by disabling lazy bytecode loading
with the @Flag{d} flag, shown as ``R/all'':

@centerline{@one-plot["racket -l racket/base"    #:cs 132 #:r 93
                                                 #:r-all 120
                                                 ;; #:r-jit 269
                                                 #:max max-load
                                  #:suffix " msec"
                                  #:desc "startup+load time"]}

(We could also force bytecode to be JITted immediately---but JITting
is more work than just loading, so that timing result would not be
useful.)

@elemtag["racket-bytecode-load"]{We} get a similar shape and a larger
benefit from lazy loading with the @racketmodname[racket] library,
which is what the @exec{racket} executable loads by default for
interactive mode:

@centerline{@one-plot["racket -l racket"         #:cs 483 #:r 263
                                                 #:r-all 408
                                                 ;; #:r-jit 1094
                                                 #:max max-load
                                  #:suffix " msec"
                                  #:desc "startup+load time"]}

@bmnotes{These results were gathered by using @tt{time} in a
shell a few times and taking the median. The command was as shown, but
using @exec{racketcs} for the ``R/CS'' lines and @tt{racket -d} for
the ``R/all'' lines.}

@; --------------------------------------------------------------------------------
@section*{Memory Use}

Like load times, differences in memory use between Racket CS and
current Racket can be attributed to code-size differences from
bytecode versus machine code and by lazy bytecode loading.

The following plots show memory use, including both code and data,
after loading @racketmodname[racket/base] or @racketmodname[racket],
but subtracting memory use at the end of a run that loads no libraries
(which reduces noise from different ways of counting code in the
initial heap). The ``R/jit!'' line uses @Flag{d} to load all bytecode
eagerly, and it further forces that bytecode to be compiled to native
code by the JIT compiler:

@(define cs-base 42775)
@(define r-base (+ 9478 612))
@(define drmax-mem 883000)
@(define max-mem (- 119626 cs-base))

@centerline{@one-plot["racket -l racket/base" #:cs (- 52788 cs-base) #:r (- (+ 9747 1680) r-base)
                                              #:r-all (- (+ 12336 1680) r-base)
                                              #:r-jit (- (+ 10606 17137) r-base)
                                              #:max max-mem
                                              #:display-scale 1/1024
                                              #:suffix " MB"
                                              #:vertical-bars? #t
                                  #:desc "memory use after load"]}

@centerline{@one-plot["racket -l racket" #:cs (- 101682 cs-base) #:r (- (+ 32370 3096) r-base)
                                         #:r-all (- (+ 52381 3104) r-base)
                                         #:r-jit (- (+ 47755 39028) r-base)
                                         #:max max-mem
                                         #:display-scale 1/1024
                                         #:suffix " MB"
                                         #:vertical-bars? #t
                                  #:desc "memory use after load"]}

These results show that bytecode is more compact than machine code,
as expected. Lazy parsing of bytecode also makes a substantial
difference in memory use for the current Racket implementation.
Racket's current machine code takes a similar amount of space as Chez
Scheme machine code, but the JIT overhead and other factors make it
even larger. (Bytecode is not retained after conversion to machine
code by the JIT.)

On a different scale and measuring peak memory use instead of final
memory use for DrRacket start up and exit:

@centerline{@one-plot["drracket" #:cs (- 729903 cs-base) #:r (- (+ 325144 37092 (+ 81898 6872)) r-base)
                                 #:r-all (- (+ 370778 34944 (+ 121307 6172)) r-base)
                                 #:r-jit (- (+ 348111 194232 (+ 120623 51632)) r-base)
                                 #:max drmax-mem
                                 #:display-scale 1/1024
                                 #:suffix " MB"
                                 #:vertical-bars? #t
                                 #:desc "peak memory use for startup+exit"]}

This result reflects that DrRacket's memory use is mostly the code
that implements DrRacket, at least if you just start DrRacket and
immediately exit.

The gap narrows if you open an earlier version of this document's
source and run it three times before exiting, so that memory use
involves more than mostly DrRacket's own code:

@centerline{@one-plot["drracket" #:cs (- 919090 cs-base)
                                 #:r (- (+ 409813 37780 (+ 248077 12264)) r-base)
                                 #:r-all (- (+ 465260 37892 (+ 246046 10980)) r-base)
                                 #:r-jit (- (+ 440761 175188 (+ 220002 79048)) r-base)
                                 #:max drmax-mem
                                 #:display-scale 1/1024
                                 #:suffix " MB"
                                 #:vertical-bars? #t
                                 #:desc "peak memory use for startup, run x3, exit"]}

@bmnotes{These results were gathered by running @exec{racket}
or @exec{racketcs} starting with the arguments @exec{-l racket/base},
@exec{-l racket}, or @exec{-l drracket}. The command further included
@tt|{-W "debug@GC" -e '(collect-garbage)' -e '(collect-garbage)'}| and
recording the logged memory use before that second collection. For the
``R'' line, the reported memory use includes the first number that is
printed by logging in square brackets, which is the memory occupied by
code outside of the garbage collector's directly managed space. For
``R/all,'' the @Flag{d} flag is used in addition, and for ``R/jit!,''
the @tt{PLT_EAGER_JIT} environment variable was set in addition to
supplying @tt{-d}.}

@; --------------------------------------------------------------------------------
@section*{Expand and Compile Times}

Compile times have improved some for Racket CS since the original
report, but not dramatically. These plots compare compile times from
source for the @racketmodname[racket/base] module (and all of its
dependencies) and the @racketmodname[racket] module (and
dependencies):

@centerline{@compile-time-plot[]}

Compilation requires first macro-expanding source, and that's a
significant part of the time for loading from source. Racket CS and
current Racket use the same expander implementation, and they expand
at practically the same speed, so the extra time in Racket CS can be
attributed to machine-code compilation. The following plots show how
parts of the compile time can be attributed to specific subtasks:

@centerline{@compile-time-detail-plot[]}

@elemtag["racket-expanded-load"]{Another} way to look at compile times
is to start with modules that are already expanded by the macro
expander and just compile them. The @Flag{M} flag alone does not do
that, but it's meant here to represent an installation that was
constructed by using the @Flag{M} flag for all build steps:

@centerline{@recompile-time-plot[]}

The difference in these compile times reflects how Chez Scheme puts
much more effort into compilation. Of course, the benefit is the
improved run times that you see in so many benchmarks.

The compile-only bars are also significantly shorter than taking the
expansion-plus-compilation bars and removing only the gray part.
That's because the gray part only covers time spent specifically in
the macro expander or running macro transformers, but it does not
cover the time to compile macro definitions as they are discovered
during expansion or to instantiate modules for compile-time use.

Given that the Chez Scheme compiler is so much slower (for good
reason) than the current Racket compiler, we might ask how it compares
to other, non-Racket compilers. Fortunately, we can make a relatively
direct comparison between C and Racket, because the Racket macro
expander was formerly written in C, and now it is written in Racket
with essentially the same algorithms and architecture (only nicer).
The implementations are not so different in lines of code: 45 KLoC in
C versus 28.5 KLoC in Racket. The following plot shows compile times
for the expander's implementation:

@(define (/~ n) (* 100 (floor (/ n 100))))

@centerline{
@general-plot[ #:suffix " msec"
               #:desc "compile time"
             (list "CS" "R" "C")
             (list scheme-color r-jit-color plain-c-color)
             (list
              (hash 'expander (/~ 12480))
              (hash 'expander (/~ 1390))
              (hash 'expander (/~ (- (+ 74261 1928) (+ 65513 1885)))))]
}

To further check that we're comparing similar compilation tasks, we
can check the size of the generated machine code. Toward that end, we
can compile the Racket code to C code through a @tt{cify} compiler,
which is how the expander is compiled for the current Racket
implementation for platforms that are not supported by Racket's JIT.
Below is a summary of machine-code sizes for the various compiled
forms of the expander.

@(define (// n) (* 100 (floor (/ n 1024 100))))

@centerline{
@general-plot[ #:suffix " KB"
               #:desc "machine code size"
               #:vertical-bars? #t
             (list "CS" "R/jit!" "R/jit!/no" "R/cify" "C")
             (list scheme-color r-jit-color r-jit-color cify-color plain-c-color)
             (list
              (hash 'expander (// (- 3331312 925568))) ; object-count diffs after pre-reqs; compiling as program instead of library cuts 925568
              (hash 'expander (// 4761632)) ; via PLT_EAGER_JIT=y PLT_LINKLET_TIMES=y
              (hash 'expander (// 3018048)) ; initialize inline-fuel to 0, rebuild cstartup.inc
              (hash 'expander (// 1809823)) ; cify: cify 6081768 minus non-cify 4,955,680 plus .zo size 683,735
              (hash 'expander (// 1006323))) ; delta of v6.12 5,278,268 and v7.2.03 4,955,680 minus .zo size 683,735
]}

The current Racket implementation generates much more code from the
same implementation, in part because it inlines functions aggressively
and relies on the fact that only called code is normally translated to
machine code; the ``R/jit!/no'' bar shows the code size when inlining
is disabled. In any case, while the machine-code sizes vary quote a
bit in this test, they're all on the same general scale.

In summary, as an extensible language, the question of compile times
is more complicated than for a conventional programming language. At
the core-compiler level, current Racket manages to be very fast as a
compiler by not trying hard. Racket CS, which gets its compile times
directly from Chez Scheme, spends more time compiling, but it still
has respectable compile times.

@bmnotes{The numbers in compile-time plots come from running
the shown command (but with @exec{racketcs} instead of @exec{racket}
for the ``R/CS'' lines) with the @tt{PLT_EXPANDER_TIMES} and
@tt{PLT_LINkLET_TIMES} environment variables set. The overall time is
as reported by @tt{time} for user plus system time, and the divisions
are extracted from the logging that is enabled by the environment
variables.}

@bmmorenotes{For measuring compile times on the expander itself, the
Chez Scheme measurement is based on the build step that generates
@filepath{expander.so}, the current-Racket measurement is based on the
build step that generates @filepath{cstartup.inc}, and the C
measurement is based on subtracting the time to rebuild Racket version
6.12 versus version 7.2.0.3 when the @filepath{.o} files in
@filepath{build/racket/gc2} are deleted.}

@bmmorenotes{For measuring machine-code size, the expander's code size
for Chez Scheme was computed by comparing the output of
@tt{object-counts} after loading all expander prerequsites to the
result after the expander; to reduce the code that is just form the
@tt{library} wrapper, the expander was compiled as a program instead
of as a library. The code size for Racket was determined by setting
@tt{PLT_EAGER_JIT} and @tt{PLT_LINKLET_TIMES} and running @exec{racket
-d -n}, which causes the expander implemtation to be JITted and total
bytes of code generated by the JIT to be reported. The ``R/no-inline''
variant was the same, but compiling the expander to bytecode with
@racket[compile-context-preservation-enabled] set to @racket[#f],
which disables inlining. The ``R/cify'' code size was computed by
taking the difference on sizes of the Racket shared library for a
normal build and one with @DFlag{enable-cify}, after stripping the
binaries with @exec{strip -S}, then further subtracting the size of
the expander's bytecode as it is embedded in the normal build's shared
library. The ``C'' code size was similarly computed by subtracting the
size of the Racket shared library for version 7.2.0.3 from the size
for the 6.12 release, stipped and with the expander bytecode size
subtracted.}

@; --------------------------------------------------------------------------------
@section*{Build Time}

Since Racket programs rely heavily on metaprogramming
facilities--either directly or just by virtue of being a Racket
program---the time required to build a Racket program depends on a
combination of compile time, run time, and load time. Few Racket
programmers may care exactly how long it takes to build the Racket
distribution itself, but distribution-build performance is probably
indicative of how end-to-end performance will feel to a programmer
using Racket.

@centerline{@build-desc[]}

The following plots are all on the same scale, and they show memory
use plotted against time for building the Racket distribution from
source. The first two plots are essentially the same as in the
@hyperlink[jan18-url]{January 2018} report. While the graph stretches
out horizontally for Racket CS, showing a build that takes about three
times as long, it has very much the same shape for memory use.

@tabular[
 #:sep @hspace[1]
 #:column-properties '(top right)
(list
 (list @color[racket-color]{Racket CS} @image[#:scale 0.5 build-rcs.svg])
 (list @smaller{} @smaller{})
 (list @color[c-color]{current Racket} @image[#:scale 0.5 build-r.svg]))
 ]

One might assume that the difference in compile time explains the
slower Racket CS build. However, this assumption does not hold up if
we completely isolate the step of compiling fully expanded modules. To
set up that comparison, the following plots show build activity when
using current Racket and making ``compile'' just mean ``expand.'' It
happens to take about the same time as a Racket CS build, but with
more of the time in the documentation phases:

@tabular[
 #:sep @hspace[1]
 #:column-properties '(top right)
(list
 (list @color[c-color]{current Racket -M} @image[#:scale 0.5 build-rl.svg]))
 ]

Although current Racket compiles from expanded source relatively
quickly, a build requires loading the some modules over and over for
compiling different sets of libraries and running different
documentation examples. The documentation running and rendering
phases, as shown in the blue in green regions, are especially show and
use especially much memory, because documentation often uses sandboxes
that load libraries to run and render examples. (The big jump at the
same point in the blue and green region merits further investigation.
It might be a sandbox bug or a leaky unsafe library.)

Given the result of the expand-only build as an input, we can switch
in-place to either Racket CS or normal-mode current Racket and compile
each fully expanded module to machine code:

@tabular[
 #:sep @hspace[1]
 #:column-properties '(top right)
(list
 (list @color[racket-color]{Racket CS finish} @image[#:scale 0.5 build-rlcs.svg])
 (list @smaller{} @smaller{})
 (list @color[c-color]{current Racket finish} @image[#:scale 0.5 build-rlr.svg]))
 ]

Each module is compiled from expanded form just once, and that
compiled form can be used as needed (for cross-module optimization) to
compile other modules. Also, documentation doesn't get re-run and
re-rendered in this finishing build, because the build process can
tell that the sources did not change. Overall, compilation finishes in
under 20 minutes for Racket CS, which is a reasonable amount of time
for 1.2 million lines of source Racket code.

These build-finishing plots illustrate how the Racket distribution
server generate bundles for multiple platforms and variants in hours
instead of days. The build server first creates expanded-module builds
of the packages and main collections, and it serves those to
machine-specific finishing builds.

@bmnotes{These plots were generated using the
@filepath{plt-build-plot} package, which drives a build from source
and plots the results. The @Flag{M} build was created by setting the
@tt{PLT_COMPILE_ANY} environment variable, and then the finishing
builds were measured by another run on the result but using the
@DFlag{skip-clean} flag for @filepath{plt-build-plot}.}

@; --------------------------------------------------------------------------------
@section*{Implementation Outlook}

Based on the data that we've collected so far, I see three directions
toward improving end-to-end performance for Racket CS:

@itemlist[

 @item{Improvements to new implementation of Racket's I/O API.}

 @item{Better support in Chez Scheme to trade performance for faster
       compilation, combined at the Racket CS level with a
       bytecode-and-JIT setup that supports lazy decoding of bytecode.

       The @hyperlink[jan18-url]{January 2018} report mentions an
       experimental JIT mode for Racket CS, and that alternative
       remains in place. At the moment, it's not a good alternative to
       Racket CS's default mode, but it still may be a step in the
       right direction, especially considering that it allows
       JIT-style compilation and ahead-of-time compilation to
       coexist.}

 @item{Algorithmic improvements to the way macros and modules work.

       That the full expansion stack takes 10 times as long as core
       compilation for Racket libraries suggests that there is room
       for algorithmic improvements that would help both the current
       Racket implementation and Racket CS.}

]

There are bound to be additional performance factors that we haven't
yet isolated. Whether it turns out to be the factors that we know or
others, working in the new implementation of Racket will make it
easier explore the solutions.
