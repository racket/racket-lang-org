#lang scribble/manual

Title: Racket Compiler and Runtime Status: January 2021
Date: 2021-01-24T00:00:00
Tags: Chez Scheme

@(require racket/runtime-path
          (only-in scribble/html-properties
                   render-convertible-as)
          (submod "racket-status-jan2021/bm-jan21.rkt" pict)
          "racket-status-jan2021/color.rkt"
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
@(define feb20-url "http://blog.racket-lang.org/2020/02/racket-on-chez-status.html")

@(define-runtime-path build-cs-svg "racket-status-jan2021/bmtimes-jan21/build-cs.svg")
@(define-runtime-path build-r-svg "racket-status-jan2021/bmtimes-jan21/build-r.svg")

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
    @title[#:version ""]{Racket Compiler and Runtime Status: January 2021}
    @author[@elem{Matthew Flatt}])])

@(define (bmnotes . l) (smaller (italic (bold "About the measurements:") " " l)))
@(define (bmmorenotes . l) (smaller (italic l)))

@(define (color c . content) @elem[#:style (style #f (list (color-property c))) @bold[content]])

With the upcoming version 8.0 release, Racket on Chez Scheme (Racket
CS) will become the default implementation of Racket. This report discusses the
implications of that change, continuing a series that was originally
about Racket CS; the most recent previous report was
@hyperlink[feb20-url]{the February 2020 report}. The original
@hyperlink[jan18-url]{January 2018 report} explains the motivation
behind throwing out around 200k lines of C code to replace it with around
150k lines of Scheme and Racket code.

@; --------------------------------------------------------------------------------
@section*{Switching to Racket CS}

For most users, the differences between Racket CS and Racket BC
(``before Chez'') may be too small to notice. The installer and
executables will be larger, because they contain machine code instead
of bytecode, and some programs may run a little faster. Otherwise,
Racket programs are supposed to run the same.

To test this, we've built and tested every package available from
@hyperlink["https://pkgs.racket-lang.org"]{the package server} on
Racket CS over the past several months and compared the
@hyperlink["https://github.com/racket/racket/issues/3457"]{results}
with those from Racket BC version 7.9. Out of more than 1800 packages,
currently only 12 now fail to compile, and 5 others fail some of their
tests. The new build failures, as well as 2 of the 5 new test
failures, are the result of changes at the C API level. Several
packages already have pull requests to fix them, and we hope others
will be fixed soon, too. The remaining 3 test failures are differences
in thread scheduling that provoked existing bugs in the packages.

Although Racket CS will be the default for v8.0, Racket BC will
remain available through a ``More Installers'' link on the download
page for the foreseeable future. So, Racket users will still have the
option of falling back to the old runtime system and compiler if
something goes wrong, or if they need a package that does not yet work
on Racket CS.

@; --------------------------------------------------------------------------------
@section*{Progress in the Past Year}

During 2020, Racket CS became a little faster and a little smaller,
and many corners of the implementation were repaired or made more
compatible with the BC implementation. The planned changes that were
on the @hyperlink[feb20-url]{previous report's roadmap} all happened!
In addition, we introduced an AArch64 backend and ported to run
natively on M1 Macs. Finally, we made the Chez Scheme garbage
collector run in parallel, which brought the performance of
places-based parallelism in Racket CS about on par with BC.

Building Racket CS no longer involves first building Racket BC as a
way to bootstrap Chez Scheme. Instead, Chez Scheme bootstraps itself
on any supported platform using a new portable bytecode (pb) backend.
Bootfiles for pb mode are checked into a separate Git repository that
is used in a submodule-like way, but bootfiles can always be built
from source by using an existing Racket build; the bootstrap
implementation is now detangled so that much older Racket versions in
the v7 series can work for bootstrapping.

@; --------------------------------------------------------------------------------
@section*{Benchmark Performance}

For traditional Scheme benchmarks, not much has changed in the
results. A plot is practically indistinguishable from the one in the
@hyperlink[feb20-url]{previous report}.

For the shootout benchmarks, which measure more Racket-specific
functionality and libraries, there's some improvement, especially
toward the end of the table. (Shorter is better.) The light-blue line
shows the results from the previous report (which corresponds to
version 7.6), so a shorter ``CS'' bar means improvement there. The
improvements mostly reflect unboxed floating-point arithmetic.

@centerline{@shootout-benchmarks-table*[]}

Below is another set of microbenchmarks from the
@tt{racket-benchmarks} package, comparing Racket CS to BC. Some points
to note:

@itemlist[

 @item{CS is usually faster than BC. (Shorter is better.)}

 @item{The fastest relative CS runs are much faster than the fastest
       relative BC runs; that is, the shortest blue bars are much
       shorter than the shortest red bars. Although it's difficult to
       extract from these plots, the difference is really that CS
       performs more consistently, so it's slowest relative runs are
       not as slow as the slowest relative BC runs.}

 @item{These benchmarks mostly compare subsystems that are implemented
       in C for BC to subsystems implemented in Scheme and Racket for
       CS. That's the key and motivating point --- the maintenance
       advantage, more than the performance advantage.}

]

@centerline{@control-benchmark-plot[]}

For the rest of this report, the plots will show CS slower or bigger,
but gradually approaching BC performance. You should not read that as
``CS is slower than BC.'' This report is about the parts of the
implementation where we spent time last year, and we don't dwell on
parts where no improvement was needed.

@bmnotes{Benchmarks are in the
@hyperlink["https://github.com/racket/racket/tree/master/pkgs/racket-benchmarks/tests/racket/benchmarks"]{@filepath{racket-benchmark}}
package in the @filepath{shootout}, @filepath{control},
@filepath{hash}, and @filepath{chaperone} directories. We dropped some
of the @filepath{control} benchmarks because they take to long to run
on BC, which means that the plots here understate how much more
consistently CS performs compared to BC.}

@; --------------------------------------------------------------------------------
@section*{Startup and Memory Use}

@(define max-load 400)

Load times improved for Racket CS:

@centerline{@one-plot["racket -l racket"         #:old-cs (+ 280 44) #:cs (+ 229 56) #:r (+ 221 32)
                                                 #:max max-load
                                  #:suffix " msec"
                                  #:desc "startup+load time"
                                  #:old-CS "CS v7.6"
                                  #:CS "CS"
                                  #:R "BC"]}

That's mostly due to reduced code size:

@(define old-cs-base 45958)
@(define cs-base 43182)
@(define r-base (+ 4573 588))
@(define drmax-mem 883000)
@(define max-mem (- 119626 cs-base))

@centerline{@one-plot["racket -l racket" #:old-cs (- 93259 old-cs-base)
                                         #:cs (- 83863 cs-base)
                                         #:r (- (+ 31550 2752) r-base)
                                         #:max max-mem
                                         #:display-scale 1/1024
                                         #:suffix " MB"
                                         #:vertical-bars? #t
                                  #:desc-inset 300
                                  #:desc "memory use after load"
                                  #:old-CS "CS v7.6"
                                  #:CS "CS"
                                  #:R "BC"]}

On a different scale and measuring peak memory use instead of final
memory use for DrRacket start up and exit:

@centerline{@one-plot["drracket" #:old-cs (- 503660 old-cs-base)
                                 #:cs (- 389698 cs-base)
                                 #:r (- (+ 288058 28060) r-base)
                                 #:max drmax-mem
                                 #:display-scale 1/1024
                                 #:suffix " MB"
                                 #:vertical-bars? #t
                                 #:desc-inset 300
                                 #:desc "peak memory use for startup+exit"
                                 #:old-CS "CS v7.6"
                                 #:CS "CS"
                                 #:R "BC"]}

The large drop in peak memory use for DrRacket CS is due to
garbage-collection improvements for old generations in large heaps,
where old-generation objects tend to be marked in place instead of
copied.

@bmnotes{These results were gathered by running @exec{racket} with the
arguments @exec{-l racket} or @exec{-l
drracket}. The command further included @tt|{-W "debug@GC" -e
'(collect-garbage)' -e '(collect-garbage)'}|, and reported sizes are
based on the logged memory use before the second collection. For the
``BC'' line, the reported memory use includes the first number that is
printed by logging in square brackets, which is the memory occupied by
code outside of the garbage collector's directly managed space.
Baseline memory use was measured by setting the
@envvar{PLT_GCS_ON_EXIT} environment variable and running with
@exec{-n}, which is has the same effect as @exec{-e
'(collect-garbage)' -e '(collect-garbage)'}. DrRacket was initialized
with @racketmodname[racket/base] as the default language; also,
background expansion was disabled, because measuring memory use is
tricky on Racket BC.}

@; --------------------------------------------------------------------------------
@section*{Build Times}

Compile times improved for Racket CS:

@centerline{@compile-time-plot[]}

This improvement, combined with others, makes the CS distribution now
build a little faster from source than a BC build, and it uses about
20% less memory than the BC build. The following two
plots use the same scale, where the foreground blue or red line shows
memory use (vertical) plotted over time (horizontal) as recorded on
each major collection (see
@hyperlink["https://build-plot-cs.racket-lang.org/about.html"]{this
page} for a detailed description):

@tabular[
 #:sep @hspace[1]
 #:column-properties '(top right)
(list
 (list @color[racket-color]{Racket CS} @image[#:scale 0.5 build-cs-svg])
 (list @smaller{} @smaller{})
 (list @color[c-color]{Racket BC} @image[#:scale 0.5 build-r-svg]))
 ]

A shorter build time in less space represents a big milestone for
Racket CS. Even though most Racket users start with a pre-built
distribution, build time measures end-to-end performance across many
parts of the implementation, and build performance correlates well
with end-to-end performance in many Racket applications.

The build plots above are for a sequential (i.e., single-threaded)
build. More typically, Racket is built on machines with more CPUs, and
build times easily benefit from process-like parallelism. Over the
past year, garbage-collector improvements for parallel collection have
made build times with place-based parallelism dramatically shorter on
a machine with 4 hyperthreaded cores (although build dependencies
ultimately limit parallelism). Since parallel collection debuted in
Racket CS v7.9 but has improved since, we show v7.9 as an intermediate
point:

@centerline{@parallel-build-plot[]}

As the plot shows, as of the previous report (before parallel garbage
collection), trying to build with multiple places was
counterproductive. The speedup from places-based parallelism in v8.0
is finally close to BC's speedup, which is about the same as using
process-based parallelism (but with the advantage of staying within a
single Racket process). The parallelism available from BC remains a
little higher, because its implementation of places uses completely
separate allocation heaps, while parallelism in CS use a single heap.

@bmnotes{The compile-time results were gathered by using @tt{time} in
a shell a few times and taking the median. The build plots were
generated using the @filepath{plt-build-plot} package, which drives a
build from source and plots the results. The parallelism results were
generated by starting with an in-place installation, using @tt{raco
setup --fast-clean} and @tt{rm doc/docindex.sqlite} to start from a
clean state, and getting elapsed real time with @tt{time racro setup
--jobs 8}, and dividing by elapsed time from build plots (which is not
exactly the same build job, but is close).}

@; --------------------------------------------------------------------------------
@section*{Reflection and Outlook}

We tried to declare success and move on @hyperlink[feb20-url]{a year
ago}, but that didn't work. This time really is different, because
Racket CS is ready to be the default Racket implementation. Time will
tell whether that difference is enough to allow a different focus in
the coming year. Given how much the CS compilation strategy
(ahead-of-time) differs from the BC strategy (bytecode compilation
plus a JIT), it's not clear that the compile time, load time, and
memory footprint differences between BC and CS can be reduced further.

In terms of maintainability, one possible direction for improvement is
to shift even more Racket support into the Chez Scheme layer.
Gustavo's decision to implement type reconstruction as a Chez Scheme
pass was good call. Contributor @tt{yjqww6}'s implementation of
Racket-style lifting as a Chez Scheme pass was a clear improvement for
Racket CS, because it eliminated a larger pass on the ``schemify''
side and implemented at a better layer of abstraction with less
interference on local optimizations. Similarly, extending Chez
Scheme's support for weak hash tables, which was not difficult, allowed us
to discard a complex and slower implementation in the ``rumble''
Racket-adapter layer. More direct support for left-to-right evaluation
guarantees and cross-linklet inlining could further reduce the
schemify layer, and maybe one day eliminate it.

We've mostly kept BC and CS in sync, so far, but moving forward,
Racket BC will not necessarily get new functionality that is added to
CS. After all, the point of CS is to eventually shed technical debt in
BC. This divergence has already started: Racket CS fully supports M1
Macs, but Racket BC runs only in interpreted mode or as x86_64 (i.e.,
we did not add an AArch64 JIT to Racket BC).

@italic{Thanks to Sam, Ryan, Robby, Matthias, and John for improving
this report. Special thanks to Sam, Paulo, and Robby for moving CS
testing forward. Many contributors helped track down and repair CS
bugs, and besides others mentioned, thanks to Shu-Hung, Sorawee, and
Bogdan for tackling some substantial repairs.}
