#lang scribble/manual

Title: Parallel Threads in Racket v9.0
Date: 2025-11-21T00:00:00
Tags: Threads, Parallelism

@(require (for-label racket/base
                     racket/future
                     racket/place)
          racket/format
          (only-in scribble/core
                   style
                   color-property
                   background-color-property)
          (only-in scribble/html-properties
                   attributes))

@(define (section* #:tag [tag #f]. l)
   @section[#:style (style #f (list 'unnumbered 'no-header-controls)) #:tag tag l])

@(define (code name)
   (~a "https://users.cs.utah.edu/plt/parallel-bm/" name))

@(define (codelink name)
   (list "[" (hyperlink (code name) "code") "]"))

@(define small-tab-style
   (style #f (list (attributes '((style . "font-size: 60%"))))))
@(define smallish-tab-style
   (style #f (list (color-property "purple")
                   (attributes '((style . "font-size: 80%"))))))
@(define (div s)
   (para #:style (style #f (list 'div)) s))
@(define real-msec-color
   (style #f (list (color-property "forestgreen")
                   (attributes '((style . "font-weight: bold"))))))
@(define big-tab-style
   (style #f (list (attributes '((style . "font-size: 140%"))))))

@(define (exec . s)
   (elem #:style (style #f (list (attributes '((style . "font-family: monospace; font-size: 1rem")))))  s))

@(define (bg-color s)
   (background-color-property s))

@(define n-col (list 'left 'top (bg-color "whitesmoke")))
@(define n-col/right (list 'right 'top (bg-color "whitesmoke")))
@(define old-col (list 'right 'top (bg-color "lightblue")))
@(define seq-col (list 'right 'top (bg-color "beige")))
@(define corou-col (list 'right 'top (bg-color "palegoldenrod")))
@(define para-col  (list 'right  'top (bg-color "palegreen")))
@(define fut-col (list 'right 'top (bg-color "bisque")))

@(define (benchmark* labels colors task
                     #:precision [precision 1]
                     #:compact? [compact? #f]
                     #:no-n? [no-n? compact?]
                     . lines)
   (define (result baseline l)
     (define (lbl t l)
       (if compact?
           (list t (div (elem l)))
           (list t)))
     (define lst
       (list (para
              #:style (if compact? big-tab-style (style #f null))
              (hspace 1)
              (elem #:style real-msec-color
                     (if baseline
                         (format "×~a" (~r (/ (cadr baseline) (cadr l))
                                           #:precision precision))
                         "")))
             (div
              (elem #:style smallish-tab-style (format (if compact?
                                                           "~a msec real"
                                                           "~a")
                                                       (cadr l))))
             (if #f
                 ""
                 (tabular #:style small-tab-style
                          #:column-properties (if compact?
                                                  (list 'right 'left)
                                                  (list 'right))
                          (list (lbl (div (elem (format "~a" (car l))))
                                     "\uA0msec CPU")
                                (lbl (div (elem (format "~a" (caddr l))))
                                     "\uA0msec CPU for GC"))))))     
     (if baseline
         lst
         (list (cadr lst) 'cont (caddr lst))))
   (tabular
    #:style (style #f (list (attributes '((style . "margin: 1em")))))
    #:sep (hspace 2)
    #:cell-properties (append
                       (if compact?
                           null
                           (list
                            (append
                             (if no-n?
                                 null
                                 (list n-col n-col))
                             (apply
                              append
                              (for/list ([color (in-list (cdr colors))])
                                (list n-col n-col n-col)))
                             (list n-col/right n-col/right n-col/right
                                   n-col))))
                       (list
                        (append
                         (if no-n?
                             null
                             (list n-col n-col))
                         (apply
                          append
                          (for/list ([color (in-list colors)])
                            (define (c->left s)
                              (if compact?
                                  (cons 'left (remq 'right s))
                                  s))
                            (list (c->left color) (c->left color) (c->left color))))
                         (list n-col))))
    (append
     (if compact?
         null
         (list
          (append (if no-n?
                      null
                      (list "" task))
                  (apply
                   append
                   (for/list ([color (in-list (cdr colors))])
                     (list 'cont 'cont 'cont)))
                  (result #f '("CPU" "real msec" "GC"))
                  (list ""))))
     (list* (append
             (if no-n?
                 null
                 (list "" (bold "N")))
             (apply
              append
              (for/list ([label (in-list labels)])
                (list (bold label) 'cont 'cont)))
             (list ""))
            (for/list ([a (in-list lines)])
              (define baseline (cadr a))
              (append
               (if no-n?
                   null
                   (list "" (bold (format "~a" (car a)))))
               (apply
                append
                (for/list ([b (in-list (cdr a))])
                  (result baseline b)))
               (list "")))))))

@(define (benchmark task . lines)
   (apply benchmark*
          (list "sequential" "coroutine" "parallel" "futures")
          (list seq-col corou-col para-col fut-col)
          task lines))

@(define (versus-benchmark task . lines)
   (apply benchmark*
          #:precision 2
          (list "\uA0v8.18 sequential" "\uA0v9.0 sequential")
          (list old-col seq-col)
          task lines))

@(define (small-benchmark task col1 col2)
   (benchmark* #:compact? #t
               (list "\xA0concurrent" "\xA0parallel")
               (list corou-col para-col)
               task
               (list 1
                     col1
                     col2)))

@italic{posted by Matthew Flatt, Ryan Culpepper, Robby
Findler, Gustavo Massaccesi, and Sam Tobin-Hochstadt}

With the upcoming version 9.0 release, Racket includes support for
shared-memory threads that can take advantage of multicore hardware
and operating-systems threads to run in parallel---not merely
concurrently with other Racket threads, as was the case in versions before 9.0.

Creating a thread that runs in parallel is as simple as adding a
flag to the call to @racket[thread].
To see the effect, try first putting the following code into a file named
@filepath{thread.rkt} and running @exec{racket thread.rkt} on the
command line:

@(racketblock
   @#,(hash-lang) @#,racketmodname[racket/base]

   (define N 22)
 
   (define (go)
     (thread
      (λ ()
        (for ([i (in-range (expt 2 N))])
          (black-box (sqrt i))))))

   (define t1 (go))
   (define t2 (go))
 
   (thread-wait t1)
   (thread-wait t2)
  )

Racket will find many square roots (tweak @racket[N] to match your
machine), but will keep only one core of your CPU busy. Using
@exec{time} in the shell reports ``CPU'' (possibly broken into
``user'' and `system'') and ``real'' times that are similar. To
use two cores, add @racket[#:pool 'own] to the @racket[thread] call:

@(racketblock
   @#,(hash-lang) @#,racketmodname[racket/base]

   (define N 22)
 
   (define (go)
     (thread
      #:pool 'own (code:comment "<-- only change is here")
      (λ ()
        (for ([i (in-range (expt 2 N))])
          (black-box (sqrt i))))))

   (define t1 (go))
   (define t2 (go))
 
   (thread-wait t1)
   (thread-wait t2)
  )

In this case, real time should be about half of CPU time, while CPU
should remain similar to before. In other words, the parallel version
runs twice as fast. On the machine used @seclink["perf"]{below}:
@;
@small-benchmark[@racket[(go) (go)]
  (list 979 1011 10)
  (list 1021 517 13)
  ]
@;

Passing the new @racket[#:pool] argument
creates a @defterm{parallel thread}; create pools via
@racket[make-parallel-thread-pool] to have a group of threads share
processor resources or just pass @racket['own] to have the
new thread exist in its own
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{parallel thread pool}.

As a further addition to @racket[thread], a @racket[#:keep 'result]
argument keeps the result of @racket[_thunk] when it returns, instead
of discarding the result. Retrieve a thread's result with
@racket[thread-wait]. So, for example,

@racketblock[
 (thread-wait (thread _thunk #:pool 'own #:keep 'result))
 ]

runs @racket[_thunk] in parallel
to other Racket threads, blocks the current Racket thread (while
allowing other Racket threads to continue, even non-parallel ones),
and then returns the result value(s) when @racket[_thunk] completes.

To maintain backwards compatibility, the @racket[thread] function still creates a @defterm{coroutine
thread} by default, which is a lightweight thread that is preemptively
scheduled and whose execution is interleaved with other coroutine
threads. For many tasks that need the organizational benefits of
concurrency without the performance benefits of parallelism, such as
when managing GUI interactions or orchestrating remote processes,
coroutine threads are still the best abstraction. Coroutine threads
can use @racket[#:keep 'result], too.

Racket's full thread API works with parallel threads. Follow the links
from the @racket[thread] documentation to see more details on thread
pools and for more interesting uses. Of course, just because you put
tasks in parallel threads doesn't mean that they always speed up,
as sharing and communication can limit parallelism. Racket's
@hyperlink["https://docs.racket-lang.org/guide/parallelism.html#(part._effective-futures)"]{future
visualizer} works for parallel threads, tho, and it can help you
understand where synchronization in a task limits parallelism.
Also, adding parallelism to Racket potentially creates trouble for existing
libraries that were not designed to accommodate parallelism. We expect
problems to be rare, however.

We'll explore the performance details and explain why we expect most
programs will continue to work well later in this post, but first:

@; -----------------------------------------------------------------
@section*{Racket's Road to Parallelism}

Running threads in parallel counts as news in 2025?! Well, it has been a long
road.

Racket's implementation started in the mid-1990s, just as a wave of
enthusiasm for parallel programming was winding down. Although
operating systems by that point consistently supported within-process
threads, computers with multiprocessors were not commonly available.
Many language runtime systems from the same era---including Python,
Ruby, and OCaml---took advantage of the internal simplicity of a
single-threaded runtime system while offering constructs for
@defterm{concurrency} at the language level. Racket has always
included threads for concurrency, and it was an early adopter of
@hyperlink["http://cml.cs.uchicago.edu/"]{Concurrent ML}'s
abstractions for managing concurrency well. But an absence of
parallelism was deeply baked into the original implementation.

Over time, to provide support for parallelism, we added
@defterm{places} and @defterm{futures} to Racket. Places support
coarse-grained parallelism through a message-passing API, effectively
running parallel instances of the virtual machine within a single
operating-system process; limited sharing makes the implementation
easier and safer than arbitrary sharing between parallel threads.
Futures provide fine-grained parallelism for restricted computations;
a future blocks when it tries to perform any operation that would be
difficult for the runtime system to complete safely in parallel.
Places and futures are both useful, and they avoid some pitfalls of
shared-memory threads. Still, fitting a parallel task into futures or
places usually requires special effort.

Meanwhile, single-threaded execution was only one of the problems with
the original Racket (a.k.a. PLT Scheme) implementation. To address
larger problems with the implementation and to improve performance, we
started in 2017
@hyperlink["https://blog.racket-lang.org/2020/02/racket-on-chez-status.html"]{rebuilding
Racket on top of Chez Scheme}. Rebuilding took some time, and we only
gradually deprecated the old ``BC'' implementation in favor of the new
``CS'' implementation, but the transition is now complete. Racket BC
is still maintained, but as of August 2025, we distribute only Racket
CS builds at @url{https://download.racket-lang.org}.

Chez Scheme is a much better foundation for improving parallelism in
Racket. Part of the Racket-rebuilding effort included improving Chez
Scheme's support for parallelism: we added memory fences as needed for
platforms with a weak memory-consistency model, and we parallelized
the Chez Scheme garbage collector so that garbage collection itself
runs in parallel. There's still plenty of room for improvement---the
garbage collector is only parallel with itself, not the main program,
for example---but further improvements are more within reach than
before. Equally important, the rebuild included new implementations of
the Racket thread scheduler and I/O layer in Racket itself (instead of
C). Because of these improvements, Racket's futures worked better for
parallelism from the start in Racket CS than in Racket BC.

With version 9.0, we finally take advantage of new opportunities for
parallelism created by the move to Racket CS. Internally, a parallel
thread is backed by combination of a future and a coroutine thread.
The main extra work was making Racket's coroutine thread scheduler
cooperate more with the future scheduler and making the I/O layer safe
for Chez Scheme threads---all while making locks fine-grained enough
to enable parallelism, and keeping the cost of needed synchronization
as low as possible, including for non-parallel Racket programs.

@; -----------------------------------------------------------------
@section*[#:tag "perf"]{Performance}

Here are some simple benchmarks on an M2 Mac to give a sense
of the state of the current implementation. This machine has 8
cores, but 4 big and 4 little, so ×4 speedup is possible with
4-way parallelism but less than ×8 with 8-way parallelism.

As an easy first example, we should expect that a Fibonacci @codelink["fib.rkt"] run of
1 iteration in each of 4 coroutine threads takes the same time as
running it 4 iterations in 1 thread, while 1 iteration in each of 4
parallel threads should take about 1/4th of the time. Also, for such a
simple function, using plain old futures should work just as well as
parallel threads. That's what we see in the numbers below.

@italic{Times are shown as a speedup over single-threaded, then in
real elapsed milliseconds, with CPU milliseconds as the upper smaller
number to the right, and CPU milliseconds that are specifically for GC
as the lower smaller number to the right. The times are from a single
run of the benchmark.}

@benchmark[@racket[(fib 40)]
           (list 1
                (list 493 511 0)
                (list 490 506 0)
                (list 494 494 0)
                (list 495 495 0))
           (list 4
                 (list 1978 2045 0)
                 (list 1967 2034 0)
                 (list 2210 554 0)
                 (list 2168 545 3))
           (list 8
                 (list 4021 4154 0)
                 (list 4021 4154 2)
                 (list 5928 776 1)
                 (list 6006 796 2))]

Of course, most programs are not just simple arithmetic. If we
change our example to repeatedly convert numbers back and forth
to strings as we compute Fibonacci @codelink["strfib.rkt"], then
we can see the effects of the more complex conversions. This version
also triggers frequent allocation, which lets us see
how thread-local allocation and parallel garbage collection scale.

@benchmark[@racket[(strfib* 32)]
           (list 1
                 (list 197 204 3)
                 (list 198 205 0)
                 (list 192 192 0)
                 (list 203 211 0))
           (list 4
                 (list 796 826 10)
                 (list 780 808 2)
                 (list 861 222 10)
                 (list 857 221 10))
           (list 8
                 (list 1563 1619 11)
                 (list 1545 1602 4)
                 (list 2544 419 59)
                 (list 2551 406 59))]

From this table, we still see reasonable scaling up to four cores,
but the additional work and the use of the garbage collector limit
scaling beyond that point.

That first string variant of Fibonacci includes a slight cheat,
however: it goes out of its way to use a @racket[string->number*]
wrapper that carefully calls @racket[string->number] in a way that
avoids evaluating expressions that compute the default values of some
arguments. The defaults consult the
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{parameters}
@racket[read-decimal-as-inexact] and
@racket[read-single-flonum]---which a perfectly fine thing to do in
general, but turns out to block a future, because parameter values
can depend on the current continuation. In contrast, parallel threads
continue to provide a benefit when those kinds of Racket constructs
are used. We can see the difference by using plain
@racket[string->number] in place of @racket[string->number*], which
will fetch parameter values 14 million times in each individual run of
@racket[(strfib 32)]:

@benchmark[@racket[(strfib 32)]
           (list 1
                 (list 751 772 4)
                 (list 562 578 1)
                 (list 721 721 1)
                 (list 851 873 0))
           (list 4
                 (list 3085 3169 12)
                 (list 2303 2364 6)
                 (list 3103 797 33)
                 (list 4058 4164 2))
           (list 8
                 (list 6225 6409 14)
                 (list 4608 4730 13)
                 (list 9166 1493 197)
                 (list 8135 8353 4))]

The coroutine column here also shows an improvement, surprisingly.
That's because a coroutine thread has a smaller continuation than the one in
the sequential column, and the cost of fetching a parameter
value can depend (to a limited degree) on continuation size.
The effect of parallel threads on this kind of program is
more consistent than fine details of a continuation's shape.

Operations on mutable @racket[equal?]-based
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{hash tables}
@codelink["hash-nums.rkt"] are another case where
futures block, but parallel threads can provide performance
improvement.

@benchmark[@racket[(hash-nums 6)]
           (list 1
(list 184 193 1)
(list 182 190 0)
(list 186 186 0)
(list 182 191 0))
           (list 4
(list 733 767 5)
(list 729 763 0)
(list 824 208 3)
(list 729 763 0))
           (list 8
(list 1473 1541 11)
(list 1463 1532 1)
(list 2373 346 24)
(list 1470 1539 1))]

As an illustration of the current limitations of parallel threads in
Racket, let's try a program that writes data to a byte-string
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{port} then hashes it
@codelink["hash-digits.rkt"].

@benchmark[@racket[(hash-digs 7)]
           (list 1
                 (list 123 127 3)
                 (list 123 127 4)
                 (list 135 135 3)
                 (list 122 126 3))
           (list 4
                 (list 487 503 13)
                 (list 519 536 35)
                 (list 599 201 38)
                 (list 504 520 28))
           (list 8
                 (list 986 1022 33)
                 (list 1057 1097 81)
                 (list 1630 403 85)
                 (list 1017 1049 60))]

Here we see that parallel threads do get some speedup, but
they do not scale especially well. The fact that separate
ports are not contended enables performance
improvement from parallelism, but speedup is limited by some
general locks in the I/O layer.

Even further in that direction, let's try a program that
hashes all files in the current directory @codelink["hash-dir.rkt"] and
computes a combined hash. When run on the @filepath{src} directory of
the Racket Git repository, most of the time is reading bytes from
files, and locks related to file I/O are currently too coarse-grained
to permit much speed-up.

@benchmark[@racket[(hash-dir)]
           (list 1
                 (list 169 170 1)
                 (list 169 169 0)
                 (list 248 256 0)
                 (list 170 170 0))
           (list 4
                 (list 690 692 3)
                 (list 658 662 3)
                 (list 1068 515 5)
                 (list 679 681 3))
           (list 8
                 (list 1377 1393 10)
                 (list 1290 1293 6)
                 (list 2158 868 18)
                 (list 1366 1368 7))]

Having locks in place for parallel threads can impose a cost on
sequential programs, since locks generally have to be taken whether or
not any parallel threads are active. Different data structures in
Racket use specialized locks to minimize the cost, and most benchmarks
reported here run report the same numbers in sequential column in Racket v8.18 (the
previous release) and Racket v9.0. The exceptions are the
@racket[(hash-nums 6)] and @racket[(hash-digs 7)] benchmarks, because
those measure very-fine grained actions on mutable hash tables and I/O
ports, and the cost is largest for those. Comparing sequential times
for those two versions shows that support for parallel thread can cost
up to 6-8% for programs that do not use them, although the cost tends
to be much less for most programs.

@versus-benchmark[@racket[(hash-nums 6)]
           (list 1
                 ;; old
                 (list 180 188 0)   ; seq
                 #;(list 181 188 0) ; corou
                 #;(list 180 188 0) ; fut
                 ;; new
                 (list 187 195 0)
                 #;(list 183 191 0)
                 #;(list 183 191 0))
           (list 4
                 (list 728 757 2)
                 #;(list 743 774 0)
                 #;(list 723 752 0)
                 (list 739 773 3)
                 #;(list 736 770 0)
                 #;(list 732 767 0))
           (list 8
                 (list 1461 1520 9)
                 #;(list 1458 1517 1)
                 #;(list 1446 1505 1)
                 (list 1477 1546 12)
                 #;(list 1474 1542 1)
                 #;(list 1492 1562 1))]


@versus-benchmark[@racket[(hash-digs 7)]
           (list 1
                 (list 114 118 3)
                 #;(list 115 119 3)
                 #;(list 114 118 3)
                 (list 122 126 3)
                 #;(list 121 125 3)
                 #;(list 121 125 3))
           (list 4
                 (list 458 474 12)
                 #;(list 488 504 33)
                 #;(list 469 485 28)
                 (list 489 506 13)
                 #;(list 511 527 35)
                 #;(list 499 515 29))
           (list 8
                 (list 915 947 25)
                 #;(list 998 1030 93)
                 #;(list 944 975 57)
                 (list 989 1025 26)
                 #;(list 1046 1088 75)
                 #;(list 1016 1051 61))]

Overall, parallelizable numerical programs or ones that manipulate
unshared data structures can achieve speedup through parallel threads
relatively easily, but I/O remains a direction for improvement.

@; -----------------------------------------------------------------
@section*[#:tag "compat"]{Backward Compatibility}

If a library uses mutable variables or objects, either publicly or
internally, then it must use locks or some other form of concurrency
control to work properly in a multithreaded context. Racket already
has concurrency, and the expectation for libraries to work with
threads does not change with the introduction of parallel threads.
Racket's semaphores, channels, and other synchronization constructs
work the same with parallel threads as concurrent threads. Even
programs that use lock-free approaches based on compare-and-swap
operation (such as @racket[box-cas!]) continue to work, since Racket's
compare-and-swap operations use processor-level primitives.

Still, there are a few concerns:

@itemlist[

 @item{Racket's coroutine threads offer the guarantee of
@defterm{sequential consistency}, which means that effects in one
thread cannot be seen out-of-order in another thread. Parallel threads
in Racket
@hyperlink["https://docs.racket-lang.org/reference/memory-order.html#(part._memory-order)"]{expose
the underlying machine's memory-consistency model}, which may allow
reordering of memory effects as observed by other threads. In general,
a weak memory model can be an issue for code not intended for use with
threads, but Racket---more precisely, Chez Scheme---always guarantees
the memory safety of such code using memory fences. That is, Racket
code might observe out-of-order writes, but it never observes
ill-formed Racket objects. The fences are not new, and they are part of
the same write barrier that already supports generational garbage
collection and the memory safety of futures. Although
sequential consistency supports lock implementations that don't work
with weaker memory models, so they would work with coroutine threads
and not parallel threads, we have not found any such
implementations in Racket libraries.}

@item{Some Racket libraries use @defterm{atomic mode} for concurrency
control. Atomic mode in Racket prevent coroutine thread swaps, and
entering atomic mode is a relatively cheap operation within Racket's
coroutine scheduler. When a parallel thread enters atomic mode, then
it prevents other coroutine threads from running, but it does
@italic{not} prevent other parallel threads from running. As long as
atomic mode is used consistently to guard a shared resource, then it
continues to serve that role with parallel threads.

Entering atomic mode is a much more expensive operation in a parallel
thread than in a coroutine thread; in many cases, Racket core
libraries that need finer-grained locking more specifically need to
move away from using atomic mode. Still, making atomic mode
synchronize a parallel thread with coroutine thread provides a
graceful fallback and evolution path.}

@item{Foreign functions that are called by Racket in a coroutine
threads are effectively atomic operations when there are no parallel
threads, since a coroutine swap cannot take place during the foreign
call. It's rare that this atomicity implies any kind of lock at the
Racket level, however, and the foreign function itself is either
adapted to operating-system threads or not. Racket can already create
operating systems threads through @racket[dynamic-place], and
foreign-function bindings have generally been adapted already to that
possibility.}

]

The greater degree of concurrency enabled by parallelism exposed some
bugs in our existing core libraries that could have been triggered
with coroutine threads, but hadn't been triggered reliably enough to
detect and repair the bugs before. Beyond those general improvements,
our experience with pre-release Racket is that parallel threads have
not created backward-compatibility problems.
