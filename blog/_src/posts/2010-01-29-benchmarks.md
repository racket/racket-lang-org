
    Title:Benchmarks
    Date:2010-01-29T12:57:00.005-05:00
    Tags:

*posted by Matthew Flatt*

First, the usual disclaimer:


That said, I've run the latest version of PLT Scheme on two sets of benchmarks:

* [Benchmarks in the PLT sources](http://www.cs.utah.edu/%7Emflatt/benchmarks-20100126/log3/Benchmarks.html) – vs. Bigloo, Chicken, Gambit, Guile, Ikarus, Larceny, MIT Scheme, and Scheme48; safe operations and generic arithmetic only

* [Benchmarks in the Gambit sources](http://www.cs.utah.edu/%7Emflatt/benchmarks-20100126/log1/Gambit_20benchmarks.html) – vs. Bigloo and Gambit; generic vs. fixnum-/flonum-specific arithmetic, safe vs. unsafe operationsThe second set is why I started running benchmarks. Fixnum-/flonum-specific arithmetic and unsafe operations are new in PLT Scheme 4.2.4. The benchmark results suggest that the new operations in PLT Scheme offer roughly the same performance benefits as in Bigloo and Gambit. There's room for improvement, but it's a good first cut.


For the other results: PLT Scheme is rarely the fastest implementation on a given benchmark. For most purposes, though, it's in the same ballpark – except for programs that spend all their time capturing and invoking continuations.


It's fun to run benchmarks occasionally. Now, back to working on language design, libraries, documentation, usability...

<!-- more -->



* * *

Could it be possible to add Guile 1.9.7 to that list?

— *dales@vxitech.com, 29 January 2010*

* * *

Although I don't want to stay in the benchmark-running business, Guile 1.9.7 does seem to be much faster than 1.8.7. So, I've replaced 1.8.7 with 1.9.7 in the benchmark results.

— *Matthew Flatt, 29 January 2010*

* * *

Thanks for adding Guile 1.9.7. I wonder though, did you include the runtime for the first Guile run? Guile will compile files the first time they're run, and then used the cached object file later.

I ask because my old slow laptop is getting better times than the ones you list. Perhaps you inadvertantly included that first compilation time.

In any case, congrats on those great PLT results! :)

Happy hacking,

Andy

— *Andy Wingo, 30 January 2010*

* * *

I don't think that compilation time is included, since run time is measured within the program using `(times)'. Also, clearing out the cache before running doesn't seem to give different results than running a second time.

— *Matthew Flatt, 30 January 2010*

* * *

Good point regarding the use of `time'. Ah well, a Lisp is never finished, is it. Thanks for the response, and happy hacking :)

— *Andy Wingo, 30 January 2010*

* * *

