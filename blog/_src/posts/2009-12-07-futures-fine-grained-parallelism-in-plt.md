
    Title:Futures: Fine Grained Parallelism in PLT
    Date:2009-12-07T13:18:00.002-05:00
    Tags:

*posted by Robby Findler*


We're pleased to announce the initial release of parallel futures, a
construct for fine-grained parallelism in PLT. Roughly speaking, a
programmer passes a thunk to 'future' and it gets run in parallel.
That "roughly" holds a few gotchas, partly because we're just getting
started and partly due to the technique we're using. See the
documentation for more details:



[http://pre.plt-scheme.org/docs/html/futures/](http://pre.plt-scheme.org/docs/html/futures/)



If you've got a multicore machine where you can't keep the cores busy
or your office/machine room is a bit cold, try this program:

```racket
#lang scheme
(require scheme/future)
(define (loop) (loop))
(for-each
 touch
 (for/list ([i (in-range 0 (processor-count))])
  (future loop)))
```

Note that you have to build mzscheme with futures; it isn't enabled by
default, but see the docs above for how to do that. Beyond the above,
we've also gotten a few parallel kernels going and are seeing good
scalability up to 8 cores (the biggest machine we have around for the
time being).


<!-- more -->



* * *

Why is the touch explicit? 

Why don't you use future-strict primitives to force future values?

— *matthias, 7 December 2009*

* * *

One could build another layer on top of this one to do things like that (and perhaps we should not have taken over the names 'future' and 'touch' and let a library like that have them).

touch is explicit so that the runtime system primitives that depend on a particular context are well-defined (eg, exception handlers and continuation marks).

— *Robby, 7 December 2009*

* * *

Will never versions have it enabled by default for the OSX package?

— *Robert, 7 December 2009*

* * *

Eventually, yes. But it is easy to build under mac os x. Just make sure you have the developer tool installed and then it is only a few simple commands in a Terminal window.

— *Robby, 7 December 2009*

* * *

Does that "mess up" anything with the packaged installation?

— *Robert, 7 December 2009*

* * *

No. You can have as many versions as you want, completely separate. (Double clicking in the finder will always choose one of the open drschemes; I haven't figured out what the OS does when none of them are open.)

— *Robby, 7 December 2009*

* * *

It seems to me that delay/thread in scheme/promise has an analogous existing interface to a later forced-touched promise-future.  Outside the very important and appreciated implementation difference of having multiple CPUs, where does the analogy fall short enough to require a different set of functions?

— *griffinish, 20 December 2009*

* * *

Well, parallelism is an important difference, of course (enough that warrants its own, separate api so programmers know what they are getting), but another difference is that with promises the computation won't get "frozen" in the way that futures can get frozen.

— *Robby, 20 December 2009*

* * *

