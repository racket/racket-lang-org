
    Title:Simple GnuPLoTting
    Date:2008-11-24T17:19:00.019-05:00
    Tags:

*posted by vyzo*

[gnuplot](http://www.gnuplot.info/) is a very powerful and widely used interactive data plotting  program. It can generate two-dimenational and three-dimensional surface plots, either on screen  or print to files with a wide-array of supported formats. If you are not familiar with it, you can get an idea of its capabilities [here](http://gnuplot.sourceforge.net/demo/) 

A new package, [gnuplot.plt](http://planet.plt-scheme.org/display.ss?package=gnuplot.plt&amp;owner=vyzo), is now available on planet which allows you to programmatically interact with gnuplot processes and generate plots on the fly. The package provides a simple interface that abstracts gnuplot's quirky syntax and takes care of data marshalling using temporary files.

Without further ado, here is to every computer scientist's favorite growth curves:

```racket
(require (planet vyzo/gnuplot))
(define gplot (gnuplot-spawn))
(define data 
  (gnuplot-data
   (build-list 90
     (lambda (x) 
       (let ((x (add1 (/ x 10.))))
         (list x (log x) (* x (log x)) (expt x 2) (expt 2 x)))))))
;; png output options
(define png '(png enhanced transparent font (str arial)))
;; on screen plot
(gnuplot-set gplot '(title (str "growth curves")))
(gnuplot-plot gplot 
  #:range '(() (1 1000))
  (gnuplot-item data '(using (seq: 1 1) title (str "x") with line))
  (gnuplot-item data '(using (seq: 1 2) title (str "log(x)") with line))
  (gnuplot-item data '(using (seq: 1 3) title (str "xlog(x)") with line))
  (gnuplot-item data '(using (seq: 1 4) title (str "x^2") with line))
  (gnuplot-item data '(using (seq: 1 5) title (str "2^x") with line)))
;; replot to png
(gnuplot-hardcopy gplot "/tmp/grow.png" #:term png)
;; redo with logscale
(gnuplot-set gplot '(logscale y))
(gnuplot-replot gplot)
(gnuplot-hardcopy gplot "/tmp/loggrow.png" #:term png)
```










<!-- more -->



* * *

Hi, perhaps this is a really stupid question, but which language was your code snippet written in?  If I try and run it with DrScheme in ASL it throws an error on the require function.
I can get it to execute with Essentials of Programming Languages 3rd language, but then I had to define add1 and build-list.  After that, it still errors on the gnuploy-hardcopy, which I presume means this is the wrong language.

— *Daniel, 23 December 2008*

* * *

Hi Daniel,

Add the line

    #lang scheme

and run it in the "module" language.

Merry Christmas,
Jens Axel

— *Jens Axel Søgaard, 24 December 2008*

* * *

Thanks!  I was about to code an interface to gnuplot, and I see you've already done it.  I owe you a beer (at least).

— *Geoff Knauth, 27 October 2011*

* * *

