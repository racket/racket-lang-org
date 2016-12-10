
    Title:Rebuilding Racket’s Graphics Layer
    Date:2010-12-08T10:59:00.005-05:00
    Tags:

*posted by Matthew Flatt*

Racket version 5.1, which is scheduled for release in early February, will look a little different on the outside. Unix/X users will see the biggest difference: DrRacket and all Racket GUI programs will take on the desktop theme for menus, buttons, and other GUI widgets. Text handling is also better than before on Unix/X, especially when printing. Windows and Mac OS X users will see smaller changes, such as better printing, better handling of mouse-wheel events, and support for 64-bit Windows and Mac OS X.


On the inside, version 5.1 is the biggest single change in Racket (or PLT Scheme) history. We’ve reimplemented the GUI layer, which meant throwing out about 200,000 lines of C++ code that built on Xt, Win32, and Carbon. We’ve replaced that C++ code with about 30,000 lines of Racket code that builds on Gtk, Win32, Cocoa, Cairo, and Pango. This change modernizes Racket's graphics support while significantly reducing the cost of maintaining the GUI and drawing libraries.



In the space between the GUI implementation and the surface, there are many API improvements:


*  You can run GUI programs with just `racket`, instead of having to use `gracket`. Depending on how much your platform distinguishes between GUI and console applications, there may still be an advantage to using `gracket` (i.e., to tell the OS that you mean to start a GUI application or that you want a single instance of the application), but the difference is minor.

*   Most of the drawing library has moved to `racket/draw`, which you can use without the rest of the GUI library – and, in the case of Unix platforms, without an X-server connection. After detangling the graphics and GUIs libraries, the graphics library is now integrated in more places, such as adding pict support for Scribble documents. 

*  The drawing library includes some new capabilities, such as rotation, affine transformations, and bitmaps with alpha channels.

Replacing hundreds of thousands of lines of C++ code with tens of thousands of lines of Racket code sounds like a no-brainer. The old library was implemented in C++ because we started in 1995 by gluing together a Scheme interpreter with a portable GUI library. Then the GUI code stayed in C++, because the interpreter wasn’t fast enough and the foreign interface was too clumsy. Racket is now plenty fast and its foreign interface has improved a lot since then.

Still, the reimplementation took about 18 months. Smoothly integrating cross-platform GUI support with a programming language can be more difficult than it sounds, and mating new libraries with a legacy API creates additional challenges. Finally, many Racket tools depend Racket’s “eventspaces,” which are multiple process-like entities in the same virtual machine, each with its own GUI event loop. Implementing eventspaces on top of modern GUI toolkits turns out to be tricky, because the toolkits insist on a single event-loop per process and they cannot tolerate event-loop actions during certain callbacks. Fortunately, delimited continuations can help work around those limitations.


Cairo and Pango are the two big enablers of the Racket graphics rewrite. The old Racket graphics library depended on many toolkits (X11, Win32, QuickDraw, Quartz, PostScript, and more), and it had poor font handling. Again, the problem was that we chose the previous technology in 1995. Cairo and Pango have since solved the portable-graphics problem, and we were able to trade in 80,000 lines of C++ glue for about 8,000 lines of Racket glue. The code could be much less if we didn’t have to match most of the old drawing API, but we're still very happy with the result.


On the GUI side, the remaining 22,000 lines of Racket code replace similar C++ code that binds to three different toolkits. The set of underlying toolkits has changed, and a few eventspace tricks are new, but the approach is essentially the same as before. The code is nevertheless much more compact, because (no surprise) Racket is better than C++. Interestingly, the amount of toolkit-specific code is right around 6,500 lines for each toolkit, even though the way that a C programmer uses the different toolkits seems very different: Objective-C classes (Cocoa) versus signal callbacks with explicit wiring (Gtk) versus a single callback function for message handling (Win32). Maybe they're the same because we built a Racket mini-language for each toolkit that makes them all about equally convenient.


The rewrite is not perfectly compatible with old code, and no doubt we have many bugs to find before the release. The process is well on track, though, and the new library implementations give a us a solid foundation to keep making Racket better.



To try out the current development version, visit

[http://pre.racket-lang.org/installers
](http://pre.racket-lang.org/installers)

<!-- more -->



* * *

This is fantastic!  Good show!

— *steck, 8 December 2010*

* * *

There is a problem on Windows Vista Professional x64:

ffi-obj: couldn't get "GetWindowLongPtrW" from "user32.dll" (The specified proce
dure could not be found.; errno=127)

 === context ===
D:\p\racket\collects\ffi\unsafe.rkt:176:2: get-ffi-obj*
D:\p\racket\collects\mred\private\wx\win32\utils.rkt: [running body]
D:\p\racket\collects\mred\private\wx\win32\sound.rkt: [traversing imports]
D:\p\racket\collects\mred\private\wx\win32\procs.rkt: [traversing imports]
D:\p\racket\collects\mred\private\wx\common\cursor.rkt: [traversing imports]
D:\p\racket\collects\mred\private\kernel.rkt: [traversing imports]
D:\p\racket\collects\mred\private\check.rkt: [traversing imports]
D:\p\racket\collects\mred\mred.rkt: [traversing imports]
D:\p\racket\collects\mred\main.rkt: [traversing imports]
D:\p\racket\collects\racket\gui\base.rkt: [traversing imports]
D:\p\racket\collects\drracket\drracket.rkt: [traversing imports]


[Exited. Close box or Ctrl-C closes the console.]

— *malkia, 8 December 2010*

* * *

malkia --- thanks for the report. I really should have checked whether our latest build was in good shape before posting. It turns out that this bug has been fixed, but machine-configuration issues have prevented a new build from completing since Sunday. We'll get a working Windows build up soon.

— *Matthew Flatt, 8 December 2010*

* * *

How did "delimited continuations [] help work around those limitations"?

— *grant rettke, 8 December 2010*

* * *

When you said that event loop were insisting on there only being one per process, were you thinking of GTK or only Mac/Win? AFAIK you can have multiple glib event loops without issue. You typically have one per thread, but that's not the same thing as one per process.

— *joseph, 9 December 2010*

* * *

Delimited continuations help when a particular callback must be executed before further events are handled. For example, while you drag a scroll thumb on some platforms, the toolbox is in charge of the event loop; if handling a scroll-changed event calls code that takes forever, then the whole GUI system becomes stuck. Delimited continuations let Racket call arbitrary code to handle the scroll event, and if it doesn't complete fast enough, a continuation is captured and resumed sometime later, allowing the GUI to partially respond meanwhile. This use of delimited continuations is similar to a use to implement threads, but without facets of threads (such as thread-specific data) that would get in the way.

— *Matthew Flatt, 9 December 2010*

* * *

On one event loop per process: I was thinking of Cocoa and Gtk. Maybe I'm wrong about Gtk. I know that glib supports multiple event loops, but my impression was that the generality isn't preserved through the Gtk layer. I couldn't get multiple event loops to work in Gtk, and I got the impression from the docs and web searches that it couldn't work, but I'd welcome a pointer to a little example showing a program with multiple Gtk event loops.

— *Matthew Flatt, 9 December 2010*

* * *

Great! Is there any plan to include/use the gtk tree store or add  multiple columns to the actual list-box?

I was planning to create a simple gui with the typical database access showing the contents in a table grid.

— *aleix, 10 December 2010*

* * *

What is the best book to learn "Racket" and Scheme with? Doese the HtDP still hold?

— *Robert, 10 December 2010*

* * *

Yes, there are plans to enable multi-column list boxes and other new things. I'm not sure how soon those will be available, but probably soon.

— *Matthew Flatt, 10 December 2010*

* * *

To get started with Racket, see http://docs.racket-lang.org/getting-started/ . HtDP is still a good starting point.

— *Matthew Flatt, 10 December 2010*

* * *

For anyone that tried the Windows build and ran into problems -- there is now a new build that should work: http://pre.racket-lang.org/installers

— *Eli Barzilay, 10 December 2010*

* * *

Great! Thanks for all the hard work.

— *Sunny, 21 December 2010*

* * *

