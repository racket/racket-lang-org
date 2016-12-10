
    Title:DrRacket, now more responsive
    Date:2012-11-15T00:26:00.000-05:00
    Tags:

*posted by Robby Findler*

DrRacket is now more responsive when editing
than the 5.3.1 release. How much more?
Well, I ran a script that starts up DrRacket,
opens class-internal.rkt from the distribution
and puts the insertion point right before the first
" character. It then repeats these three steps 10 times:
first it types fdjafjdklafjkdalsfjdaklfjdkaslfdjafjdklafjkdalsfjdaklfjdkasl
as fast as it can, then it types the same number of backspaces. Next it type "a and
waits for the syntax colorer to finish adjusting the colors. Then it deletes
those two (again with two backspaces) finally waits for background check syntax to complete.

The script also measures the number of wall-clock milliseconds that the handling
of each event took and here are the results:







Each vertical bar’s hight is proportional to the percentage of the events
that took at least the corresponding number of milliseconds. The red bars
show how well version 5.3.1’s DrRacket does, and the blue shows how the
current git head fares (as of [http://git.racket-lang.org/plt/commit/a4d440a5](http://git.racket-lang.org/plt/commit/a4d440a5)).



As you can see, about 80% of the events took less than 26 milliseconds to complete
in the current version, but more like 60 milliseconds in 5.3.1. As some sense
of scale, a television refreshes its screen every 16 2/3s millseconds, so if
all of the events took less than that then DrRacket would feel very
responsive indeed.


How?: 
The key to finding all of the performance improvements was
finding something to measure. It sounds simple (and indeed,
it didn’t take long to do), but
once I had that, it because relatively easy to find suspiciously
slow events, to sort out what they were doing and to speed
them up. (Thanks to
[Matthew](http://www.cs.utah.edu/~mflatt/)
for this excellent advice!)



Specifically, I added a bunch of logging to various
parts of racket/gui, framework,
and DrRacket itself. For example, the graphs above are generated from logging
information about how long events take to be handled.



Some of the problems were stupid things, e.g.,
[there was one place](http://git.racket-lang.org/plt/commit/4421e227ffa)
where DrRacket was overriding a callback that happened on each keystroke
that would invalidate the entire visible region of the editor. This
forced the entire buffer to be redrawn on each keystroke, making
DrRacket’s sluggishness proportional to the size of the definitions
window(!).



The performance graph above was recorded a smallish window, namely maximzed
on my laptop: 1365x740. A bigger window doesn’t change the blue bars, but here’s
how a 1102x1174 changes the red ones:








There were two more complex fixes. First: background check syntax.
It runs mostly in a separate, parallel place and thus
(on a multi-core machine) doesn’t interfere with DrRacket’s editing all. The last phase,
however, is to communicate the results of check syntax back and that has
to update state on the GUI and thus competes with the handling of callbacks.
This communication breaks up the check syntax information into chunks
and installs that information one chunk at a time, so as to avoid
tying up the event handling thread for too long.
Thanks to some logging, I found that some of the chunks were too large
and was able to split them up into smaller chunks.



The most complex change was in the syntax colorer. It used to use a co-routine
that would parse part of the buffer, suspend the co-routine, color the part it parsed,
and then yield control back to handle other events.
Unfortunately, the coroutine would
commonly run for 10 or 15 milliseconds, but then build up 30 or 40 milliseconds worth
of work to do to color the buffer. The fix to the colorer was to eliminate the co-routine
and interleave the coloring and the parsing, meaning the whole process now has
finer granularity, and thus is able to be interrupted more frequently and
more accurately.


Not done yet: 
There is still a lot more to do. Editing scribble files
is less responsive and the contour window definitely still makes
DrRacket sluggish. Yesterday I was able to get DrRacket
in a state that brought back some sluggishness and I don’t know how
I did that (restarting DrRacket got rid of the problem, unfortunately).
I also think I need to look more closely and what happens when
purple search bubbles are visible. An interactive GC would probably
also help.



If you spot some way to make DrRacket feel more sluggish than it should be,
please let me know!

<!-- more -->



* * *

Excellent work!

— *grant rettke, 15 November 2012*

* * *

the attention to details you guys have is incredible. thanks for the hard work.

— *Jay Kominek, 15 November 2012*

* * *

As a user of DrRacket @ 2560x1600, this is amazing news!

— *Nick Sivo, 15 November 2012*

* * *

