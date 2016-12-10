
    Title:DAGs vs Trees
    Date:2010-02-28T20:16:00.006-05:00
    Tags:

*posted by Robby Findler*


As I wondering whether or not there is a better layout algorithm for the module browser window, I looked into [tree maps](http://www.cs.umd.edu/hcil/treemap-history/). Of course, the modules in a program form a DAG, not a tree, so I wondered just how big the tree would get if all of the shared structure in the DAG were replicated. Hey, I figured, if a tree map can handle showing me my entire filesystem, maybe that could work.



... yeah, no. Turns out to be hopeless. In the spirit of a geeky take off on a jelly bean counting contest, lets see if you can guess just how big these things get. Consider the module graph from the program `#lang scheme` (ie, the graph that just contains an empty program). This program loads 170 modules with 917 connections between modules (counting the main file that just contains the `#lang scheme`).



So, the question: how many nodes are there in the unsharified tree? First one to come within 1 billion of the right answer gets all of the fame and glory that this blog brings to bear (har har). I'll post the answer in the comments in a few days (and no fair cheating, those of you that know enough to be able to get your hands on the DAG).


<!-- more -->



* * *

Okay, I'll give it a go on the scale you indicate: ... 282 billion? (9^12)

— *Kohath, 1 March 2010*

* * *

Sadly, not close. Lets make this easier. If you're within a trillion of the correct answer, you win.

— *Robby, 2 March 2010*

* * *

There are 102,295,446,100,979 nodes. Which is a lot. If we somehow used only a single pixel per node (at the standard 72dpi) that would require a monitor that was more that 2 miles square to display the entire thing.

— *Robby, 3 March 2010*

* * *

