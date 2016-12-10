
    Title:King of the Hill on Rosetta Code
    Date:2015-05-03T08:30:00.001-04:00
    Tags:

*posted by Tim Brown*

Racket is _"King of the Hill"_ on Rosetta Code: **This announcement is a follow up to "800!".**

In it I said we'd _"[S]ee you at 1000!"_; but you'll understand why we stopped at this milestone.

Please read that article if you need an introduction to Rosetta Code, and the efforts being made to implement Racket tasks there, and more detail on how you can help. It is more instructive and less braggart than this post.

On Rosetta Code (RC), early in the morning on April 29th, Racket drew level with Tcl in the number of tasks that had been implemented for it. Shortly after that we could announce that:

**Racket has the Most Tasks Implemented in Any Language on Rosetta Code!**

Before I go into too much detail, it must be said that this is another amazing achievement. I, and I'm sure the rest of the Racket community, want to thank and congratulate everyone who has contributed to this effort.

How Did This Happen?
---

 On the front page of RC's site, it states its goal as:

 > ... to present solutions to the same task in as many different languages as possible, to demonstrate how languages are similar and different, and to aid a person with a grounding in one approach to a problem in learning another.

 As well as achieving these comparative goals, implementing tasks also provides a useful library of tools, applications and examples for Racket users themselves. Therefore, doing so is a laudable activity in its own right. The persistent effort and progress have been made by Racketeers on RC, both before and since the "800!" tasks post has been (mostly) performed in that spirit. And that should be plenty enough incentive for _you_ to do so, too.

 But I admit, there is a competitive element that creeps in (affecting some more than others). After having passed the 800 task mark after spending so much time in second place to get past the current leader, Tcl to stay [ahead of Python] [1] these, too, provide plenty of motivation to implement tasks. And if winning isn't important, why, then, do we keep score? 

 And in _that_ spirit, early in the morning on April 29th, I was busily [cherry-picking] [2] tasks on Rosetta Code to help close the gap with Tcl; when I thought I would take a quick check on Tcl's and Racket's [task counts][3]. From what I could see, both had a task count of 845! Racket had drawn level with, Tcl as the _Joint Most Popular Programming Language_ on RC.

 I got the independent verification of this from the #racket [IRC Channel][4]. It was true! But Racket was _only joint first._ This point was not lost on the denizens of IRC (`zedoary` being one); who posted two more tasks in very quick succession, bringing Racket up to 847 -- two clear of the previous leader!

 How does this Help Racket?
 ---

  **Plenty of Examples**
  Look back at the intentions of Rosetta Code itself. It is expected that users of other languages can come and compare what they know with what Racket provides. Strictly speaking, of course, in a lot of cases they won't be able to compare since the other language won't be represented whereas Racket will.

  There is also, now, a large collection of Racket examples, which Racketeers themselves can use to improve their understanding of Racket. Strangely, this is not actually one of the stated objectives of RC; it is a welcome side-effect of the work.

  **A Tool for Advocacy**
  Advocates of Racket can use this position on Rosetta Code to show that Racket is as, if not more, capable than any language. Especially for general purpose computing.

  "Racket is Number One on Rosetta Code" isn't a bad place to start with, I guess.

  Additionally, I would like to point out that whatever any of the other languages (or tasks) seem to throw at it, there is something in Racket that allows it to take it in its stride. Sometimes the implementations have had high [line counts][5]; but they rarely, if ever, seem contrived.

  If you need to provide reasons for tasks not being implemented in Racket, here are a few you can use:

* _Nobody has implemented them "yet":_ let it be known that we've done the best  part of 850 tasks, and there are only so many hours in the day.

* _Someone has written an FFI for Tcl to an obscure library:_ The task for Tcl  has then simply been to load the FFI. The task for Racket is either to a) implement the library, which is much more effort than Tcl put in or b) to produce FFI bindings itself, which after the first time doesn't bring much to the party. The same holds true for tasks written _for_ languages which are basically DSLs, showing off how they work in domain for which they are specific.

* _The task is written and documented entirely in Russian:_ This makes  translating it an "exercise."

Is it Time to Rest on our Laurels?
---

 That was a rhetorical question.

Please ignore it.

There are many reasons to continue to work on Rosetta Code.

We Haven't Finished
---

**Implement Some Outstanding Tasks!**

There are 922 tasks on Rosetta Code. 849 are implemented in Racket (more have been added as we speak)! Even excluding the impossible and Russian tasks, that's still many more tasks to implement.

**Improve Existing Tasks!**

Some tasks are old, and lack style. Some may even be re-branded Scheme tasks. Anyone can edit these tasks. Add style to them. Tasks can then not only be an example of how to use the syntax and features of Racket, but also exemplars of well-written code.

**Propose New Tasks!**

There are things that Racket and other Lisps do well that haven't been illustrated on RC. How about the fancier macro facilities that Racket provides?

I'm sure you can think of something. Might you suggest something involving _anaphoric macros_?

Oh, and if you _do_ suggest something, maybe you can implement it, too!

They Haven't Finished
---

**New Tasks are Being Invented!**

Tasks are being added to Rosetta Code constantly. Keep an eye out, some of these are really quite interesting.

**Tasks are Being Implemented!**

 Tcl and Python (and maybe others in the future) will want what we have earned here, and they are going to continue to propose and implement tasks. _"King of the Hill"_ is a precarious place. The more clear blue water between us and them Just do it! Buy glucose sports drinks

 Maybe I _am_ getting too competitive.

 Finally
 ---

 Once again, many thanks to the people who have contributed to Racket on Rosetta Code. Including those who have answered questions on the mailing list or IRC. Your help has been invaluable even if the questions made you wonder "why on earth does he or she want to do _that?_"

 Finally, but certainly not least: Thanks to the folk at Rosetta Code. They've provided a site and experience which have been instructive, educational and fun; and without whom none of this would have been possible.

[1]: Python, is also doing magnificently well, to be sure. It even had the  audacity to draw level with Racket according to the FUPPLR a couple of times.

[2] A good way to start on Rosetta Code is to find tasks that are  _easy_ to implement. In order to find _easy_ tasks you will need to browse the  unimplemented tasks (and maybe some implemented ones, too) and decide what  you could either implement and/or translate without breaking too much of a  sweat. In the process you will also develop a sense of what tasks are out  there ready to be implemented. A good example of an _easy_ task would have been  [Pentagram](http://www.rosettacode.org/wiki/Pentagram).

[3] There is a [Frequently Updated Popular Programming Languages  Report](http://timb.net/popular-languages.html), which I refer to but recently  it has been miscounting tasks, and needs a bit of a look at.

[4] The `#racket` IRC channel is a fantastic community if you need support  with your Racket issues

[5] Remember that Rosetta Code is not a _Golf_ site. If it were, J's weird  20-character-strings-that-do-anything (if only you could remember what they do  30 seconds after you've written them) would win hands down. Keep to the [Style  Guide](http://www.ccs.neu.edu/home/matthias/Style/style/) as best you can. And  since RC is a wiki, if you're not perfect, others can improve the style of your  code.


<!-- more -->



* * *

Where is the list of unsolved task?  I would think this would be in one place.

— *L.A., 6 May 2015*

* * *

The list of unsolved tasks is here: http://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_Racket

— *Jens Axel Søgaard, 6 May 2015*

* * *

