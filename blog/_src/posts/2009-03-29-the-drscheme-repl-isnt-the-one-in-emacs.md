
    Title:the DrScheme repl isn't the one in Emacs
    Date:2009-03-29T13:03:00.004-04:00
    Tags:

*posted by matthias*

Dear old Lisper,

You have found drscheme and it is almost like your old Lisp machine.
It is easy to program in it, it has things that nobody else has, and
we all love parentheses. But after some initial enthusiasm you
are wondering why in the world, we decided not to provide
commands for sending individual definitions and expressions
from the Definitions window to the Interactions window, aka,
REPL.

It wasn't an accident. It was by design after some difficult
experiences. I am personally a product of the Emacs world that you are describing below,
and my advisor Dan Friedman was called the "Lispman" on his door
sign at Indiana.

When I first discovered the idea of sending individual expressions
and definitions from a buffer to a repl, it was a near-religious
revelation to me. I wanted everyone to know this trick and use it.
When I started teaching the freshman course at Rice, I told our
chairman so and he asked "why". I was shocked, awed, and I failed
to explain to him how it mattered. He was a mathematician and I
wrote it off. They don't know.

Then I started watching my sophomores and juniors at Rice in lab.
Now that was a true disappointment. Few if any used this trick and
when they did, they more often tripped up and got the repl into a
state where they didn't know what was going on.

In the mid 90s, I wrote some more Little books with Dan, and boy,
time and again, I watched him stumble across the state of the repl.
I even watched him re-start the repl and load the whole buffer more
often than not.

Why? In the presence of macros and higher-order functions and
other beasts, it is difficult for masters of the universe with 30
years of experience to keep track of things. What do you think
students with 10 or 20 days worth of experience will do? Is it
really such a deep principle of computing to create the objects
incrementally in the repl as opposed to thinking systematically
through the design of a program?

I decided not and asked Robby to make DrScheme's repl transparent.
That is, it re-starts the repl and re-loads the buffer every time.
I consider this behavior a suitable compromise: have a repl but
don't confuse yourself with send-defs and send-exprs. This is
especially true in an age when sending an entire buffer takes as
much time as sending an individual expression or definition.
Soon we'll get "compilation behind your back" so that only the
current buffer is re-interpreted. It'll start things even faster.

Even though I had used the incremental mode for more than a decade
when I switched from Emacs to DrScheme in 1998, I have hardly ever
looked back. I miss a few other things but the incremental repl
is one of those rituals old Lispers acquired and never questioned
... but it isn't fundamental and critical to anything. (Note
there is no qualifying clauses, no when no if no but. I really mean
this last sentence the way I spelled it.)

<!-- more -->



* * *

This comment has been removed by the author.

— *Kyle Cronin, 29 March 2009*

* * *

Why not provide the ability to do both? Reloading an expression and reloading the entire buffer are two different things that, while their functionality somewhat overlaps, provide two distinct ways of interacting with Lisp/Scheme expressions. You can have your big "Run" button for students new to Scheme, but also perhaps provide a keyboard shortcut that would send a definition or expression to the current REPL. One advantage to this is that often times when I'm testing my code I'll create temporary variables with dummy data in the REPL, and being able to reload a definition without recreating the test data would be quite a timesaver. In other words, both mechanisms have their uses, and there's no reason why both can't be included.
(this is a revision of my first reply, mainly due to some grammar issues)

— *Kyle Cronin, 29 March 2009*

* * *

I can understand the motivation behind the design. But this way of working does not work well with my way of developing a program incrementally.

Let's say you are developing the procedure string-prefix? which answers if a string is a prefix of another. You have a file utils.scm with the definition of string-prefix?

Now you want to test it. Start the REPL and try your definition with a couple of examples. Iterate the process until you have a correct function.

The bad thing here is that all the tests you have developed are only in the REPL and are not saved in the file utils.scm. So if you continue working on your program later and need to verify the definition of string-prefix? you must re-type and re-engineer a test suite for it.

The way I do it, is to have the test suite in comments around the definition and send those tests with send exp and watch for the results.

— *Pierre, 29 March 2009*

* * *

I, too, do not see why it can't be an option.  One of the frustrations I have with the REPL in DrScheme is related to the error-flagging whenever I define something in the REPL.  

Yes, dang it, I *KNOW* it's inconsistent with the definitions window.

Could it possibly be an option?

— *Yakov, 29 March 2009*

* * *

I am guessing that most people avoid this by doing the exploratory
part of the programming inside of the REPL and when they are happy
with new functions they do a:

M:S:right-arrow
C:c
C:F6
C:v

— *grant rettke, 29 March 2009*

* * *

Another useful feature gone because somebody thinks they know better what is good for other people than those people themselves.

— *Pascal Costanza, 29 March 2009*

* * *

Just like `goto`.

— *Eli Barzilay, 29 March 2009*

* * *

Pascal: Someone will probably write a DrScheme plugin to add this feature.

— *grant rettke, 29 March 2009*

* * *

"Another feature gone ..." -- I didn't know that features go away via someone's effort; I thought they came *in* via someone's effort. I must have been doing it wrong all along!

— *Robby, 29 March 2009*

* * *

See this quote from Steele and Sussman.

— *Pascal Costanza, 29 March 2009*

* * *

Pascal,
Just to be clear: yes, I know about using `lambda` as a `goto`.  In a sane language like Scheme, `goto` is gone -- but only on the superficial level of what you see when you browse through the report or the manual.
An incremental REPL is gone from PLT Scheme in a similarly superficial way: DrScheme does not support it -- but it's still part of the language, and you can still fire up MzScheme and talk to it like any other REPL.  More than that, as said in previous comments: it wouldn't be too hard to implement a send-last-sexpr functionality in DrScheme.  Why was it done so far?  For the exact same reason that nobody implemented a `prog` macro in PLT: nobody really needed it.

— *Eli Barzilay, 29 March 2009*

* * *

Eli, the article above argues that it is a feature designed into DrScheme. It is one reason I never got warm with DrScheme, it does not support the style of interaction I like. I'm old. Bearded. Lisp hacker. I'm probably not the 'target' for it. Yeah, how I liked MacScheme - that was fun.

— *Rainer, 30 March 2009*

* * *

That was interesting. I am new to Scheme (PLT Scheme) and I have only played with DrScheme so I didn't know there was any other "way" to do it.

Now, I have read the responses from the "incrementalists" and I see the point they are making. Couldn't what they want be just another "mode" for DrScheme? You change the option to "Incremental REPL" or something and you get the way of working they like.

— *Robert, 30 March 2009*

* * *

Rainer: yes, it is a feature designed into DrScheme.  But as mentioned above several times now, it doesn't contradict having the incremental kind of REPL too: all that is needed for that is a simple send-to-repl key, and that's really not too hard to do.
See also Robby's reply: it took effort to implement the resetting functionality -- an effort that was made due to the design decision. So DrScheme has an additional feature -- and implementing a send-to-repl key just happens to be a something that is not going to use that feature (making it even easier to implement).
Back for the reason why it wasn't implemented so far: it's because once people got used to it, they liked it.  Once they liked it, they saw no need to implement support for the old mode of work.  It's actually quite similar to why we use S-expression syntax: once people got used to that, they didn't see any reason to go back and implement M-expressions as originally planned.

— *Eli Barzilay, 30 March 2009*

* * *

I do the vast majority of my programming as a scientist (in Matlab, unfortunately).  However, I recently did some consulting work where the language I used was not important and so chose to use PLTScheme.  Like my scientific work, though, this consulting involved a lot of large data sets which one wanted to load, and then interactively transform - generally speaking such "exploratory data analysis" is not a process for which large scale program design is very useful.  What is useful is to be able to load large data sets (or perform long simulations or calculations) once, storing them in variables in a running interpreter, and then interactively plot and/or analyze the results - a process which often involves sending partial expressions to a running process to be evaluated.  Had I been able to do this in DrScheme I might have used the environment.  The absence of this feature was definitely irksome enough that I decided to use Emacs and Quack for my development instead.

I don't know how many other programmers out there are use their programming languages as extensions of their ability to visualize and analyze data rather than as systems to design applications, but I would suspect it is a fair number.  For them, it may be worth the confusion to allow incremental compilation/evaluation.

— *J.V. Toups, 30 March 2009*

* * *

No matter -- old Lispers can just post 
to them newfangled Wikis to see the 
effect of small changes in state on 
the big picture.

-- Paul

— *steck, 31 March 2009*

* * *

J.V., the data analysis problem is precisely the kind of process where a specialized environment is possibly superior to the old incremental Emacs mode. Just like writing shell scripts is neat once you have figured out how to use the Language dialog to preset command line arguments.

— *matthias, 31 March 2009*

* * *

Why so much fuss about it?  Like Grant pointed out, I generally develop and refine functions in the REPL and when I'm satisfied with it, I write it to the definitions buffer.  You may also copy from the definitions, copy on the REPL and further refine it, despite the "out of sync" alert.

— *namekuseijin, 2 April 2009*

* * *

namekuseijin,

Sure, you can also program with one arm tied behind your back. It's just not very helpful.

The fact that you're programming in this style proves that you would benefit from a less restrictive environment. It also proves that the argument that "nobody implemented it, because nobody needed it" is not really valid. We have actually already seen a few people mentioning here that they would "need" it. But unfortunately, many programmers just use whatever they are given and don't question their tools too much, and instead use workarounds, like you seem to do in this case.

You see this also a lot in the Java world. So, indeed, nothing much is really fundamental and critical to anything...

— *Pascal Costanza, 2 April 2009*

* * *

"A less restrictive environment" is bogus: the environment lacks a keypress that makes copy-from-definitions-paste-to-repl easier, but it is *not* restricted.  Confusing that with "a restriction" is what makes you draw other bogus conclusions, so I'll stop here.

— *Eli Barzilay, 2 April 2009*

* * *

Pascal, like I said I usually just work out the functions at the excellent command line -- complete with full command history, completion and parenthetical editing -- and then paste to the definitions once I'm done.

If I ever have to redefine it, I may just either access the command history (even from previous closed sessions) or, from your usual emacs position when doing the same (in the tail of a definition), do:
Alt+Shift+<- (to select the definition)
Ctrl+C (to copy)
Ctrl+D (do go to interactions)
Ctrl+V+Enter (to rerun the definition)

Hardly Earth-shattering, specially in the face of the powerful command-line editing.  But yeah, the above procedure could be turned into a single step with something as keybinding customization.  Section 3.3.7 of the DrScheme environment manual spill out the details.

I've grown on Emacs, but got fed up of all the baroqueness and hand dextery.  I'm no virtuoso pianist.

— *namekuseijin, 3 April 2009*

* * *

namekuseijin,

Sure, it's possible to find workarounds to get what you want. And there may indeed be very good reasons to stick to these workarounds, because you deem other things more important than this.

But note that your suggested workaround makes things even harder to follow than what Matthias describes in his blog posting. Not only do you now have to track which definitions actually resemble closest the current state of the image, you also have to track them down in a long list of definitions that is ordered by history rather than by some program logic.

By the way, emacs is a straw man here. I don't like emacs that much either. But there are much better alternatives, like the IDEs for Macintosh Common Lisp, LispWorks, Allegro Common Lisp, and Corman Lisp, to name just a few current ones.

For example, in LispWorks I can choose to define the current selected form in the buffer - or even undefine it! - by clicking the respective entries in the contextual menu that I get, as usual, by right-clicking. I can also select several forms in a row and define or undefine them in one go, which makes it already a lot easier to have some confidence in a consistent state of the system. And by using a system definition facility, it is also quite straightforward to do a complete reload of everything, but without invalidating all the data on which I want to test my definitions.

But that's Common Lisp, and that's worse than Scheme in every respect, right? So much for religion...

— *Pascal Costanza, 4 April 2009*

* * *

Pascal: your comments go down a line where each is more bogus than the previous.  You now claim that namekuseijin's method is harder because definitions don't follow some mythical "program logic".  With any conventional REPL (including the one in DrScheme and in MzScheme) there is no "program logic" that dictates the ordering of definitions entered on the REPL -- there is just the history of whatever you happen to have typed in.  The core of Matthias's post is exactly about that: this history leads to hidden dependencies (and therefore bugs) that are not evident in your code.

Yes, there are some Lisps that will allow you to dump the current runtime image -- and yes, you can use that as a kind of a formalization of the history as "the code" -- but trying to write real software (one that requires maintenance) this way is crazy.  The problems involved in doing this kind of development are hard, and -- AFAIK -- no Lisp is actually trying to solve them *properly*.  Ad-hoc "solutions" involve tools similar to what you mention: undefining a name, retroactively defining a name, etc.  CLOS is a good example in the retroactive-change attempt: defining an already existing class has some complex semantics that try to update previously defined instances, and that tends to work only with very simple classes.  In the more common cases you end up facing a need for `update-instance-for-redefined-class` -- which means that you now need to actually write code around the problem inherent in REPLs.  Worse, if the redefinition is due to change in your source, this code is a one-time throwaway tool that will never be used outside of your currently outdated REPL.

IME, I found myself reaching a similar conclusion to Matthias: in theory, I can maintain the REPL, and I can invest some effort in doing so by writing update methods.  In practice, it's just easier to restart the REPL and be done with it.  It's nice to have a REPL, and it's easy to get carried away with it to the point where I write code for the sake of not killing it -- and at that point a relevant question is whether the REPL is helping me or whether I'm helping the REPL.

The bottom line is that the Lisp world is infested with loading-order problems, and there are a good number of packages (and correspondingly a good amount of human effort) that try to solve these issues.  And this is not some theoretical "we had a problem and now it's solved" -- I see people who are running into these problems today, and I see people who are working on these problems today.

— *Eli Barzilay, 4 April 2009*

* * *

Eli,

If you send definitions from the buffer to the image, or send commands to undefine them, then you can preserve the arrangement of the program text according to some program logic. This has nothing to do with the REPL. (Corman Lisp seems very interesting here because it actually does not make the distinction between a buffer and a REPL anymore, but gives you only buffers from which you can interact with the image.)

It's good that you have reached some conclusions and stick to a certain programming style. But why do you (or Matthias) make attempts to impose your own views on others?

— *Pascal Costanza, 4 April 2009*

* * *

Never thought blogs could be as amusing as usenet. :)

In any case, I'll get onto Corman Lisp when I'm fed up with DrScheme's interaction style.  should take a while...

— *namekuseijin, 4 April 2009*

* * *

Pascal,

  No one is imposing their will on others.

Indeed, others have explaining how one might build the kind of interactions you propose into DrScheme, if you were so inclined. As I said in a rather obtuse way earlier (for which I apologize), it takes work to do such things. If someone deems it important, they can certainly do it. I'd be more than happy to help if said person gets stuck in trying.

— *Robby, 4 April 2009*

* * *

Pascal,

Re "If you send definitions from the buffer to the image, or send commands to undefine them" -- that would be equivalent to writing your program with `eval`.  While this is available in a number of languages including outside of the Lisp world, I have yet to see anyone writing "Real" code that way, for exactly the same reason I mentioned earlier: maintaining such code would be a nightmare.  (And this is even if your environment provides your with more powerful eval-like primitives like querying the current bindings, or removing a binding -- all existing in MzScheme (and in DrScheme, of course) too, BTW.)

And re "make attempts to impose your own views on others" -- where did I do that??  MzScheme *does* have these feature, DrScheme only *adds* a feature on top of MzScheme.  Like I said about 2000 times now, the only thing that is actually missing from DrScheme would be a convenient "send the current expression to the REPL" key -- which is very likely to be very easy to add.  I also said that nobody did this because nobody has seen any burning need to do so.  I can tell you even more: I *do* use the REPL to debug code every once in a while. Why didn't I bother adding that feature?  Well, I said that too: I know where Emacs is when I need it, and Emacs is already doing what Emacs is doing very well -- I have absolutely no desire to compete with it.

— *Eli Barzilay, 4 April 2009*

* * *

Having read through the comments, while I can understand the reasoning behind not having a "send the current expression to the REPL" key, it seems that there are two distinct schools of thought here, with no middle ground.

The best way to support both schools would probably be to have two distinct modes, one for each of both.  I, for one, could definitely use both, each for different purposes.

One reason that I still keep MIT/GNU Scheme around is that I like its feature to use C-x C-e to return the current expression to the REPL.  It is actually convenient when working through such a book as SICP, because then I can keep a running log of the current session history without resetting the session every time I change a definition.  It is also one reason that I haven't used DrScheme for SICP yet.

On the other hand, I can see Matthias's point in re-starting the REPL and re-loading the definitions buffer for users who have difficulty in keeping track of the state of the REPL.

Considering that effort was spent in making DrScheme's REPL transparent, and that it wasn't transparent before, why not retain the current mode in as a "non-interactive mode," and simply create an alternative "interactive mode" based on code from the code base of an earlier version of DrScheme before that change was made.

This should not take too much effort, and should be acceptable by both camps.  It would resolve this conflict, and add a useful alternative, making DrScheme acceptable to both schools of interaction.

— *Benjamin L. Russell, 8 April 2009*

* * *

Ok, so here is a try to create a keybinding to push a sexp to the interaction windows (I'm not sure I will personally use this though):

In "keys.ss" that defines my DrScheme keybindings, I bind "c:e" with a function that takes an editor and an event.
Then I use:
(send (send editor get-keymap) call-function "select-backward-sexp" editor event #f)
and calls alike with "shift-focus", "copy-clipboard", ...

The problem is once "shift-focus" has been called, the focused editor has changed, but I don't know what it is, so I can't call "paste-clipboard" on the new editor (the interaction window in fact, which I'm not sure is really an editor).
So how do I get the editor under focus? Or how do I get the interactions editor?

— *Laurent, 22 January 2010*

* * *

I've added a note to the keybindings documentation that comes with the code to implement a few of the standard 'send things to the repl' keybindings.

— *Robby, 24 September 2011*

* * *

