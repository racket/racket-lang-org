
    Title:Small is Beautiful, Large is Useful, and Scheme is Both
    Date:2007-06-14T14:05:00.000-04:00
    Tags:

*posted by matthias*

 They say, Scheme is small and this is good.

 Have you heard of X? No? It is **the** smallest computational basis. It is a single function that can compute everything a Turing machine can compute; a Church lambda calculus; a Post model; a RAM; a what-have-you model of computation. Indeed, X is so simple that two equations suffice to specify it completely [[Barendregt](http://www.andrew.cmu.edu/user/cebrown/notes/barendregt.html), [page 166](http://www.andrew.cmu.edu/user/cebrown/notes/barendregt.html#8)]. Imagine that: a complete language report in two lines; a compiler that fits in a few K instead of Ms; no more arguments about smallness. 

 Small alone can't be any good. If you used X alone, your programs would be the size of the universe or something like that. That's what the theory of computability teaches us [[Church and Turing](http://en.wikipedia.org/wiki/Church-Turing_thesis)]. Adding LAMBDA and a few primitives to get a pure functional language isn't good enough either. That's what the theory of expressiveness shows [[Felleisen](http://www.ccs.neu.edu/scheme/pubs/#scp91-felleisen); [Mitchell](http://theory.stanford.edu/people/jcm/publications.htm); [Riecke](http://portal.acm.org/citation.cfm?id=99583.99617)]. And, using an R5RS Scheme to build large systems with many people at a dozen sites isn't doable either. That's what the PLT experience determined.

When we set out to construct DrScheme using MzScheme, we also conducted an experiment:

 Could we really build a graphical system that manages (shared) resources and that provides excellent error feedback with just plain Scheme? 

 Could we just add enough libraries to do all this? Or would we have to change the kernel of the language?  As much as we tried to keep MzScheme small, it became clear quickly that we needed exceptions, structures, module-like features, a mechanism for concurrency, a way to manage resources such as windows, tcp connections, and so on. The list isn't infinite but it is much longer than I expected. Our "Revenge of the Son of the LISP machine" paper is a good summary for the state of the art around 1999 [[Flatt and Son](http://www.cs.utah.edu/~mflatt/publications/index.html)].

As language designers we stepped back time and again to look at our monster. What could we remove? What would we have to add in response? For some five years, we had first-class modules (units) and first-class classes in the core of the language. We had almost banned macros. They were so ugly I stopped teaching about them because I did want to use our own dog food in my courses but I couldn't stomach the macro system. It was such a step back from Eugene's extend-syntax. But then Matthew figured out the next big step in macro and module technology [[Flatt, You Want It When?](http://www.cs.utah.edu/~mflatt/publications/index.html)]. And with that out went units and classes from the core and many other things. So we learned lessons, and we need to keep building systems to learn more.

I have no question that the idea of Scheme is beautiful. At the same time, I have also learned that if I wish to use this beautiful idea in practice, I need to add the ingredients that it takes to build large systems. R6RS reflects this insight, and I am happy about it.

<!-- more -->



* * *

Doesn't this ultimately mean that the Scheme program [1] has failed? Or to put it differently: Why not go back to older Lisp dialects and rediscover what was gratuitously thrown away just because it wasn't considered essential?

[1] "Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses and restrictions that make additional features appear necessary.", as the introduction to several instances of the Scheme report states.

— *Pascal Costanza, 14 June 2007*

* * *

LISP was very large.  Scheme was much smaller.  Does this mean that adding features to Scheme moves it back toward LISP?  No, not necessarily. If we learn from our progress, our "second crack" at some of these features may be nicer[*] than our first try.

[*] more expressive, less difficult to implement, syntactically cleaner, etc.

John "De Gustibus..." Clements

— *John Clements, 14 June 2007*

* * *

I was interested to follow up on some of the papers, but the links didn't seem to take me to either the Flatt and Son or the You Want it When? papers....

— *Robert, 15 June 2007*

* * *

Hi Robert,

"You want it When?" is the subtitle of the paper: 
Composable and Compilable Macros.

/Jens Axel

— *Jens Axel Søgaard, 15 June 2007*

* * *

Pascal: The Scheme "program" hasn't failed at all because it isn't a program. It is a useful "Abmahnung" (imperative is as close as I can get in English) for the designers of programming languages. Due to this imperative, Scheme is simpler than CL to this day and will remain so: one space for values; one powerful construction for control; etc. To this day, I consider Scheme the experimental "out of the box" cousin of CL, and when Guy will write CLtL/3 one day, perhaps he and his colleagues will borrow from Scheme. 

Robert: "son" refers to an ICFP paper with the title "revenge of the son of the LISP machine" and "You want it" refers to an ICFP paper on modules and macros. Search on Matthew's page; he doesn't seem to have tags/labels available.

— *matthias, 15 June 2007*

* * *

I stick to my question: Doesn't this mean that Scheme has ultimately failed to adhere to the imperative it set for itself?

As far as I see it, the major contribution of the original Scheme reports was to show that it is possible to define a substrate of the then common language constructs and programming paradigms. First-class continuations and tail recursion give you the means to implement any control abstractions (including advanced ones like backtracking and generators), lambda expressions give you the basic mechanisms to implement data structures (including advanced ones like Smalltalk-style objects). Indeed, Scheme removed "the weaknesses and restrictions that make such additional features appear necessary" at the end of the 70's / beginning of the 80's.

By R6RS, Scheme will have seen the addition of, for example, hygienic macros, a module system, Unicode, bytevectors, and records. However, these features have just been added on top of the existing Scheme language. It's at least not obvious to me that attempts have been made to remove "the weaknesses and restrictions that make such additional features appear necessary." To the contrary: Syntax-rules was adopted as a macro system, although Clinger's "Macros that Work" would have been a subset on top of which other hygienic macro systems could have been built. Syntax-case is now being adopted but doesn't replace syntax-rules, although it's the more general macro system. If anyone wanted to add, say, syntactic closures on top of R6RS, there is no machinery to do so (or it's at least not obvious how to do this). Next, there is no substrate for implementing module systems, for example, in order to experiment with alternative module systems. I don't know whether such a substrate would be possible, but this doesn't change the fact that "just" one of the possible design alternative is being adopted. Scheme doesn't seem to adopt a way to extend the kinds of character sets it can deal with. Instead, Unicode is being adopted, again without a substrate that could enable other kinds of character sets. Bytevectors are given a literal syntax, but there is no provision for programmatically extending surface syntax, although it is know how to do this in Lisp dialects. Records are provided with different APIs. Although there is a low-level functional API for dealing with records, again there is no substrate that would allow building other variations of records. For example, there is no way to add multiple inheritance to records or to change how record fields are represented, although again it is known how to achieve these things with appropriate substrates.

I agree with you that the notion of a minimal language that provides enough rope to build everything else on top is essentially a pipe dream. But claiming that a particular language is closer to this ideal than others although it is objectively (!) as bloated as the next one is pretentious.

With regard to your remarks about a hypothetical CLtL3: I am not worried about the prospect of feeding some of the experiences of building current Scheme systems back into Common Lisp. To the contrary, I actually think that a lot of interesting work has been done in the Scheme community, some of which surpasses what is possible in Common Lisp. However, I am worried about the fact that this doesn't seem to happen the other way around. Few Schemers seem to have a good working knowledge of Common Lisp, as you can tell for example by mischaracterizations of Common Lisp in papers about Scheme-related research. Those who do, usually assess Common Lisp in a much more humble way. (See http://community.schemewiki.org/?Scheme-VS-Common-Lisp for example.)

It's a shame that there is so little communication between the Scheme and the Common Lisp communities. We could learn a lot from each other, and it is important to realize that this holds for both directions. I am glad that Christian Queinnec, Manuel Serrano and especially Michael Sperber were present at this year's International Lisp Conference (mostly attended by Common Lispers), and hope that this has a positive effective in that regard in the long run.

— *Pascal Costanza, 16 June 2007*

* * *

1. "Scheme is simpler than CL" does not mean it is better than CL. Perhaps I should have used the word "uniform" instead. See the conclusion of my expressiveness paper for "better"; this is not a new opinion. 

2. If you were on the PLT mailing list, you'd see how much we try to learn from CL. We are currently discussing comprehensions and loops and keywords. We naturally look at CL (and other approaches) before we make a decision. [And besides Mike and Queinnec and Manuel, Friedman, Clinger and I have given talks at the conference when it is over here.]

3. As for the substrate vs API decisions, I think what you are trying to say is that the idea of a thin API has failed. You are right in that dimension.

— *matthias, 16 June 2007*

* * *

1. Scheme was simpler / more uniform than CL, but not anymore. R6RS, as it is currently proposed, is simpler / more uniform in some respects, but more complex in others. For example, in Common Lisp, the external and the internal representation of source code is exactly the same, whereas Scheme makes a distinction between s-expressions and syntax objects. So Common Lisp is more uniform in that regard.

This is a general observation in programming language design, IMHO: If you simplify some aspects of a programming language, others will turn out more complex, and as soon as a programming language grows beyond a certain size, the best you can do is to try to balance the various trade offs. Unfortunately, the initial simplifications in the originally covered areas limit the kinds of decisions you can afford to make in other areas later on. This is a fate that a lot of programming languages have faced in the past and are still facing.

I didn't try to say anything about "better" (and didn't intend to suggest that you do). "Better" is, of course, always relative to a particular task at hand.

2. This is good to hear. The mailing list archive is hard to navigate and it seems impossible to search in it. The load seems to be high, so I probably will not subscribe to it. Just a hint: In Common Lisp, the performance overhead caused by keyword arguments can be compensated by taking advantage of compiler macros. This gives you full flexibility of "dynamic" keyword arguments while being able to preprocess them at compile time when concrete keyword arguments are statically known.

3. It's good to hear that we agree on something. :)

— *Pascal Costanza, 16 June 2007*

* * *

It's a shame that there is so little communication between the Scheme and the Common Lisp communities. We could learn a lot from each other, and it is important to realize that this holds for both directions. I am glad that Christian Queinnec, Manuel Serrano and especially Michael Sperber were present at this year's International Lisp Conference (mostly attended by Common Lispers), and hope that this has a positive effective in that regard in the long run.

— *kuril, 29 April 2008*

* * *

