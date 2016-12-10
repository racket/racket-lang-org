
    Title:R6RS is "perfect"
    Date:2007-06-09T13:15:00.000-04:00
    Tags:

*posted by matthias*


When I read the "side by side" and "head to head" descriptions of the alternatives facing the Scheme community (see Comp.Lang.Scheme and the R6RS mailing list), I am wondering which one is which and which one is better.

 
*  Is it really good that Scheme (the spec) doesn't support a module system?
 
*  Is it really good that almost all major implementations support their own version of a module system?
 
*  Is it really good that programmers can't even leave the module structure intact when porting code?

Imagine your own similar questions and add them here. We have lived in a side-by-side universe for a long time, and there are quite a few programmers who have suffered from this not-really-the-same-language problem. Besides the module system, there are other not-quite-the-same-but-related features that implementations have and programmers wish to use.
The R6RS process has pushed several major implementors/implementations to agree on a design for module systems and other constructs. Their report declares that they are ready to put a large amount of work in to get from r5rs to r6rs. I believe that this step would help the community in several arenas, listed in increasing order of relevance:


* the academic publishing business

* the fund raising business 

* adapting each others innovations

* supporting programmers who learn on one and switch to another implementation

* supporting commercial programmers who need reassurance that there is more than one implementation and implementor [ever attended Commercial Uses of Functional Programming?]

Is the document perfect? Is every construct exactly the 'right thing'? Of course not! Guy and Gerry revised their first Scheme report because they didn't get it 'right'. R3RS and R4RS and R5RS revised flaws in R(n-1)RS because the authors/editors didn't get it 'right'. It is extremely difficult, and usually impossible, to get the design of a complex artifacts (such as a programming language) 'right' the first time. In these cases, it's all about the feedback loop and revising your design based on observations. (Remember the 'science' part in the name of our discipline?)  Indeed, 'right' doesn't exist; what exists is 'most pragmatic and internally beautiful,' and nothing else. 
Our choice is quite simple: move forward as a community with some amount of convergence (r6rs) or split into dozens of mutually incompatible sub-communities (status quo, including SRFIs).
_Also posted as [
"R6RS is perfect"](http://lists.r6rs.org/pipermail/r6rs-discuss/2007-June/002538.html) at the R6RS discussion list._

<!-- more -->



* * *

The biggest argument against R6RS seems to be bloat. They say Scheme betrays its root, because it gets big and complex, instead of "smallest possible, while usable."

Is it really debated enough, wether all this stuff belongs into Scheme? Probably. Then why don't people just point to the mailing list thread (or whatever)?

— *beza1e1, 9 June 2007*

* * *

I'm just curious - why cannot the module system and the rest of the stuff that R6RS tries to do be implemented as SRFIs first, and only after they're used for some time they could be frozen into the new spec ?

— *taw, 9 June 2007*

* * *

Easy questions. 

Yes, one can produce an OS with Turing Machines. (At least I am pretty sure one can.) I still wouldn't want to do it. In this spirit, Scheme isn't the best medium for creating large applications and frameworks. 

The implementors (on the editoral board) _have_ implemented variations of the proposed features and they like what they see. PLT users have experienced a good number of them, and they like what they see. Now it's time to commit. 

And I am pretty sure that for those who wish to stay behind, some soul among the implementor crowd (is there a language with more implementations than Scheme?) will maintain an old R4RS or R5RS compatible Scheme.

— *matthias, 9 June 2007*

* * *

As far as I can tell, there's a vocal part of the Scheme community that is entirely unwilling to compromise anything for anyone.  This is a bit foreign to me, having come to Scheme from Common Lisp.  Whether they constitute a plurality or not is another matter, but consider just how many different forms of define-record syntax are out there.

I as a programmer not an implementer would love to see any RnRS with some provision for modules, almost no matter what the syntax or semantics might be.  I might have my own personal favorite, but the network benefit of (almost) everybody using the same damn thing is important.  The SRFI libraries are immensely useful here, but because module systems are such a sore point there is no useful way, even leveraging SRFIs, to write one library that engages the native module system for any two different Scheme implementations.

Pass the damn thing already.  I'll write using Swahili identifiers if need be.

— *Graham, 10 June 2007*

* * *

(Sorry about my non-native English and bad collocations.)

Well, I think the biggest problem with the concept of modules coupled with the core is when we make this question to ourselves: "what is really important to put on core? Is it module? Is it another thing else?"

Anyway, I think one great point of R6RS is about define Unicode as standard... Am I too wrong??

— *SpamKids, 30 August 2009*

* * *

