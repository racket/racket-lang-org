
    Title:Don't say "abstract" (instead say "general")
    Date:2007-09-14T17:56:00.000-04:00
    Tags:

*posted by John Clements*

The word "abstract" is common in computer science.  An abstract thing is one where some part of the whole is unspecified.  For instance, the expression "3*x + 3" is an abstraction of the expression "3*4+3", because the "x" is unspecified. Likewise, a function is an abstraction over some set of values, supplied when the function is called.

The word "general" is not at all common in computer science.  In non-computer-science use, the word "general" is used to describe things that may be applied to more than one thing or situation.  For instance, a "more general solution" is one that applies not just to the problem at hand, but instead to a larger set of problems.

From a computer science perspective, things that are abstract are also general.  Things that are general are also abstract.  Substituting the word "general" for the word "abstract" would not be a terrible hurdle.

From a non-computer-science perspective, however, "general" and "abstract" have very different implications.  Something that is general is better: it is more useful, it applies more frequently.  Something that is abstract, though, is worse: it is lacking detail, it is non-concrete.

This is one difference--the major difference?--between computer science (and of course mathematics) and the real world: the abstract is no less concrete. We can abstract over expressions using functions, and we can even abstract over syntactic things, using hygienic macros.  The result of such abstraction is a perfectly well-defined element in our universe of expressions.

In computer science, then, the pejorative sense of the word "abstract" is misleading, and the use of the terms "abstract" and "abstraction" merely provides ammunition for those who wish that we could all still be writing assembly language.

I suggest instead the use of the word "general."

John "purveyor of barbarous neologisms" Clements

<!-- more -->



* * *

Why would you want to adjust a valid and widespread usage just to integrate with usage elsewhere?  If we followed this idea to its logical conclusion we would have no vocabulary at all.

— *amoe, 15 September 2007*

* * *

Makes sense to me if you want to talk with the folks from elsewhere that you should learn their language. If you just want to stick with your own people, then you don't bother with things like that.

— *Robby, 15 September 2007*

* * *

I'm not sure that "abstract" and 
"general" capture fungible notions.

In computer science and mathematics, abstraction captures the essential core of something.  I remember hearing David Foster Wallace, who'd written a book about math, talking about how the number 5 is an abstraction over 5 sheep, 5 houses, and so on.  But CS/math abstractions are very precise. I know exactly what you mean when
you say "five".

To me, the word "general" as used in common parlance connotes some amount of fuzziness.  "Oh, that's the general idea."  Not what you 
want for computing -- unless maybe 
you're Lofti Zadeh.

— *steck, 4 October 2007*

* * *

I know exactly what you mean when you say "five".

You do?  5 might be Frege's 5 (the set of all fivesomes), or von Neumann's 5 (the set {0, 1, 2, 3, 4}, where 4 is {0, 1, 2, 3}, where 3 is {0, 1, 2}, where 2 is {0, 1}, where 1 is {0}, where 0 is {}), or some other 5 altogether.

— *John Cowan, 30 October 2007*

* * *

