
    Title:Macros Matter
    Date:2007-05-03T17:35:00.000-04:00
    Tags:

*posted by matthias*

Thank you Jens for setting up this Blog. 



PLT Scheme is a 12-year old project now and it is definitely time to open it up to the world. The language and the project has contributed numerous ideas and products to the world. This covers programming languages (units, mixins, an implementation of cml-style concurrency, etc); programming tools (drscheme, check-syntax, transparent repls, module browsers, etc), programming pedagogy (htdp, htdc); program engineering (we resurrected the "expression" problem, web programming and continuations); and some more. 



Time and again, people have asked me what I consider the one 'feature' that distinguishes us from the rest of the hordes of programming languages. I always respond with a single word: 


**Macros.**


We have pushed macros hard, and we have accomplished a lot with them. I conjecture that without macros, we would never have achieved the level of productivity that this group displays.



Of course, everyone else in academia works on types. ML's module type system of the third kind and Haskell's system-complete type system are serious challenges to anyone.  It is probably true that you shouldn't consider yourself a programmer if you can't read and write some of those type-laden programs, and I seriously believe that they are the next generation of influential languages. 



For the generation-after-the-next then, I see "macros" as one of the big topics (next to concurrency). A real programmer will have to know how Lisp and Scheme-style macros can reduce labor by orders of magnitude, how macros provide the tools for creating the "ultimate abstraction" in the form of domain-specific and embedded languages (Hudak's words). And there is no better place to start with than PLT Scheme's macro system.



So I would like to dedicate this blog to all things macros and everything else that matters in (and to) PLT Scheme.

<!-- more -->



* * *

The Scala programming language claims that its combination of language features allows for the easy addition of new control structures and domain-specific syntax, without the need for macro-based metaprogramming.  

Although I have only a surface knowledge of Scala, the introductory Scala documentation provides some examples to back up this claim, and I'm inclined to believe that Scala's facilities would suffice for most of the purposes that macros are commonly used for.  Scala's "actors" library (essentially an implementation of Erlang-style concurrency with corresponding syntactic extensions) is a good example of a task I would expect to require macros, and yet they do just fine without.

It would be really interesting to to know what a macro expert thinks about Scala's approach.  Is Scala essentially providing macros in disguise, or does it have a real lack of expressive power relative to Scheme's macros?

— *puzzler, 4 May 2007*

* * *

My impression is that Scala has "macro power"
in the same spirit as Java has "closure power." 

Rumor has it that Java 7 will have real closures. 

Someone show me that I am wrong.

— *matthias, 4 May 2007*

* * *

I am pleased to see a PLT Scheme blog, and look forward to reading it.

As you know, Matthias, I teach supplementary lectures on macros and continuations to post-HtDP students. Continuations "fit" -- there are challenges, but they're of the sort that students are used to from other parts of Scheme. Macros are a different matter. Perhaps it's because I don't understand them that well myself, but it seems to me that there is a considerable jump in complexity. They're not as natural, and there are no good expository materials. I would like to see the transition into macrology be a little smoother. Is this possible? --PR

— *plragde@uwaterloo.ca, 4 May 2007*

* * *

Well, I can't speak for other students of scheme, but I found procedural macros easier to grok than the syntax-rules language.  Though I program more in Common Lisp now, the first macro system I learned to use in a non-trivial way was the explicit renaming system in scheme 48

— *akopa, 4 May 2007*

* * *

If you have read the docs, you know that 
PLT Scheme does have procedural macros. Better still, it has those and implements macros as proper abstractions.  

This idea of _macros as proper abstractions_ is why I have strong doubts that Scala has any real "macro power" and that systems such as in CL or S48 will ever be close. 

Because we support macros-as-abstractions, implementing  classes, mixins, and traits as macros is not only feasible, it's a joy. Indeed, implementing an entire language, such as Arc is doable and is no just a toy (as it would be if implemented in a primitive macro system).

— *matthias, 5 May 2007*

* * *

I have not used the syntactic abstractions in PLT.  I wasn't even disparaging Scheme macros in my previous comment.  

However, the introductory materials to Scheme macros always concentrate on syntax-rules.  In addition, there is an implicit assumption that the pattern matching language is the "easy" way to think about source transformation.

This is does not seem to be the case, or at least it isn't sufficient for many Scheme students.

In my case, the mechanics of explicitly constructing the syntax tree helped me enormously.

As far as providing a proper abstraction goes, in Common Lisp the burden is on the macro writer.  Philosophically, Scheme advocates will always find defmacro wanting, but I am able to construct a macro writing framework for my own macros that provide good abstraction and meaningful error reporting for the macro writer and the consumer.  I acknowledge  that it can be a lot of work.

— *akopa, 6 May 2007*

* * *

Hi Akopa,

There are more than one type of macros in the Scheme world. To a Scheme a "procedural macro system" isn't associated with Common Lisp's defmacro.

For simple transformations, syntax-rules is easy to use. However, if you need to write macros that doesn't fit into a simple rewriting rule, then you'll need something more powerful like syntax-case macros.

For a very nice explanation of syntax-case seen from the Common Lisp perspective see:

http://groups.google.com/group/comp.lang.lisp/msg/7893ba79443a82f8?hl=en&

— *Jens Axel Søgaard, 6 May 2007*

* * *

Here is the url broken into 3 lines

http://groups.google.com/
group/comp.lang.lisp/
msg/7893ba79443a82f8?hl=en&

— *Jens Axel Søgaard, 6 May 2007*

* * *

"As far as providing a proper abstraction goes, in Common Lisp the burden is on the macro write."

If that's the case for _any_ linguistic construct, the language simply doesn't support abstraction (for this aspect). Period. Just think: in the spirit of this statement, assembly language supports first-class closures.

— *matthias, 7 May 2007*

* * *

"'As far as providing a proper abstraction goes, in Common Lisp the burden is on the macro write.'

If that's the case for _any_ linguistic construct, the language simply doesn't support abstraction (for this aspect). Period. Just think: in the spirit of this statement, assembly language supports first-class closures."

Yeah, but macro-writing macros allow you to create the abstraction.  Don't get me wrong, it's nice the Scheme supports this out of the box. 

Of course assembly language supports first-class closures; it's just a macro expansion away :).

— *akopa, 7 May 2007*

* * *

Scala has limited non-macro-based facilities for syntactic extension. This along with its liberal method naming syntax and the ability to use anything as infix lets you build things like embedded linear algebra operators (A*B where A and B are matrix operators could be matrix multiplication, etc.).

Scala is a hell of a lot faster than PLT Scheme, and it plays perfectly with the vast Java ecosystem, while Scheme does not. They are good at different things and there is no need for Schemers to feel they need to put down a fine language like Scala. If you're going to play the language debate game, focus your energy on languages that actually do suck.

— *warren, 9 May 2007*

* * *

warren wrote:
> Scala has limited non-macro-based facilities for
> syntactic extension. [...]

Right -- it has no macros.

> Scala is a hell of a lot faster than PLT Scheme,

I find this questionable.  I tried a quick fib
test, and with an input of 38 (which should be
large enough to compensate for Scala's horrendous
initial overhead).  With 5 runs for each, MzScheme
was 2.7 times faster in user time, and 3.2 times
faster in total time.  But we're not playing
language debate games, right?

> and it plays perfectly with the vast Java
> ecosystem, while Scheme does not.

MzScheme does not play with Java code
easily, because it's implemented in C.  Schemes
that are implemented in Java do.  OTOH, MzScheme
plays perfectly with the vast(er) C ecosystem.
But we're not playing language debate games,
right?

> They are good at different things and there is
> no need for Schemers to feel they need to put
> down a fine language like Scala.

I looked again at all comments -- and the only
sense in which Scala was "put down" is by saying
that it has no macros.  Reading the Scala
documentation re its extensibility feature
all I see is an ability to specify thunk
arguments, and automatic thunking of expressions
that are used for these arguments.  Saying that
this is anywhere close to having macros is like
saying that my old Pontiac Grand Prix 93 has the
same functionality of a Ferrari.

The original question was: "Is Scala essentially
providing macros in disguise, or does it have a
real lack of expressive power relative to Scheme's
macros?".  The answer to this question is a simple
and definite "no, it does not have the expressive
power of macros" -- and that's for any macro
system I know about (including non-sexpr macro
systems, like CamlP4 or CPP).  It's as simple as
that.  Language debate games or not.

> If you're going to play the language debate
> game, focus your energy on languages that
> actually do suck.

Look in the above comments again, bearing in mind
my answer above.  Saying that Scala has no macros
is a plain fact, not a subjective arguments.  It
is therefore your own comment that started a
debate game.  The current comment contributes to
that, sorry.

— *Eli Barzilay, 9 May 2007*

* * *

eli, is CamlP4 is missing anything besides the obvious thing (ie, hygiene)?

— *Neel Krishnaswami, 9 May 2007*

* * *

Well, hygiene is a big deal.  The advantage that CamlP4 gives you over CPP is that it uses proper structures, which is also what you get with defmacro, so it's a good step in that direction.

One techincal point here is that when working with it, I found that the types got in my way.  The AST type is quite big, and evey piece of code must have many trivial cases.  One solution to that was obvious: create an s-expr-like type, and translate the AST to that.

Getting hygiene is also related to being integrated into the languaue, which is another thing you don't get with CamlP4.  Transformations live completely outside of your code, so, for example, it is not possible to have libraries that provide new syntax transformers.

This, combined with the heavy AST type mean that you need to do a *lot* of work for every transformation, which makes it more difficult to use.  I don't see people going through all that just to implement a small debugging macro, or something simple like `when' or a while loop.

BTW, one thing I did when I worked with CamlP4 is to create a defmacro-like facility.  It was limited in many ways: different AST nodes means that my macros would only work with AST subsets that make sense both as expressions and as patterns; the scope was in-file only; we needed to add facilities for including code etc etc.  Still, it was much better than CPP (which was used for debug code), and even fixed a couple of bugs.

— *Eli Barzilay, 9 May 2007*

* * *

Thanks for your comment. The way I usually use P4 is to define a small grammar for my extension, with a custom AST type for that. Then, I write a function to compile my custom AST into the Ocaml expression syntax. This matches how I write things on the whiteboard pretty well, and personally I found it more convenient than syntax-rules (which has a bizarre evaluation order). 

I do get really annoyed every time I have to define a gensym function -- I should not have to do this in 2007! -- and I take your point about the need for better language integration, because I have often wanted to write type-directed transformations. (For example, if you want to extend pattern matching and still check coverage, you need to know all the type and module definitions in the current scope.) You can't do this with P4.

— *Neel Krishnaswami, 10 May 2007*

* * *

Let me preface my comment by saying that I think PLT Scheme is a wonderful language, and my thanks to the PLT folks for making it happen.  With that said, I wanted to respond to these comments by Matthias:

"Because we support macros-as-abstractions, implementing classes, mixins, and traits as macros is not only feasible, it's a joy. Indeed, implementing an entire language, such as Arc is doable and is no just a toy (as it would be if implemented in a primitive macro system)."

I heard from a reliable source that the current Arc implementation doesn't use scheme macros at all.  So it perhaps is a bad example to use in this case.

— *Repl, 10 June 2007*

* * *

