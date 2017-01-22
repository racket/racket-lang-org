
    Title:On `eval` in dynamic languages generally and in Racket specifically
    Date:2011-10-18T14:28:00.004-04:00
    Tags:

*posted by Matthew Flatt*

The `eval` function is at the heart of a dynamic language,
and it strikes many newcomers as an amazingly powerful tool. At the
same time, experienced programmers avoid `eval`, because
unnecessary use creates trouble. It's not easy to explain
why `eval` should be avoided or when it"s appropriate to
use `eval`, but I'll take another stab at it here.


What is `eval`?
--

Consider the following "program" in English prose:


 > Assume that your favorite color is red. Now imagine a balloon that is
 your favorite color. Paint a canvas the same color as the balloon.


As English goes, that's a fairly clear program with a fairly
well-defined result. When I follow those instructions, at least, I
will always produce a red canvas (assuming that I have a canvas and
some red paint, but a potential lack of art supplies is not the point
here).

I would come up with a red canvas even if I read the instructions when
surrounded by people who speak only Chinese, obviously, since I'm the
one reading the instructions. Furthermore, it would be straightforward
to translate the program to Chinese, and then a person who reads
Chinese would produce a red canvas.

A translator might even take the liberty of simplifying the program to
just


 > Paint a canvas red.


The translation loses some of the poetry of the original, but the
result is the same.

In Racket terms, the paragraph corresponds to a module. It can be
compiled (i.e., translated) and optimized (i.e., simplified). A
program can be made up of multiple modules that are written in
different languages, but since each module can be reliably translated,
they can all be compiled into some common language to run the
program.


Here's a different program:


> Tell the person next to you "Assume that your favorite color is red." Tell the person "Now, imagine a balloon that is your favorite color." Tell the person "Paint canvas the same color as the balloon."


Getting a red canvas back may be a little trickier in this case. If the
person next to me speaks only Chinese, then my program may fail with a
message-not-understood error.

If I want to translate the program to Chinese, then it's not clear
whether the parts in quotes should be translated. Maybe I mean for a
person who can read Chinese but only sound out English to run the
program when surrounded by English speakers, or maybe I mean for a
Chinese person to run the program when surrounded by Chinese people.
Either way, I have to be a lot more specific to a translator. For more
complex programs, the instructions to the translator can become
complex and fragile.

Finally, a translator probably won't feel comfortable simplifying the
program to


> Tell the person next to you "Paint a canvas red."



because there could be all sorts of environmental conditions that make
the result different--such as people who are willing to paint
but unwilling to accept assumptions about their favorite colors.

The paragraph with "tell the person..." is a program that uses `eval`.
It can't be compiled and optimized as well as the earlier paragraph, and the language context
in which it is run may change the result. The quotes around sentences
correspond to the quote in front of an expression passed to `eval` in
Racket; there's no particular reason that the language for `eval` will
match the language of the program that has the quoted text. The
issues become even more complex if you try to implement different
parts of the program in different languages.

If the analogy to multiple spoken languages seems strange--maybe
your language is Javascript, period--the problem of translation to
another language is really a proxy for program understanding. There's
a direct connection to performance and optimization (i.e., translation
to efficient machine code), but using `eval` also makes a program more
difficult to understand for the same reasons that it makes the program
more difficult to translate. For example, a reader of your program may
not be able to tell whether "assume your favorite color is red" is
just a rhetorical device to get to a red canvas or whether some new
instructions will arrive that will ask for your favorite color.


When is `eval` Good?
--

The program with "tell the person next to you" above uses `eval` in a
bad way. The task could just as well be performed by the person reading
the instructions, instead of getting another nearby person involved.

Some other uses `eval` are both good and necessary. For example,
consider the following program:


> Ask the construction manager for instructions. Walk to the building
 site and convey those instructions to the construction crew.


This program uses `eval` when it conveys instructions to the
construction crew, but no quoted forms appear in the program. The
absence of quoted code is one sign that `eval` may be
appropriate. Note that the program could work no matter what language
the manager and crew speak, although there is an implicit (and
sometimes non-trivial) assumption that the manager and crew speak the
same language.

Here's another example:


> Go outside, and tell each member of the construction crew "take a
 lunch break, now."


There's a quoted program in this case, but it's crucial to ask other
people to run the quoted program, instead of just taking the lunch
break yourself. That is, `eval` is really necessary. The implementor
of this program takes on the burden of making sure that the
instructions are in a suitable language, however, and may need to
parameterize the quoted program by an explicit action to translate it
to a language understood by the construction crew.

Here's one more reasonable example:


> Ask the construction manager for instructions. Follow them.


In this case, it's the construction manager's problem to give you
instructions in a language that you understand.

Here's a questionable example:


> Decide how long to work before lunch, say N hours, and write a note
 to yourself to work N hours. Add to the note by telling yourself to
 take a lunch break afterward.


If you could really write that program without quotes, then it's
probably ok. The example is misleading, though, because languages
don't usually support


> write a note to yourself to work N hours


You'd have to write instead


> write a note to yourself that says "work" followed by the number N
 and then "hours"


and the quote marks are where the problem comes in. If you translate
the program to Chinese, then you have to be careful to somehow
translate "work" and "hours" to Chinese, too.

The point here is not that programs without quoted text are clearly
good or that programs with quoted text are clearly bad. The real point
is that a programmer has to be especially careful about passing around
instructions and using quoted instructions. Using `eval` means
accepting the burden of using instructions will make sense by the time
they are delivered. That burdened is best avoided, which is why
experienced programmers avoid `eval`, but some of the examples
illustrate cases where the burden is not avoidable or where the
actions enabled by `eval` make the burden worthwhile.


Using `eval` in Racket
--

In the context of Racket, the multiple-language analogy is relatively
accurate, because Racket is about having many programming languages
work together and allowing programmers to define ever better languages
and language constructs. In Racket, it's especially likely that a
library written in one language is used in a context where another
language is the default.

Newcomers to Racket sometimes stumble over the fact that

```racket
 #lang racket
 (define my-x 1)
 (eval '(+ my-x 2))
```

or even

```racket
 #lang racket
 (eval '(+ 1 2))
```

does not work at all, and yet if the program

```racket
 #lang racket
 (define my-x 1)
```

is loaded into a read-eval-print loop--for example, by clicking the
"Run" button in DrRacket and then typing into the lower interactions
panel--then

```racket
 (eval '(+ my-x 2))
```

works as expected.

DrRacket's interactions window has to use `eval` in the sense that it
reads an expression to evaluate and then passes it on to the
interpreter for an answer. More generally, to make various pieces of
the environment fit together, DrRacket sets `eval` globally to use the
module's language while evaluating expressions in the interactions
window. In Racket terminology, DrRacket sets the `current-namespace`
parameter to the module's namespace when it initializes the
interactions window. In contrast, while the module body is being
evaluated, `eval` treats expressions as being in the language that is
empty by default, which is why `eval` during the module evaluation
produces a different result from `eval` during the interactions
windows.

You may wonder why DrRacket doesn't initialize the namespace of `eval`
to be the module's namespace from the start, so that in-module uses of
`eval` and the interactions window behave the same. In a program that
is implemented by multiple modules, which module's language should be
used? In particular, if the language it's always the main module's
language, then a module may behave differently on its own than as part
of a larger program. In the process of developing Racket and DrRacket,
we've seen many such problems, and so Racket now arranges for the
default language to be empty (which is different from any useful
language) to help programmers remember that there's a language issue
to consider whenever `eval` is used.

The Racket Guide's chapter 15 covers in more depth the issues and
namespace tools of Racket for harnessing the power of `eval`:


 [http://docs.racket-lang.org/guide/reflection.html](http://docs.racket-lang.org/guide/reflection.html)


Think of `eval` as a power tool. For some tasks,
there's no real substitute, and so we want `eval`
around. At the same time, `eval` should be used with care. In
dynamic languages generally, that means a reluctant and targeted use
`eval`. In Racket specifically, it means knowing the namespace toolbox
and being as explicit as possible about the intended context for
dynamic evaluation.

<!-- more -->



* * *

Coming from a security analysis background, I appreciate the arguments about analyzability.

You might be interested in my related response to "What do you wish language designers paid attention to?". The bit excerpted below touches on the same points -- quoting confusion is hard, eval can be bounded, the environment in which the evaled code should be run is often not the environment in which eval is invoked.  

"""

Please make your language analyzable/auditable for computer security people.

...

Limit the authority of embedded scripting languages

A lot of useful systems are organized as a static core that kicks off a lot of code written in dynamic (even functional) languages.

And embedding scripting languages can make a system much more extensible.

But a scripting language shouldn't have the full authority of the VM.

If you choose to allow embedded scripting languages, make it easy for the invoker to limit what they can do. An object-capabilities model (see comment on Newspeak above) is very appropriate here ; so when evaluating code in a scripting language, the caller should pass in the code to execute and all the global variables for that code.

Treat eval as a language embedding itself as a scripting language

If your language can invoke its own compiler to turn a string into code, then allow it to be sandboxed the same as you would any embedded scripting language.

...

Don't encourage quoting confusion

"""

cheers

— *Mike Samuel, 19 October 2011*

* * *

Typo:

I think there's a problem with the following sentence:

Using eval means accepting the burden of using instructions will make sense by the time they are delivered

Perhaps the word "that" between "instructions" and "will"?


— *Hendrik Boom, 27 April 2014*

* * *

