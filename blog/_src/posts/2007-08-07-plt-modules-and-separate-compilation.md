
    Title:PLT Modules and Separate Compilation
    Date:2007-08-07T20:41:00.001-04:00
    Tags:

*posted by Richard Cobbe*


For my summer job this year, I'm programming in Common Lisp; this is the
first time I've used the language for anything more than toy examples.  The
experience has given me new appreciation for the PLT module system and how
it enables separate compilation.



Lisp has a package system, of course, but it's not the same thing.  It's
primarily a tool to make sure that the symbols in one part of the program
don't collide with the symbols in another part (unless you ask them to).
Packages aren't about abstraction: while you can specify which symbols are
exported from the package and which aren't, that's just a suggestion that's
not enforced by the language.



(You'll notice, by the way, that I used the word "symbol" and not
"identifier," which is the more common term in the study of programming
languages, in the previous paragraph.  That's deliberate: the Lisp package
system works on symbols, not identifiers, so it also affects quoted,
literal symbols.  In my experience, this is sometimes helpful, sometimes a
real pain, and usually completely unexpected.  But that's a topic for
another post.)



Also, there's no real relationship between Lisp packages and files.  One
package can be spread across multiple files, and one file can contain code
in several different packages.



All this means that separate compilation in Lisp is a real problem.  There
is a system, ASDF, that attempts to address this need.  (For more details,
consult [the closest thing to a
homepage](http://www.cliki.net/asdf) that I could find for ASDF.)  I'm no expert on ASDF, but
essentially the programmer specifies  the dependencies between source
files, in a set of files that exist parallel to the Lisp source.  (ASDF
does support grouping source files into larger chunks and specifying
dependencies between those chunks, but as far as I can tell that's largely
a convenience thing.)



The key thing for separate compilation, of course, is the dependencies.
With ASDF, the programmer specifies those manually, and then ASDF basically
does a topological sort such that if file a depends on file b, then ASDF ensures that a is compiled and loaded before b is compiled, and again before B is loaded.  (This should start sounding a little familiar to folks who've worked in the area where PLT's modules and macros intersect.)



So far, so good.  Unfortunately, there are a couple of problems with this
setup.  First, the dependencies between files are specified outside the language.  This means that,
if you happen to forget one, the results are not well-defined.  If ASDF
happens to choose an order that's consistent with the dependency you left
out, everything will just work, and you won't have any indication that
there's a problem.  If, however, it doesn't, then you'll get random
"undefined function" and "undefined symbol" errors---if you're lucky (at
least in SBCL, the implementation of Common Lisp that I use at my job).  In
PLT, by contrast, inter-module dependencies are part of the language, so
the compiler will always give you
an undefined-identifier error when it tries to compile a module in which
you've forgotten a require form.  Big win, in my opinion (although we
could argue about whether this should be an error or a warning, and whether
the compiler should report lots of errors or just one before giving up
completely).



Second, because ASDF lives outside the compiler, it can't be very smart
about how macros affect separate compilation.  I don't fully understand
this, perhaps because the folks who've been mentoring me at my job haven't
thought it worth the time to explain it to me fully.  But it appears that,
if you change a macro that's used in other files, or change a function
that's called by a macro at expansion time, you have to do
the effect of a make clean in a distressingly large number of cases.
This is a real problem when you've got a large source base (~200K LOC, I
think) and you're trying to speed up builds, as we are, and it's especially problematic if you're trying
to run unrelated parts of the build in parallel.



I've certainly griped about the complexity of the interaction between PLT's
modules and macros in the past.  But after this summer, I have to say it's
awfully nice to have a module system that Just Works for separate
compilation.  Nicely done, Matthew.



(I've pointed the folks at work at Matthew's [ICFP 02 paper](http://www.cs.utah.edu/plt/publications/macromod.pdf), but as that technique requires a lot of support from the compiler, and we don't have the resources to add the necessary support to SBCL ourselves, I don't know that it'll be more than a "wouldn't it be nice if we could do that?")



(Answer to rhetorical question in preceding paragraph: Yes.  Yes it would.) 


<!-- more -->



* * *

What exactly do you mean by random errors? If you get a "undefined function FOO" error that's a pretty clear indication that whatever file defines FOO should be defined as a dependency.

Also, if you wish to speed up compilation by determining precise dependencies between files in your system asdf-dependency-grovel might be a useful little tool.

— *Luís, 7 August 2007*

* * *

I agree about the issue of macros --- ASDF does not allow you to say "this system exports a macro, and if the macro has changed, all downstream systems should be recompiled."  This is left as an exercise to the reader, and I have certainly been caused severe pain by that.

WRT "compiler support," I don't really agree with you.  This is Common Lisp, after all, and I like the fact that ASDF works on SBCL and Allegro Common Lisp, etc.

— *Robert, 16 August 2007*

* * *

Hellow. Could you please advice something like PLT but for SBCL. Sorry for the oftopic.

— *Dmitrenko Evgenij, 23 December 2009*

* * *

