#lang scribble/manual

Title: Racket is 25
Date: 2020-05-10T00:00:00
Tags:

@italic{posted by Matthias Felleisen and Matthew Flatt}

@section[#:style 'unnumbered]{The First Thoughts [Matthias]}

@subsection[#:style 'unnumbered]{Wednesday 25 January 1995}

POPL used to be a Monday-through-Wednesday-noon affair. Cormac Flanagan
presented his @racket[future] paper there (a static analysis on how to
eliminate implicit touch operations; there was also the infamous
Felleisen-Wadler paper). Wednesday night, Cormac and I were flying back
to Houston. On this flight back Cormac asked a seemingly simple
question:
@;
@nested[#:style 'inset]{If functional programming is that good, why is
nobody using it?}
@;
We spent the entire flight discussing this from various angles. My
experience with helping my son's algebra teacher and, years earlier, a
baby sitter with algebra homework, pushed me in the direction of
``algebra is functional programming and we can make a huge difference
by bringing math alive.'' We arrived late in the evening, and when I got
home, I sent email that we’d have a meeting next morning.

@subsection[#:style 'unnumbered]{Thursday 26 January 1995}

We all met in Corky Cartwright’s office because it was bigger than
mine. I announced that I wanted to leave theory behind and build a
curriculum, a programming language, and support software to use
functional programming for teaching algebra, more math, and
programming in K-12 schools.

Shriram embraced the idea on the spot. ``I have always wanted to be
involved in such a project.'' Bruce loved the chance to design a new
language. Cormac made clear he'd help but he really wanted a
dissertation. Matthew was quiet. Corky later dismissed the idea. ``Make
sure you have path back when it fails.''

We discussed for quite a while and came to two conclusions: the PhD
students needed to focus on software issues for dissertations in case
we'd fail. Bruce and I would focus on the language and the
curriculum.

@subsection[#:style 'unnumbered]{Friday 27 January 1995}

Bruce and I started designing a language called Jam. It was supposed
to be simple, Scheme-ish, and without parentheses because we were
convinced that teachers would not like the parenthetical notation.  At
the same time, we were sure that the syntax should be @emph{basically}
``parenthetically simple.'' But most importantly, we wanted to have
image-like parts in this language so that Sum, Integral, Indexing, and
so on would look as much as possible like the math text books.

In parallel, all of us discussed that we'd use Scheme to build this
software ecosystem.

@subsection[#:style 'unnumbered]{Saturday 28 January 1995 }

A few weeks later I found out that over this weekend Matthew had
started to ``cobbled together MrEd.'' I am sure he didn’t use
``cobble'' but something close. And it probably took a bit more than
this weekend. What I am pretty sure about is that the first key
strokes of conception probably took place on that day
(cross-producting my memory with my understanding of Matthew).

@; -----------------------------------------------------------------------------
@section[#:style 'unnumbered]{The First Keystrokes [Matthew]}

I don't remember the different meetings. Maybe it was the second
meeting where I suggested the current user interface---Emacs and the
command line---was the biggest obstacle for students.

In any case, I set out to build a user-friendly environment. The main
problem, it seemed at the time, was to build a GUI text editor that
could handle modern entities like pictures and live objects. I picked
wxWindows as a starting point, because it seemed like the most
promising cross-platform GUI library, and libscheme as the Scheme
implementation, because it was easy to embed. I figured
that the hard part of a text editor was making it run fast enough, so I
wrote that part in C++.

The program was called ``MrEd'' because it was mostly an editor. The
name ``ed'' was already taken. Among the words you get by adding a letter
to the front, only ``red'' was appealing for whatever reason, but that
name was also taken (as ``restricted ed''). Adding one more letter arrived
at ``mred.'' I liked the ``Mr'' part and didn't mind that it was already
the name of a talking horse.

Although the editor core was in C++, I at least knew enough or had
enough direction from the group to aim for Scheme for the rest of the
environment's implementation. So, libscheme became not only the vehicle
for running student programs, but also the language for implementing
much of the GUI itself. To make that practical, I started changing
libscheme: adding a built-in object system that supports extension of
C++ classes, adding compilation to an AST instead of interpreting
S-expressions directly, and so on. After a few months, I became
attached enough to this part of the implementation to break it out as a
separate piece. I called it "Ms. Scheme", but since "Ms" seemed too
likely to suggest "Microsoft", spelled it "MzScheme". (Around 2001, we
started using the umbrella name "PLT Scheme" and moved away from
adopting cute names for subsystems. "PLT Scheme" became "Racket" in
2010.)

That's why the oldest entry for @hyperlink["https://github.com/racket/gui/blob/master/gui-lib/mred/HISTORY.txt"]{@tt{HISTORY.txt}} in the @tt{gui-lib} package
is ``Version 0.7: May 10, 1995,'' while the oldest entry in @hyperlink["https://github.com/racket/racket/blob/master/racket/collects/racket/HISTORY.txt"]{@tt{HISTORY.txt}}
for the @tt{racket} collection is ``Verion 0.27: September 26, 1995.'' The
editor came first, and the language was something of an afterthought.

Of course, the language quickly came to dominate the idea---if not
the day-to-day work, which for several years was still mostly about
getting cross-platform GUI and drawing toolkit to run well. Given that
the language became the main line of the project, it's easy to see
choices that would have been a better starting point, even in 1995. But
it turned out okay.

@; -----------------------------------------------------------------------------
@section[#:style 'unnumbered]{The First Users [Matthias]}

By summer '95, I had designed the basics of what is now known as the
@hyperlink["https://course.ccs.neu.edu/cs2500/design_recipe.html"]{design recipe}
and a few days worth of teaching material. We recruited
four teachers and one of Corky's PhD candidates to test this material
with a week-long workshop.

I started with Chez Scheme and Emacs. 

During the week, Matthew demoed a rudimentary IDE based on MrEd. I
was suitably impressed and tried MrEd on
the four participants, with simple images and all. It didn't go too
well, but we had the first four innocent users try out our very first
teaching environment.

And that’s how Racket launched.
