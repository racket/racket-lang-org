
    Title:The Design of Extended Exercises
    Date:2007-12-29T15:49:00.000-05:00
    Tags:

*posted by Shriram Krishnamurthi*


One of the highlights of the TeachScheme! method is to create Extended
Exercises.  Several of these pepper _How to Design
Programs_, and even more have been created since to deal with a
variety of interesting problem scenarios (e.g., illustrating graphics
via t-shirt design, explaining networking by having machines play
roles in a theatrical play, demonstrating communication with foreign
sites by processing data from a microfinance institution, etc).
Through an Extended Exercise a student learns about how computer
science connects to domains, develops practice building programs
incrementally, learns to build earlier assignments that later
assignments can depend on, and so forth.



Here is a preliminary articulation of some principles that I think
govern a good Extended Exercise, with an emphasis on their "form
factor".





*  Pick a domain.  Whether the domain looks inward (a computing
activity such as networking) or outward (such as social networking)
doesn't matter.  If it does look inward, try to make it more
applicable through the judicious use of data (the same exercise can
look very dry or very applied depending on what data you choose).  For
instance, our networking exercises is presented in terms of
Shakespeare's Hamlet.



*  If necessary, write a Teachpack.  A domain almost certainly needs a
Teachpack to reduce the programming burden on students.  For instance,
the microfinance exercise uses a Teachpack to hide the ugly details of
screen-scraping (which in turn need to be constantly updated by a
vigilant maintainer).



*  Try to provide a non-trivial dataset in the Teachpack.  Good data can
make an assignment more enjoyable--e.g., our networking example
provides an excerpt from Act 2, Scene 2 of Hamlet
("What a piece of work is man!")--and in cases where
the exercise depends on connecting to an external site (e.g., the
microfinance example), the data may be essential.



*  Structure the assignment to have five to eight questions: not much
fewer (too few steps) nor much more (too much to grasp).



*  Try to decompose the problem using good principles of stepwise
refinement, using your own wisdom in these matters.  By showing
students several such examples, we hope for them to build up an
intuition for the process.  Your decomposition may not be strictly
linear; that's okay.  But it should be progressive.



*  Perhaps the most important point: At every step, try to have a full,
working application.  That means the Teachpack may need to export
several interfaces, each one taking more parameters and accepting more
functionality than the previous one.  Otherwise the student needs to
have all the parts working before they can understand whether even one
works in context, leading to a frustrating learning experience and
encouraging wanton hacking as they try (and invariably fail) to
quickly get to a working system.



*  Design interfaces carefully to make judicious use of first-class
functions.  It is inevitable that students will need to provide
functions (not just flat data) to what your Teachpack exports.  Show
them the invocations of your Teachpack in terms of named functions
(that they define).



*  Try to provide a few extra-credit routes for ambitious students.
Options include letting students peel back even more of the Teachpack,
or adding interesting features.




<!-- more -->



* * *

So ... which of the existing teachpacks seem closest to your ideal?

— *offby1, 29 December 2007*

* * *

