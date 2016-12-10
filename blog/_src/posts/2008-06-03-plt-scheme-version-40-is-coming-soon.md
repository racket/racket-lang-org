
    Title:PLT Scheme version 4.0 is Coming Soon
    Date:2008-06-03T18:03:00.003-04:00
    Tags:

*posted by Matthew Flatt*

PLT Scheme is now 13 years old. The initial version was little more than glue code between a few open-source libraries, which seemed to offer the quickest solution to our modest goals. Modest success leads to bigger goals, however, and then continued success leads to ever more ambitious goals. Before you know it, a mass of users, co-developers, libraries, and documentation rely on design decisions that were made for a much smaller project years before.

Naturally, many of those early design decisions turn out to be a poor fit for the project's eventual role. Starting from scratch isn't usually practical, so you gradually adjust the infrastructure to meet new needs. That was precisely the story for the version 300 series of releases for PLT Scheme. The biggest gap between our original and current goals was in run-time performance, so we replaced bytecode interpretation with a just-in-time native-code compiler, and we replaced a memory manager based on "conservative" estimates of pointer usage with one that uses precise information.

Performance improves a bit more with version 4.0, but mostly we've moved on to a bigger mismatch between the original and current goals: the way that PLT Scheme presents itself to users. PLT Scheme was originally conceived as R5RS Scheme with some extensions to make it practical, and with useful tools (notably an IDE) and libraries (notably a GUI library) built on that core. Our documentation and web pages reflected that architecture - which now seems completely upside-down.

Version 4.0 is a fresh start in the way that we present PLT Scheme. It's a new language. PLT Scheme is a dialect of Scheme, certainly, but it's not merely a superset of R5RS, R6RS, or other standards, and those standards are not really the best place to start understanding PLT Scheme. At the same time, the unique extensibility features of the PLT Scheme language and tools allow them to support other languages easily, including R5RS (though a new `plt-r5rs` executable), R6RS, and more.

Improvements to the PLT Scheme language include better syntax for modules, better support for optional and keyword function arguments, more expressive syntax for structure types, streamlined hash-table operations, new syntax for list comprehensions and iterations, a more complete and consistent set of list and string operations, and reduced dependence on mutable pairs. To current users of PLT Scheme, these changes will seem like the big ones behind version 4.0, but they're small compared to the overall re-organization and the accompanying documentation effort.

We wrote hundreds of pages of new documentation, including much more tutorial and overview information. We ported hundreds of pages of existing documents to new a system that produces cleaner, better organized, more consistent output. We will replace the old tangle of web pages (that try to explain a confusing federation of tools) with a simple page about "PLT Scheme." We have even streamlined the command-line flags for the main virtual machine.

The development of PLT Scheme version 4.0 took about one year of hard work. In retrospect, that doesn't sound too bad, considering the scale of the existing code base, the number of things that we improved, and the total size of the documentation (about 2000 pages in PDF form). Still, you can imagine how happy we are to arrive at a stable release, and we hope that the improvements in PLT Scheme version 4.0 work as well for everyone else as they do for us.

For a preview, see [http://pre.plt-scheme.org/](http://pre.plt-scheme.org/). The final version 4.0 release is just days away.

<!-- more -->



* * *

Outstanding, I can hardly wait!

JT
http://www.FireMe.To/Udi

— *Brad, 4 June 2008*

* * *

I want it _now_!

— *Daniel, 4 June 2008*

* * *

daniel: the best I can do for now is point you to the pre-releases builds online. But it should be soon!

http://pre.plt-scheme.org/installers/

— *Robby, 4 June 2008*

* * *

Very exciting!  The new documentation especially helped me get started with PLT Scheme.

Thank you!

— *David Vanderson, 4 June 2008*

* * *

Congrats, all!  The re-emergence of Scheme in programming courses is in large part thanks to your efforts.  Thank you!

— *Duane Johnson, 4 June 2008*

* * *

Thanks, 

I was waiting for something fresh, I've found it.

— *kib, 4 June 2008*

* * *

I already find DrScheme good enough for my needs. All that I wish to see is a good tutorial (like the one the Python guys have put together).

The lack of a good "official" tutorial, coupled with the difficulty one encounters reading online versions of printed books (SICP), is keeping me from using Scheme as a primary development language (I'm a hobbyist programmer, and barely have enough resources to buy books etc.)

— *General Maximus, 8 June 2008*

* * *

General Maximus: have you had a change to look at the v4.0 tutorials yet? They're at the link in the post (and soon will be on the main website).

— *Robby, 8 June 2008*

* * *

You mean this one, right? :
http://pre.plt-scheme.org/docs/html/guide/index.html

I didn't look at it before. Looks pretty nice. Since I don't know much about Scheme, can you tell me how it is as an introductory tutorial?

— *General Maximus, 8 June 2008*

* * *

That one is the one that covers the most ground and definitely should be useful, but if you're new to Scheme I would start with the two much shorter tutorials (below) and move to that one as you need more specific information for some particular task.

http://pre.plt-scheme.org/docs/html/quick/

http://pre.plt-scheme.org/docs/html/more/

— *Robby, 8 June 2008*

* * *

I only see the 3.99.0.26 pre-release up on http://pre.plt-scheme.org/installers/.

Are the 4.0 pre-releases up somewhere?

— *databus, 8 June 2008*

* * *

3.99.x is the pre-release for 4.0.

— *Robby, 8 June 2008*

* * *

Great, thanks Robby.

— *databus, 8 June 2008*

* * *

