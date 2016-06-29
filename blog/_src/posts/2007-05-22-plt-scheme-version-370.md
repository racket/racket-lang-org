
    Title:PLT Scheme version 370
    Date:2007-05-22T02:38:00.000-04:00
    Tags:

*posted by Jens Axel Søgaard*

PLT Scheme version 370 is now available from

[http://download.plt-scheme.org/](http://download.plt-scheme.org/)


Some highlights: 


* The conservative garbage collector (CGC) has been replaced with a
precise garbage collector (3m) in the standard build. For most
users, this change simply amounts to "better performance in space
and time". For example, a long-running DrScheme instance typically
uses much less memory than before.

The new memory manager also supports a new "Limit Memory..." option
(in DrScheme's "Scheme" menu) to limit the memory use of a
programming running inside DrScheme.

For those who work with C-implemented libraries and extensions, the
switch to precise collection may complicate interoperability. To a
large extent, however, (lib "foreign.ss") works the same with both
collectors. (But note that the 3m is a moving collector, so be
careful with passing Scheme objects to C.)

Although our pre-built binaries use the new collector, builds from
source using the conservative collector are still supported.


* For a program written with one of the the ["How to Design Programs"](http://www.htdp.org)
(HtDP) languages, DrScheme saves the program with meta-information
that identifies the language and records the teachpacks used by the
program. DrScheme's teachpack GUI now works only with the HtDP
languages. In other languages, use require to access teachpacks.

The meta-information is in the form of a reader extension that turns
the file content into a module-based program, which means that
teaching-language files can be loaded directly into MzScheme or used
with other PLT Scheme tools.


* The HtDP "world.ss" and "image.ss" teachpacks have been revised,
including support for the creation of animated GIFs.


* Unit-based servlets are no longer supported in the web server. Use
module-based servlets, instead. (Servlets can be implemented using
a unit within a module, but the web server's API is provided
through a module.)


* A new (lib "unit.ss") library replaces the old one and provides a
simpler and more flexible syntax. The (lib "unitsig.ss") library
is deprecated but still available as (lib "unitsig200.ss"), and
the old (lib "unit.ss") is available as (lib "unit200.ss"). 
Feedback Welcome, 
_The PLT Scheme Team
_

<!-- more -->



* * *

The reference documentation is wonderful, but where can I go to find out the changes (and the reasoning behind them) to unit.ss? Mailing list discussions would be perfect, if they exist. Thanks for any help!

— *charmless, 22 May 2007*

* * *

Chapters 4, 5, and 6 of my Ph.D. dissertation describe the new unit system, and my rational behind its various features.  However, it doesn't really describe the old system or attempt a comparison between the new and old systems.  Also, Matthew has made a few small syntactic changes from what is described in my dissertation for the v370 system.

— *Scott Owens, 23 May 2007*

* * *

