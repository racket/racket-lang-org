    Title: Completing Racket's relicensing effort
    Date: 2019-11-15T15:30:43
    Tags: 

*posted by Sam Tobin-Hochstadt, with help from Sage Gerard and Joel
Dueck and Matthew Flatt and the Software Freedom Conservancy,
especially Pamela Chestek*

With the upcoming Racket 7.5 release, almost all of Racket, including
the core Racket CS binary, the standard library, and the packages
provided with the main distribution, are available under a permissive
license, either the Apache 2.0 License or the MIT License. You can
read the details of the new license [in the GitHub
repository](https://github.com/racket/racket/blob/master/LICENSE). This
has been a long process, beginning in 2017, and we're grateful to all
the contributors to Racket, including those from very long ago, who
gave permission for the re-licensing. More than 350 contributors to
Racket responded; many of the responses can be seen in [this GitHub
issue](https://github.com/racket/racket/issues/1570).

<!-- more -->


## Why relicense?

Racket has long been a part of the free software community, and we
value both that community and the ability to build on other free
software. However, two factors mean have made our previous license,
the GNU LGPL, less of a good fit going forward. First, it is unclear
how to apply the LGPL's statement about dynamic linking to a language
like Racket, where macro expansion can copy code from libraries to
applications, and where applications are typically bundled with the
Racket runtime and libraries. Second, some organizations unfortunately
are unwilling to use software licensed under any variant of the
GPL. Since we want to provide a clear license and to promote the use
of Racket everywhere, a new, more-permissive license is the right
choice for Racket.

## What is available under the new license?

In the soon-to-be-released Racket 7.5, the Racket CS binary, and any
Racket code in the Minimal Racket distribution, is available under
both the Apache 2.0 and MIT licenses. Additionally, almost all of the
packages distributed with the full Racket distribution are available
under these licenses. The exceptions are
["slatex"](https://github.com/racket/slatex/blob/master/LICENSE),
["srfi-doc"](https://github.com/racket/srfi/blob/master/LICENSE),
["swindle"](https://github.com/racket/swindle/blob/master/LICENSE),
["r5rs-doc"](https://github.com/racket/r5rs/blob/master/LICENSE),
["r6rs-doc"](https://github.com/racket/r6rs/blob/master/LICENSE), and
["r6rs-std-doc"](https://github.com/racket/r6rs/blob/master/LICENSE)
as well as "string-constants-lib" which is discussed below; see the
LICENSE files available in those individual packages for details.  The
traditional Racket binary itself, as well as other executables such as
the DrRacket binary, continues to include code from several libraries
distributed under the LGPL and is thus still distributed with that
license; we do not anticipate this changing.

Additionally, packages that include the compiled version of [native
libraries](https://github.com/racket/libs/#license) such as Gtk which
themselves are licensed under the LGPL retain that license.

Some packages did not complete the re-licensing process until after
the release process for Racket 7.5 began; those packages will ship
with new licenses in Racket 7.6.

## Is there anything remaining to re-license?

We've completed the re-licensing for all code that we planned to
cover. However, some translations (part of the ["string-constants-lib"
package](https://github.com/racket/string-constants/blob/master/LICENSE))
have authors that are unknown or have not responded to us. This
does not affect the licensing of any programs using Racket unless they
depend on or derive from those translations directly.

## Did anyone say no?

Two people declined to re-license their contributions to Racket. We
therefore removed their contributions and, where appropriate, replaced
them with new code and/or documentation.


