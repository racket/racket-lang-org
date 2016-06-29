
    Title:Typed Scheme 2.0
    Date:2009-05-25T13:47:00.003-04:00
    Tags:

*posted by Sam Tobin-Hochstadt*

Typed Scheme version 2.0 is now available from SVN.

One persistent limitation of Typed Scheme has been that while this expression works as expected:

`(if (number? x) (add1 x) 7)`

The simple transformation of making `x` a part of a structure breaks Typed Scheme's ability to reason about the code. So this expression doesn't typecheck:

`(if (number? (car x)) (add1 (car x)) 7)`

With the newest version of Typed Scheme, now available in SVN, both of these will now work.  In general, Typed Scheme can now follow paths into arbitrary immutable structures, including pairs.

This is part of a more general reworking of underlying mechanisms of the Typed Scheme typechecker, which makes it both simpler and more flexible.  I hope that it will be possible,  sing this new foundation to add additional features that make more programs easy to express in
Typed Scheme.

Of course, these changes mean that Typed Scheme may be more unstable, so if you notice any new bugs, please let us know.

Unfortunately, this won't be available in the upcoming 4.2 release, but it will be in the release after that.

If you have any questions or comments or feature requests for Typed Scheme, please let us know.