
    Title:Maintaining self-references in Planet packages
    Date:2009-03-12T16:35:00.030-04:00
    Tags:

*posted by Carl Eastlund*


[PLaneT packages](http://docs.plt-scheme.org/planet/index.html) may refer to themselves (i.e. include [module paths](http://docs.plt-scheme.org/guide/module-paths.html) referring to some part of the same package) for a number of reasons. One module may [require](http://docs.plt-scheme.org/reference/require.html#%28form._%28%28lib._scheme/base..ss%29._require%29%29) another. [Scribble](http://docs.plt-scheme.org/scribble/index.html) documentation traces [for-label](http://docs.plt-scheme.org/reference/require.html#%28form._%28%28lib._scheme/base..ss%29._for-label%29%29) imports to construct hypertext links. DrScheme [language levels](http://docs.plt-scheme.org/tools/adding-languages.html) may [provide a module path](http://docs.plt-scheme.org/tools/drscheme_language.html#%28meth._%28%28%28lib._drscheme/tool-lib..ss%29._drscheme%7E3alanguage%7E3asimple-module-based-language%7E3c%7E25%7E3e%29._get-module%29%29) for an initial [namespace](http://docs.plt-scheme.org/guide/eval.html#%28part._namespaces%29).

In each of these cases, we want the module path to refer to the [same version](http://docs.plt-scheme.org/planet/Using_PLaneT.html#%28part._.Fine-.Grained_.Control_.Over_.Package_.Imports%29) of the same package that it occurs in. On the other hand, we do not want to have to manually search and replace the version number every time we update. Before I solved this problem I would often release some new version x.0 of a package, only to find some lingering dependency on y.0 that my search/replace had not caught. Of course, then I had to go back and replace all occurrences of both x.0 and y.0 with x.1 and release again. To avoid this headache, we need a way to express self-referential module paths with precise, implicit version numbers.

The built-in module paths don't quite support this. The relevant forms are PLaneT paths with version numbers, PLaneT paths without version numbers, and relative paths:

```racket
(planet my/package:1:0/dir/file)
(planet my/package/dir/file)
"../dir/file.ss"
```

PLaneT paths with version numbers suffer from the search and replace problem: they become obsolete, and must be changed with every new release.

PLaneT paths without version numbers "upgrade" with a new release: they automatically refer to the latest version of a package.  Unfortunately, this means they aren't really "self"-references.  As soon as version 2.0 is released, every version-free reference to the package refers to 2.0.  Even the references in version 1.0 get implicitly updated, and become forward references rather than self-references.

Relative paths are precise, in that they always refer to the same version of the same package. However, because they implicitly refer to the directory containing the source code, they are only valid within a single file.  They cannot be reliably passed to DrScheme for a language level namespace, traced for documentation links by Scribble, or used by other such external tools.

None of these options provides precise, stable, externally comprehensible, self-referential module paths.

To fill this need, I have released (planet [cce/scheme:4:1/planet](http://planet.plt-scheme.org/package-source/cce/scheme.plt/4/1/planet-docs/main/planet.html)). This module provides PLaneT package authors with several macros that construct references to the current package in require imports, Scribble documentation, and dynamic values.  The self-referential modules paths are constructed automatically at compile time based on the [source location](http://docs.plt-scheme.org/reference/stxops.html) of the macro use and the [local PLaneT package database](http://docs.plt-scheme.org/planet/search-order.html#%28part._.Acceptable_.Local_.Package%29).  Their expanded form always includes an explicit package name and version number (both major and minor).  Here I will summarize their use, with (planet my/package:1:0/dir/file) as a running example.  For full details, see the [online documentation](http://planet.plt-scheme.org/package-source/cce/scheme.plt/4/1/planet-docs/main/index.html) or install the [package](http://planet.plt-scheme.org/display.ss?package=scheme.plt&amp;owner=cce).

To import a module from within a PLaneT package, use the this-package-in require transformer.  To re-export bindings from a module imported this way, use the this-package-out provide transformer, or use require/provide/this-package in place of both.

For example, you might want to import and re-export bindings from dir/file:

```racket
(require (planet my/package:1:0/dir/file))
(provide (all-from-out (planet my/package:1:0/dir/file)))
```

You can leave out the package name and version number, thus making the code invariant across upgrades, by writing:

```racket
(require (this-package-in dir/file))
(provide (this-package-out dir/file))
```

Or, you can further simplify it:

```racket
(require/provide/this-package dir/file)
```

All three of the above are equivalent (in version 1.0).

In Scribble documentation, a module often refers to itself via [defmodule](http://docs.plt-scheme.org/scribble/doc-modules.html#%28form._%28%28lib._scribble/manual..ss%29._defmodule%29%29), [declare-exporting](http://docs.plt-scheme.org/scribble/doc-modules.html#%28form._%28%28lib._scribble/manual..ss%29._declare-exporting%29%29), or [schememodname](http://docs.plt-scheme.org/scribble/scribble_manual_code.html#%28form._%28%28lib._scribble/manual..ss%29._schememodname%29%29). I provide the extensions defmodule/this-package, declare-exporting/this-package, and schememodname/this-package, respectively. These macros allow the user to supply a path such as dir/file, or to omit one to refer to the package as a whole (or its main module). In positions where the original macros allow a sequence of module paths, these forms allow two sequences, one for self-referential module paths and one for other paths.

To document an entire module, one might first write:

```racket
(defmodule (planet my/package:1:0))
```

The automatic self-reference version is simply:

```racket
(defmodule/this-package)
```

In more complicated cases, one might document a sub-part of a package or present bindings from multiple sources:

```racket
(defmodule (planet my/package:1:0/dir/file)
  #:use-sources
  [(planet my/package:1:0/private/impl) somewhere/else])
```

These self-references can still be automated:

```racket
(defmodule/this-package dir/file
  #:use-sources
  [private/impl]
  [somewhere/else])
```

Finally, I provide this-package-version-symbol for constructing PLaneT package symbols as runtime values. This macro is analogous to [this-package-version](http://docs.plt-scheme.org/planet/Utility_Libraries.html#%28form._%28%28lib._planet/util..ss%29._this-package-version%29%29) from the [planet/util](http://docs.plt-scheme.org/planet/Utility_Libraries.html#%28mod-path._planet/util%29) collection, but it constructs a symbol rather than an s-expression. You can use this symbol to construct a module path for a DrScheme language level, or escape it with unsyntax in Scribble's [schemeblock](http://docs.plt-scheme.org/scribble/scribble_manual_code.html#%28form._%28%28lib._scribble/manual..ss%29._schemeblock%29%29) typesetting to create self-referential example code.

This list of utilities may not be complete. Users may need to write new macros for self-referential PLaneT packages. To that end, (planet [cce/scheme:4:1/syntax](http://planet.plt-scheme.org/package-source/cce/scheme.plt/4/1/planet-docs/main/syntax.html)) provides syntax-source-planet-package. This function is analogous to [this-package-version](http://docs.plt-scheme.org/planet/Utility_Libraries.html#%28form._%28%28lib._planet/util..ss%29._this-package-version%29%29), but operates on [syntax objects](http://docs.plt-scheme.org/guide/stx-obj.html) and is designed to be used in [macro transformers](http://docs.plt-scheme.org/guide/proc-macros.html). There are also -owner, -name, -major, -minor, and -symbol versions following the same analogy.

I find these tools useful for maintaining my PLaneT packages, and I hope other authors will too.  If you do give them a try, please send [feedback](http://planet.plt-scheme.org/trac/newticket) on what does or doesn't work, or what might be improved.  I would eventually like to add a refined version to the PLT Scheme collections once we get enough experience to know that these tools are fairly complete and usable.
