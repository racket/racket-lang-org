    Title: Tutorial: Creating a Package
    Date: 2017-10-11T15:46:21
    Tags: 

*posted by Stephen Chang*

This post is a summary of a tutorial presented at [RacketCon
2017](http://con.racket-lang.org/2017/).

It describes how to create a package starting from a single Racket file.

Specifically, this post explains how to:

- use `raco pkg install` to install a package,
- create an `info.rkt` file containing package metadata, and
- add a package to
  [Racket's official package catalog](https://pkgs.racket-lang.org).

<!-- more -->

## Packages vs Collections

Before getting started, we first clarify some terminology.
([This blog post][blog-pkgs] explains packages and collections in more depth.)

In Racket, a library is just a [module][doc-module] (typically in its own file)
that exports some bindings. A programmer may import such a module using a
(relative or absolute) filepath:

```racket
(require "path/to/filename.rkt")
```

A Racket [**collection**][doc-collect] is a "hierarchical group of modules"
that are *installed*, i.e., their source files are copied to a standard
location that Racket manages. This allows programmers to use them without
worrying about their location on disk:

```racket
(require scribble)
(require data/queue)
```

Finally, a [**package**][doc-package] is Racket's mechanism for organizing and
distributing collections (or parts of collections). A package may contain
multiple modules spanning multiple collections. The [official Racket package
catalog](https://pkgs.racket-lang.org) is a popular source of packages, but
github repos or even local directories may act as packages as well.

[doc-module]: http://docs.racket-lang.org/guide/module-basics.html
[doc-collect]: http://docs.racket-lang.org/guide/module-basics.html#%28tech._collection%29
[doc-package]: http://docs.racket-lang.org/pkg/Package_Concepts.html?#%28tech._package%29
[blog-pkgs]: http://blog.racket-lang.org/2015/08/modules-packages-and-collections.html

## Local Package Installation

It's best to follow this tutorial with a running example. We'll use [this
one][repo]. To start, `clone` the repo with `git` and `cd` to the repo root
directory.

The rest of the tutorial assumes all files are in a `plot-bestfit/`
directory.

The [`bestfit.rkt` file][repo-bestfit] in our library computes best-fit lines
for Racket's [`plot`][doc-plot] library.

A [`test-plot.rkt` file][repo-testplot] (in the same directory) uses the
`plot`, `math`, and `bestfit.rkt` libraries, importing them like this:

```racket
;; test-plot.rkt
(require plot
	 math
	 "bestfit.rkt")
```

Having to specify a file path in order to use `bestfit.rkt` is somewhat
brittle, however, since it depends on the exact location of the file on
disk. Instead, we can install our library as a collection:

```bash
# run from `plot-bestfit/` directory
$ raco pkg install
```

Running `raco pkg install` as described above installs the contents of the
current directory as a single-collection package, using the directory name as
the name of both the package and the collection.

Alternatively, we can give `raco pkg install` an explicit path. The following
command, executed from the parent directory, is identical to the above command:

```bash
# run from parent of `plot-bestfit/` directory
# you may need to run `raco pkg remove plot-bestfit` before trying this command
# (don't forget the `/`! omitting it will install from the pkg server instead)
$ raco pkg install plot-bestfit/
```

After installing the package/collection, `test-plot.rkt` may use a
collection path instead of a file path to import `bestfit.rkt`:

```racket
;; test-plot.rkt
(require plot
	 math
	 plot-bestfit/bestfit)
```

The require path is still somewhat cumbersome, however, considering that our
entire package consists of a single file. To shorten the path that programmers
must write, we may take advantage of the fact that [`require` implicitly looks
for a `main` module][doc-mainmod]. Specifically, we add (in the `plot-bestfit/`
directory) a `main.rkt` file with contents:

```racket
;; main.rkt
#lang racket
(require "bestfit.rkt")
(provide (all-from-out "bestfit.rkt"))
```

Now we may shorten the requires to:

```racket
;; test-plot.rkt
(require plot
	 math
	 plot-bestfit)
```

The name of the collection, however, was chosen automatically to match the
name of the directory we happened to put our files in. In the next section,
we'll show how programmers can more directly specify package metadata such as
the collection name.

[repo]: https://github.com/stchang/plot-bestfit/tree/racketcon2017-tutorial
[repo-bestfit]: https://github.com/stchang/plot-bestfit/blob/racketcon2017-tutorial/bestfit.rkt
[repo-testplot]: https://github.com/stchang/plot-bestfit/blob/racketcon2017-tutorial/test-plot.rkt
[doc-mainmod]: http://docs.racket-lang.org/guide/module-basics.html#%28part._.Library_.Collections%29
[doc-plot]: http://docs.racket-lang.org/plot/index.html

## Specifying Package Metadata

The Racket package system looks for metadata in an [`info.rkt` file][doc-info],
if one exists. Let's create one for our package. Specifically, we add (in the
`plot-bestfit/` directory) an `info.rkt` file with contents:

```racket
;; info.rkt
#lang info
(define collection "bestfit")
```

This directs Racket to name our collection `bestfit`, instead of using the
directory name (the package name will still be the directory name). Thus if we
re-install our package:

```bash
# remove old installation
$ raco pkg remove plot-bestfit
# re-install, from the `plot-bestfit/` directory
$ raco pkg install
```

we may import the collection with the new name (note that `test-plot.rkt` may
emit an eror during the `raco pkg install` above if it still uses the old
collection name):

```racket
;; test-plot.rkt
(require plot
	 math
	 bestfit)
```

[doc-info]: http://docs.racket-lang.org/pkg/metadata.html

### Specifying Dependencies

Another useful `info.rkt` field is `deps`, which specifies other packages on
which our package depends. During package installation, Racket will
automatically ask to additionally install any such dependencies.

We could add the dependencies ourselves, but an easier way is to use `raco
setup`:

```bash
$ raco setup --fix-pkg-deps bestfit
```

After having `raco setup` repair the `deps`, our `info.rkt` looks like:

```racket
#lang info
(define collection "bestfit")
(define deps '("base"
               "math-lib"
               "plot-gui-lib"
               "plot-lib"
               "typed-racket-lib"))
```

### Specifying Docs

Package installation additionally looks to compile and register documentation
for the package. A `scribblings` field in `info.rkt` points to the
documentation source file.

For example, we might add to our `info.rkt`:


```racket
;; info.rkt
(define scribblings '(("scribblings/bestfit.scrbl")))
```

Formally, the [`scribblings` entry][doc-scribblings] is a list of lists, where
each sublist begins with a documentation source filename and is followed by
various options. In our `info.rkt` file, we have one sublist that contains only
the documentation source file and does not specify any other options.

See the [notes from the "Scribbling documentation" tutorial](https://gist.github.com/florence/b3fcc1df922008604e64362484dc1c28) to learn how to write documentation.

When we are done writing our docs, we can use the `raco` tools to compile and
view them in rendered form.

```bash
# re-compiles `bestfit` collection and its docs
$ raco setup bestfit
# launch browser to view local docs
$ raco docs
```

## `raco pkg new`

When creating a new package, a convenient way to generate stubs for all the
files described in this tutorial (and more) is to run:

```bash
# produces directory `my-pkg/` which contains the file stubs
$ raco pkg new my-pkg
```

## The Racket Package Server

At this point, assume that we've created all the files described in this
tutorial and we have pushed them to a github repo. We will use this repo as an
example: <https://github.com/stchang/plot-bestfit>.

To distribute our package where others may discover and download it, we can add
the package to the Racket package catalog at <https://pkgs.racket-lang.org/>.

To add a package to the catalog:

1. "register" for an account,
2. "sign in" to the account,
3. click the "add your own" button on the front page,
4. and supply the requested information.

If we named our package "my-bestfit-pkg", then any
Racket user may install our package by running:

```bash
$ raco pkg install my-bestfit-pkg
```

Running the above command will look up the package repo from the package
catalog, and then download and install the package source files from that repo.

## Single- vs Multi-Collection Packages

The package we created in this tutorial consists of a single
collection. Alternatively, packages may contain several modules spanning
multiple collections.

Such packages must be declared as multi-collection packages by changing the
`collection` entry in `info.rkt` to have value `'multi`. This directs Racket
package installation to treat each subdirectory in the package as its own
collection (or partial collection).

For example, the [`drracket` package][repo-drracket] leverages this
organization style to implement its toolbar. Specifically, it adds a `tool.rkt`
module to many different collections, such as the `macro-debugger` and
`scribble`, in order to access their callback hooks. Observe that in addition
to the package's root `info.rkt`, *each* collection in this kind of package
uses its own `info.rkt` to specify collection-specific information such as
documentation.

Indeed, any package that wishes to add to an existing collection, even if it's
just one collection, should be declared as `'multi`. See the
[`persistent-array` package][repo-persist] for an example.

[repo-drracket]: https://github.com/racket/drracket/tree/master/drracket
[repo-persist]: https://github.com/samth/persistent-array

## A Final Note on Multi-Package Libraries

**NOTE:** Most programmers will not need to worry about this section.

If you've browsed the Racket source files at all, you may have noticed that
many core libraries further subdivide their contents into *several
packages*. The organization is typically arranged as:

- a base package `X` with just an `info.rkt` file,
- an `X-lib` package that contains most of the source files,
- an `X-doc` package with the documentation files,
- and an `X-test` package with the test files.

In this setup, the `info.rkt` in the base `X` package typically specifies
`X-lib` and `X-doc` as dependencies. This division enables users to more finely
manage dependencies, i.e., a programmer may want to use the main package but
may not want to install the tests (and its dependencies).

See the [pict package](https://github.com/racket/pict)
for a concrete example of a library organized in this manner.

[doc-scribblings]: http://docs.racket-lang.org/raco/setup-info.html?#%28idx._%28gentag._11._%28lib._scribblings%2Fraco%2Fraco..scrbl%29%29%29