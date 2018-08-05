    Title: Tutorial: Contributing to Racket
    Date: 2017-09-27T01:07:20
    Tags: 

*posted by Ben Greenman, with help from Matthew Butterick and Robby Findler and Jack Firth and Vincent St-Amour and Sam Tobin-Hochstadt*

This post describes 3 ways to contribute to Racket:
 (1) fixing a typo online,
 (2) submitting a pull request to the [`racket/racket`](https://github.com/racket/racket) repository, and
 (3) submitting a pull request to a repository in the [Racket GitHub organization](https://github.com/racket).

<!-- more -->

The source code for Racket is on GitHub.
Everything that comes with [`#lang racket`](http://docs.racket-lang.org/reference/index.html)
 is online at <https://github.com/racket/racket>.
This includes the [compiler](https://github.com/racket/racket/tree/master/racket/src),
 the [raco](https://github.com/racket/racket/tree/master/racket/collects/raco) command-line tool,
 and the libraries for basic datatypes.
Other packages included in the [main distribution](https://pkgd.racket-lang.org/pkgn/search?q=main-distribution)
 are online in repositories owned by the [`racket` GitHub organization](https://github.com/racket).
These repos include [`racket/htdp`](https://github.com/racket/htdp),
 [`racket/rackunit`](https://github.com/racket/rackunit), and
 [`racket/typed-racket`](https://github.com/racket/typed-racket).

Anyone with a [GitHub account](https://help.github.com/articles/signing-up-for-a-new-github-account/)
 can contribute to Racket by [forking a repository](https://help.github.com/articles/fork-a-repo/)
 and [submitting a pull request](https://help.github.com/articles/creating-a-pull-request/).
To help streamline the process for potential contributors,
 this post explains how to submit a quality pull request for three kinds of tasks:

1. Fixing a typo
2. Contributing to the [`racket`](https://github.com/racket/racket) language
3. Contributing to a [main distribution](https://pkgd.racket-lang.org/pkgn/search?q=main-distribution) package

If you have any trouble following along, ask for help via
 [email](https://groups.google.com/forum/#!forum/racket-users),
 [IRC](https://botbot.me/freenode/racket/),
 and/or [Slack](http://racket-slack.herokuapp.com/).

> For a Git crash course, see: <http://rogerdudler.github.io/git-guide/>

> See also: [Contributing to Racket](http://blog.racket-lang.org/2012/11/tutorial-contributing-to-racket.html) (the 2012 edition)
> and [Guide for Infrequent Contributors](http://www.greghendershott.com/2013/04/a-guide-for-infrequent-contributors-to-racket.html)


## How to fix a typo

If you find a typo in the [Racket documentation](https://docs.racket-lang.org),
 there's an "easy" way to submit a fix:

1. Find the documentation source on GitHub
2. Click the "Edit this File" button (looks like a pencil: <a href="https://github.com/racket/racket-lang-org/blob/master/blog/_src/posts/2017-09-27-tutorial-contributing-to-racket.md"><img src="/img/edit-this-file-github-button.png" title="Edit this File" alt="'Edit this File' button"/></a>)
3. Use GitHub to fork the repo and submit a pull request

The good news is that GitHub makes it easy to create the pull request.
The bad news is that it can be difficult to find the right file to edit.


### Hints for finding documentation

The source for [The Racket Reference](http://docs.racket-lang.org/reference/index.html) is here:

<https://github.com/racket/racket/tree/master/pkgs/racket-doc/scribblings/reference>

The source for [The Racket Guide](http://docs.racket-lang.org/guide/index.html) is here:

<https://github.com/racket/racket/tree/master/pkgs/racket-doc/scribblings/guide>

To find the documentation for a main distribution package `<PKG>`:
  (1) go to `github.com/racket/<PKG>`,
  (2) open the top-level folder named `<PKG>-doc/`, and
  (3) search for a sub-folder (or sub-sub-folder) named `scribblings/`.

Clicking any section header in the docs will display a module path that
  should be close to the documentation's actual source.
For example, [The Redex Reference](http://docs.racket-lang.org/redex/The_Redex_Reference.html)
 has a section titled "Patterns".
Clicking the section header reveals the module path [`"redex/redex.scrbl"`](https://github.com/racket/redex?path=redex-doc/redex/redex.scrbl).
This module **does not** contain the documentation on "Patterns", but
 it includes documentation from the relative path [`"scribblings/ref.scrbl"`](https://github.com/racket/redex?path=redex-doc/redex/scribblings/ref.scrbl)
 which includes documentation from the relative path [`"ref/patterns.scrbl"`](https://github.com/racket/redex?path=redex-doc/redex/scribblings/ref/patterns.scrbl).
That last file contains the documentation on patterns.

> You are in a mazy of twisty little module paths, all alike.

See [Section 18.2](http://docs.racket-lang.org/reference/collects.html) of The Racket Reference for more about module paths.

If this sounds like too much work, an alternative is to (1) clone the repo,
 (2) use your operating system (e.g., `grep`) to find the right file, and
 (3) go back to GitHub to do the edit and submit a pull request.


## How to contribute to the `racket` language

If you find a bug in [`racket/list`](https://github.com/racket/racket/blob/master/racket/collects/racket/list.rkt),
 or want to add an example to the documentation for [`racket/class`](https://github.com/racket/racket/blob/master/racket/collects/racket/class.rkt),
 or think of a useful addition to [`racket/logging`](https://github.com/racket/racket/blob/master/racket/collects/racket/logging.rkt),
 then you will want to install Racket from source to create a pull request.


### Step 1. Install from source

To install Racket from source, (1) clone the [`racket/racket`](https://github.com/racket/racket) repository
 and (2) run its Makefile.

```
$ git clone https://github.com/racket/racket
$ cd racket
$ make
```
<br/>

> Estimated time: 3 hours

To build the Minimal Racket, run `make base` instead of `make`.

To build the Minimal Racket and test suite, run `make PKGS=racket-test`.


### Step 2. Create a fork on GitHub

The next step is [to fork](https://help.github.com/articles/fork-a-repo/)
 the [`racket/racket`](https://github.com/racket/racket) repo on GitHub and
 add the fork as a [remote](https://help.github.com/articles/adding-a-remote/).

```
# Inside the newly-cloned `racket/` directory ...
$ git remote add fork https://github.com/<YOUR-USERNAME>/racket
```
<br/>


### Step 3. Create a pull request

From here you can make a branch for your changes,
 do the edits, and push to your fork.

```
$ git checkout -b my-edits
# do edits
$ git commit
$ git push fork my-edits
```
<br/>

Once `git push` finishes, visit <https://github.com/racket/racket>
If you are logged-in, there will be a green button to "Compare & pull request".

<img src="/img/compare-and-pull-request-github-button.png" title="Compare & pull request" alt="'Compare & pull request' button"/>

Click it, and GitHub will show a diff between the pushed branch on your
 fork and the `master` branch on the [`racket/racket`](https://github.com/racket/racket) repo.


### Hints for navigating the `racket/racket` repo

Built-in [collections](http://docs.racket-lang.org/guide/module-basics.html?q=collect#%28tech._collection%29)
 are in the folder [`racket/collects`]().
For example, [`data`](https://github.com/racket/racket/tree/master/racket/collects/data)
 [`json`](https://github.com/racket/racket/tree/master/racket/collects/json),
 [`net`](https://github.com/racket/racket/tree/master/racket/collects/net), and
 [`syntax`](https://github.com/racket/racket/tree/master/racket/collects/syntax).

The source for [`#lang racket`](http://docs.racket-lang.org/reference/index.html)
 is in the folder [`racket/collects/racket`](https://github.com/racket/racket?path=racket/collects/racket).
This includes the [reader](https://github.com/racket/racket?path=racket/collects/racket/lang/reader.rkt),
 the [`racket/match`](https://github.com/racket/racket?path=racket/collects/racket/match) package, and
 the [`racket/cmdline`](https://github.com/racket/racket?path=racket/collects/racket/cmdline.rkt) library.

The source for the compiler is in [`racket/src`](https://github.com/racket/racket?path=racket/src).

Tests for the [`racket`](http://docs.racket-lang.org/reference/index.html) language may be under the:
  [`racket-test-core/`](https://github.com/racket/racket/tree/master/pkgs/racket-test-core),
  [`racket-test/`](https://github.com/racket/racket/tree/master/pkgs/racket-test), or
  [`racket-test-extra/`](https://github.com/racket/racket/tree/master/pkgs/racket-test-extra)
  directories.


### Hints for compiling and testing

To recompile everything and rebuild the documentation, run [`raco setup`](http://docs.racket-lang.org/raco/setup.html) using
 the `raco` executable from the cloned repository.
To run the core suite of unit tests, run [`raco test -l tests/racket/test`](http://docs.racket-lang.org/raco/test.html?q=raco%20setup).

```
# From the root of the racket repo ...
$ ./racket/bin/raco setup
$ ./racket/bin/raco test -l tests/racket/test
```
<br/>

> Estimated time for `setup`: 0-3 hours

> Estimated time for `test`: 1 hour


## How to contribute to a main distribution package

The Racket [main distribution](https://pkgd.racket-lang.org/pkgn/search?q=main-distribution)
 includes many packages, such as:
 [`games`](https://github.com/racket/games),
 [`drracket`](https://github.com/racket/drracket),
 [`math`](https://github.com/racket/math),
 [`pict`](https://github.com/racket/pict),
 [`redex`](https://github.com/racket/redex), and
 [`scribble`](https://github.com/racket/scribble).
The source for each package is on GitHub, in a repository owned by the [`racket`](https://github.com/racket/racket)
 organization.

The first step in contributing to a main distribution package is to get its source code.
Depending on how you installed Racket, there are two ways to get the source:


### Step 1a. If you installed Racket from source ...

If you installed Racket from source, the following command makes a clone
 of the repository for the package `<PKG>` in the current directory.

```
$ raco pkg update --clone <PKG>
```
<br/>

> Estimated time: 1 hour

If `raco pkg update --clone` asks whether to clone dependencies, say yes.

The recommended place to store clones is in a directory named `extra-pkgs/`
 at the top level of your clone of `racket/racket`.
This directory is recommended because the `racket` repo [ignores it](https://github.com/racket/racket/blob/master/.gitignore).


### 1b. If you have a pre-compiled version of Racket ...

If you have a compiled version of Racket, i.e. from <https://download.racket-lang.org>,
 the following pair of commands makes a clone of the repository for the
 package `<PKG>` in the current directory:

```
$ raco pkg update --no-setup --catalog https://pkgs.racket-lang.org <PKG>
$ raco pkg update --clone <PKG>
```
<br/>

If `raco pkg update --clone` asks whether to clone dependencies, say yes.

> Estimated time: 1 hour


### 2. Create a fork on GitHub

After installing the package, make sure [to fork](https://help.github.com/articles/fork-a-repo/)
 the `racket/<PKG>` repo and add your fork as a remote:

```
$ git remote add fork https://github.com/<YOUR-USERNAME>/<PKG>
```
<br/>


### 3. Create a pull request

After creating the fork, you can make a new branch, commit changes, push to the fork,
 and create a pull request.

```
$ git checkout -b my-edits
# do edits
$ git commit
$ git push fork my-edits
```
<br/>

Visit GitHub to [create the pull request](https://help.github.com/articles/creating-a-pull-request/).


### Hints for navigating a package

For a typical main distribution package `<PKG>`:

- The implementation is in the directory `<PKG>-lib/<PKG>`
- The docs are in the directory `<PKG>-doc/**/scribblings`
- Unit tests are in the directory `<PKG>-test`
- Some files may have additional tests in a submodule; look for [`(module+ test ....)`](http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29)

DrRacket includes a few navigational tools:

- right-click an identifier, click "Open Defining File"
- right-click a module name, click "Open `<filename>`"
- from the "File" menu, click "Open Require Path" and use the search box

The Emacs [`racket-mode`](https://github.com/greghendershott/racket-mode)
 has similar shortcuts.

The [`raco-find-collection`](https://github.com/takikawa/raco-find-collection)
 package implements a command-line search tool.
After installing, run `raco fc <COLLECTION-PATH>`.

If you're not sure where to add tests,
 write a small program that tests your changes and submit it as a comment to the pull request.
A repo manager can suggest how to work the script into the existing unit test suite.


### Hints for compiling and testing

To recompile the code and documentation:

```
$ raco setup <PKG>
```
<br/>

> Estimated time: 1-5 minutes

To view the documentation:

```
$ raco docs <PKG>
```
<br/>

To run _some_ unit tests:

```
$ raco test -c <PKG>
```
<br/>

The command(s) to run all unit tests depends on the package.
Ask a repo owner, read the package's `.travis.yml` if it exists,
 or check the [Racket release checklist](https://github.com/racket/racket/wiki/Release-Checklist)
 for help running all the `<PKG>` tests.


## Postscript: Life of a pull request

After submitting a pull request, you enter the "LGTM loop".

```
do {
  sleep($a_few_hours);
  if ($changes_requested) {
    make_changes();
  }
  elsif ($one_week_with_no_comments) {
    send_gentle_reminder();
  }
} until ($LGTM);
```

This code is in Perl so you know not to take it too seriously.

- Wait a few days for someone to review the pull request.
  You should receive an email when this happens.
- If a week goes by with no comment, consider sending a quick "ping, any
  thoughts on this?" comment to the pull request.
- If someones comments and requests changes, then make changes in a new commit
  and push to your branch. GitHub will detect these changes and update the
  pull request.

Eventually, someone will comment "LGTM" and merge your changes.

> "LGTM" probably means "looks good to merge". It's a compliment.

Pull requests for a typo fix should be resolved in 1-2 weeks.
Same for simple bug fixes.

Pull requests implementing a new feature should be resolved in a few months.
Pull requests changing an existing feature may take longer.

Pull requests that implement a backwards-incompatible change to an existing
 feature are unlikely to be merged until [Racket2](https://github.com/racket/racket/wiki/Racket2).

