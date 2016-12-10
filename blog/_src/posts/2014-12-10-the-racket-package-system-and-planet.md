
    Title:The Racket package system and PLaneT
    Date:2014-12-10T12:58:00.000-05:00
    Tags:

*posted by Jay McCarthy*

We have recently moved the majority of Racket's code base into packages and repositories separate from the main core repository. This has given the Racket package system another cycle of attention. Whenever this happens, there are often questions and confusion about how to solve various distribution problems with the package system. A natural point of comparison is the older PLaneT system provided by Racket that appears to solve similar problems. In this blog post, I attempt to explain the purpose of the package system and its relation to PLaneT.

The package system and PLaneT do not solve the same problem and don't exist for the same reason.

PLaneT is:  

* A file distribution mechanism for source code.    

    Via `.plt` files that are installed into a particular place on your machine and then `raco setup`'d.  
  
* A mechanism for automatically downloading and installing source code just before it is needed by programs.

    Via the `(planet ...)` require form.  
  
* A centralized database of libraries

    Via the PLaneT website and its server & protocol which were undocumented and proprietary for the majority of PLaneT's life  
  
* A prescriptive model of how programs and libraries should be composed.

    Specifically the system of major/minor versions, tagging packages by author name, and embedding the names of packages in source code.  


In contrast, the package system is:

* A file distribution mechanism for source code, byte code, and documentation.  Via the `raco pkg` command.


In this way, the package system is almost identical to an operating system package system like Debian's dpkg and apt systems. The problem is very finely tailored and becomes more flexible as a result (notice that we can now distribute byte code and documentation.) This design aspires to follow the admonition of [holy writ](http://people.csail.mit.edu/jaffer/r3rs_2.html#SEC2): "Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses and restrictions that make additional features appear necessary."

Furthermore, it was intended to solve practical problems throughout the Racket ecosystem. In particular, one of the common complaints people had and have about PLaneT is the very long install times because of long builds. The package system allows this problem to be solved by distributing pre-built code.

Since the package system specifically does not address jobs 2, 3, or 4 of PLaneT, we have to ask, "Do they need to be solved?" and if so, "How can we solve them on top of the package system, i.e. as a library in honor of the design principle?".

In particular, 2 and 3 are very painful for people wanting to just use the file distribution mechanism of PLaneT. 2 causes unpredictability, because you don't know if running a program will start a long invocation of "raco setup", require Internet access, and start running un-vetted code. 3 requires you to share your code if you want to use the file distribution mechanism and is a single point of failure for doing installation.

By not mandating how to address 2 and 3 in the package system, we offer flexibility. Here is where the solutions to these jobs are now:

2. There is currently no way to get automatic installs of packages.  However, both DrRacket and xrepl offer advice about which packages you might want to install to compile and run the program. It would be natural to extend this advice to be automatic and patches are welcome.  Given the experiences of operating systems which merely make suggestions (`nethack: command not found, provided by nethack-console`), I personally feel like we are at the sweet spot.

3. The file distribution mechanism's flexible package sources combine with a very simple protocol for package catalogs (Take a URL, add`/pkg/`, add a string, get a `read`-able hash table) to look up packages you don't yet have. As a service, we run a few catalogs (one for each release, plus [pkgs.r-l.o](http://pkgs.racket-lang.org)). But we expect that users with special needs (such as sensitive installations that need exactly certain tested and trusted versions, especially with proprietary software) will build their own catalogs on private Web sites.

Clearly, however, job 4 is where PLaneT and the package system differ the most.

With the package system, we follow the precedent of operating systems.  An OS package's job is to get files into the right spot. An OS package contains a binary and instructions to install it as `/usr/bin/zsh`. It is not typical in OSes to be able to install multiple packages (such as different "versions" of the "same" package) that both provide `/usr/bin/zsh`. When you're at a Unix prompt, you don't have to write `zsh-5.0.5/usr/bin/zsh`. It's possible that many consider this is a big problem with OSes and indeed we do observe that it is fairly common to provide packages that provide binaries and libraries with embedded names such as how on my machine I have `python2.6`, `python2.7`, and `python3.2` all in my `$PATH`. It is important to realize, however, that the `deb` format and the `apt` tool didn't need to change to support this change or future changes in perspective in how to compose code.

I hope this analogy helps understand the Racket package system. In the package system, a package doesn't install "binaries", "man pages", and "init scripts", but installs similar things, such as "module paths", "documentation manuals", and "`raco` commands". Each of these has a notion of conflict: can't have two `zsh`s or two `racket/list`s; can't have two `zsh.1` pages or two docs named `doc`; can't have two modules trying to provide `raco neo-tokyo-is-about-to-explode`. If you find a random `.deb` on the Internet, can you predict what binaries it will contain from its name?  No. The same goes for Racket packages. However, if you are egregiously weird, then people probably won't want to install your packages, just like for random `deb`s.

However, clearly rules are helpful. In the world of operating systems, you know that basically all packages distributed by Debian can be installed at the same time, except for "virtual packages" that do stuff like selecting whether `postfix` or `sendmail` should be responsible for the `sendmail` command. These rules are not enforced through technology, though. Instead, the Debian maintainers have a social process that enforces them, with information being provided by technology (such as regression systems that identify unintended conflicts.) The catalog server that the Racket team provides helps facilitate a similar process with the concentric rings (all ring <=1 packages can be installed at once and ring 1< packages can do anything.)

Non-conflicting sets of packages is the simplest rule to define and enforce. Other rules about backwards compatibility are much more complicated to define and enforce. I do not believe there is much precedent in the world of OSes, although we can see a little bit of what they do through things like `libgtk`, `libgtk2`, and `libgtk3`, where generally code written for one `libgtk2` package is compatible with all `libgtk2` packages made in the future, but `libgtk3` is effectively a totally different package and introduces totally separate binaries like `gtk3-config`.

The most that the Racket team attempts to do here is to say, "Here are the rules we will follow and we think you should follow them too."  Specifically, that we will maintain backwards compatibility or make a new package.  We can't and won't enforce this, nor do we always live up to it with our own work (but we feel really bad about it when we do.)

Although my main goal of this section has been to explain my solution to (4), a great thing about the package system is that it is not binding at all. You can decide to follow the same rules as PLaneT. It is easy to do so:

  
* Always name your packages `$AUTHOR-$PACKAGE-$MAJOR`
  
* Always provide modules from only the collection, `$AUTHOR-$PACKAGE-$MAJOR`
  
* Maintain backwards compatibility within releases of `$AUTHOR-$PACKAGE-$MAJOR`
  
* Update the `'version` metadata in the package `info.rkt` to reflect the `$MINOR` version.


And, boom!, you've recreated the rules of PLaneT to a T except for two things: (a) you'll still need to put a dependency on `$AUTHOR-$PACKAGE-$MAJOR` on the outside of code in a package `info.rkt` file rather than just inside files and (b) you can't use `$AUTHOR-$PACKAGE` to refer to "whatever the current `$MAJOR`" is.

The first compromise of adding something to the `info.rkt` is fairly modest, as it requires O(1) line modifications.

The second compromise is more severe, although actually you could just maintain such a package and deal with the breakage that occurs when you try to upgrade. Such breakage, however, was present in PLaneT too, as when a package was installed based on `$AUTHOR-$PACKAGE` only the local machine would cache the version used, so if you took the requiring module to another machine, it would download a new version and, potentially, have a backwards incompatibility problem. Using the package system in the most naive way (i.e. installing the `$AUTHOR-$PACKAGE` at some point and programming to that) would work exactly the same as PLaneT, except that the package system was designed to be able to port installations from one machine to another with `raco pkg migrate`.

I hope this blog post has helped explain the package system and shown that it does not prevent you from doing anything that PLaneT let you do, it only allows you to do more.

<!-- more -->



* * *

I think node.js's package manager gets versioning right. NPM uses semver to manage dependency versions, and it works well. The node-semver package demonstrates how this versioning works. A semver string is then included a module's dependencies list, which can specify all sorts of things, including "use the latest version of the library," "use the latest version given a particular major.minor string," and "use this precise major.minor.patch" version.

NPM handles the rest, downloading the correct version of the dependencies. The reason this is tricky is that this requires every module to have its own set of dependencies, since different modules can request different versions of packages. I'm not sure exactly how Node handles loading multiple versions of the same dependency with regards to performance and interaction, though it might be worth looking into.

In contrast, the Racket package manager provides very little versioning support, and packages have to be installed in an entire user or installation scope, rather than having module-specific dependency versioning. Is this bad? I don't know. But it's different, and I think NPM's model is worth considering.

— *Alexis King, 9 January 2015*

* * *

NPM uses local packages. Essentially, every dependancy you install is local to your package, and no other packages can access them. Your dependancies, in turn have a tree of packages that you can't access. In other words, every single package gets a separate copy of its dependancies. This might seem terrible, but the node community is deeply rooted in  UNIX, so most packages expose only a few functions doing one thing well, and so the overhead of having many copies is very low.

— *g145, 4 September 2015*

* * *

