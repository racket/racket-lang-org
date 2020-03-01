    Title: Running a `pkg-build` today
    Date: 2019-12-14T17:47:39
    Tags: 

*posted by Fred Fu, Ben Greenman, and Alex Knauth*

<!-- PRE -->
<!-- - blog/pkg-build/RUN-LOG-7 -->
<!-- - https://pkg-build.racket-lang.org/about.html -->
<!--  -->
<!-- DIFF -->
<!-- - bump timeout, retries (checking ready. PR?) -->
<!-- - remote-shell pull requests -->
<!-- - can improve "echo hello" timeout for missing auth? forward prompt to user? -->


Suppose you've made a change to Racket and want to test it against all
 packages in the [main catalog][pkgd].
The [`pkg-build`][pkg-build] package can help --- provided you have:
 (1) a modified version of Racket and
 (2) a suitable VM.
This post explains how to meet these two requirements.

<!-- more -->


## Why `pkg-build`?

Backwards compatibility matters.
New changes to packages in the Racket [main distribution](https://github.com/racket/main-distribution/blob/master/info.rkt)
 should not change old programs without a very good reason.

<!-- Misses planet, github, .... -->

One way to assess the damages of a possibly-breaking change is to use
 the [`pkg-build`][pkg-build] package to compile and test every package
 registered in the [main package catalog][pkgd].
<!-- no need to explain the benefits, obvious that main catalog has a variety of "good" programs -->
A build starts with a modified version of Racket
 and a sandbox virtual machine (VM).
For every package in the catalog, `pkg-build` installs the package and its
 dependencies in a new environment on the VM; therefore:

- packages that have conflicting dependencies get tested without interfering
  with one another,
  <!-- example? (pict3d and ruckus seem compatible) -->
- packages that require a huge amount of time or memory get killed,
- and re-builds are straightforward.

And so, running a build is well worth the initial setup cost.
<!-- TODO reword -->

<!-- Manual: see the [`pkg/lib`](https://docs.racket-lang.org/pkg/lib.html) API -->
<!-- get-pkg-names-from-catalog ... easy -->
<!-- 2020-02-25 have 2446 names -->


### Motivating Example: Typed Racket
<!-- TODO reword -->

As of February 2020, Typed Racket pull request [#882][opaque-pr] weakens
 the relation between untyped predicates and the type checker.
This change fixes a soundness bug, but may raise type errors in programs
 that rely on the old behavior.
See the pull request for details.

Since we recently ran `pkg-build` on a modified Typed Racket for that pull
 request, this post uses Typed Racket as a concrete example.
If you copy/paste code below, look out for parts that need to be adapted
 for different changes.

<!-- good example for other packages ... probably bad example for racket/racket ? -->


## How to run `pkg-build`

There are three high-level parts to running a build:
 create a modified distribution of Racket,
 configure a VM,
 and run.
Your host computer will eventually serve a Racket distribution --- much
 like [download.racket-lang.org](https://download.racket-lang.org/releases/7.6/) ---
 for the VM.

The first part is the most involved because it depends on the nature of
 your changes to Racket.
For the other parts, the [`pkg-build`][pkg-build] repo comes with more help.
In total, we break things down into 7 steps below.
 
**Note:** for the fastest results, the "host" OS on your computer and
 the "guest" OS on the VM must be the same.
If the host and guest OS are different, then follow steps 1-3 on the VM;
 after you've built a modified Racket, copy it to the host and follow steps
 4, 6, and 7 on the host.

> We have tested on:
> - host Linux + guest Linux
> - host macOS + guest Linux


#### Table of Contents
<!-- TODO looks funny with the normal indentation -->

1. Create a main distribution catalog
2. Edit the catalog to point to your changes
3. Build a Racket from the catalog
4. Serve the modified Racket
5. Configure a VM
6. Set up pkg-build
7. Run the build


### Step 0: Create a new directory

First, make a new directory for the build.
Use any name that you like.

```
  mkdir ~/my-build
  cd ~/my-build
```


### Step 1: Create a main distribution catalog

Find a modern version of Racket.
The [latest release](https://download.racket-lang.org) or a recent source
 build work fine.
A slightly older release should also work.

Choose a name for a new directory,
 connect to the Internet,
 and run the following command using "`raco`" from the modern Racket.

```
  raco pkg catalog-copy --from-config my-catalog
```

All done.

> The example command above creates a new directory named `my-catalog` that contains:
> 
> - a directory `pkg/` with one file inside for each package (over 2000 files),
> - a file `pkgs` that lists all package names,
> - and a big file `pkgs-all` with all the data from the `pkg/` directory.
> 
> Your directory should have similar contents.
> 
> Beware! The generated files, especially `pkgs-all`, have very long lines
> that may crash your favorite text editor; they overwhelmed gedit on our VM.
> These files are meant to be read by Racket programs.


### Step 2: Edit the catalog to point to your changes

Delete the large file `my-catalog/pkgs-all`.

Ignore the `my-catalog/pkgs` file.

Figure out which names in the `my-catalog/pkg/` directory relate to your changes.
If you have a new commit for Typed Racket, then there are six relevant names because one
 commit to the [Typed Racket repo](https://github.com/racket/typed-racket)
 affects six packages:
 `source-syntax`, `typed-racket`, `typed-racket-doc`, `typed-racket-lib`,
 `typed-racket-more`, and  `typed-racket-test`.

Each name `N` correponds to a file `my-catalog/pkg/N` that contains
 [metadata](https://docs.racket-lang.org/pkg/Package_Concepts.html#(tech._package._metadata))
 for a package, represented as a hash.
Your goal is to update the `'source`, `'checksum`, and `'versions` fields in
 each hash to point to your changes.
The new `'source` is the GitHub URL for your changes.
The new `'checksum` is the matching commit hash.
The new `'versions` must point to your changes as the default.
 
The script below can make these edits for Typed Racket, given:

- `tgt-user` your GitHub username
- `tgt-branch` the name of the public branch where your changes live
- `tgt-commit` the commit hash for your changes

**edit-catalog.rkt**

```
#lang racket/base
;; Usage:
;;   racket edit-catalog.rkt my-catalog/

;; ---
;; TODO edit these variables
(define pkg*
  '("source-syntax" "typed-racket" "typed-racket-doc" "typed-racket-lib" "typed-racket-more" "typed-racket-test"))

(define tgt-repo "typed-racket")

(define tgt-branch "<branch-name>")
(define tgt-user "<username>")
(define tgt-commit "<commit-hash>")
;; ---

;; format a GitHub package URL for a branch, see:
;;  https://docs.racket-lang.org/pkg/getting-started.html#(part._github-deploy)
(define (make-tgt-url pkg-name)
  (format "git://github.com/~a/~a.git?path=~a#~a"
          tgt-user tgt-repo pkg-name tgt-branch))

;; update three fields: '(source checksum versions)
(define (update-pkg-hash h pkg-name)
  (define u (make-tgt-url pkg-name))
  (let* ((h (hash-set h 'source u))
         (h (hash-set h 'checksum tgt-commit))
         (h (if (hash-has-key? h 'versions)
              (hash-update
                h 'versions
                (lambda (vh)
                  (hash-set
                    vh 'default
                    (hash 'source u 'checksum tgt-commit 'source-url u))))
              h)))
    h))

;; edit one file
(define (update-pkg-file p pkg-name)
  (define h (with-input-from-file p read))
  (define h+ (update-pkg-hash h pkg-name))
  (with-output-to-file p #:exists 'replace (lambda () (writeln h+))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:args (cat-dir)
    (for ((pkg-name (in-list pkg*)))
      (define p (build-path cat-dir "pkg" pkg-name))
      (update-pkg-file p pkg-name))))
```

After running the script, you can test your edits by installing the
 package from this modified catalog directory.
For a modern "`raco`", assuming you edited Typed Racket:

```
 raco pkg install typed-racket --catalog my-catalog
```

If the install succeeds, you'll be able to run tests to validate your changes.


### Step 3: Build a Racket from the catalog

Clone a new copy of Racket and build using your modified catalog.

<!-- TODO double-check the command BUILDING NOW -->
```
 cd ~/my-build/
 git clone git://github.com/racket/racket
 cd racket
 make installers PKG="typed-racket" SRC_CATALOG=../my-catalog
```

This will take some time.

Make sure that the build happens on the same kind of system that the VM
 will eventually use.
To be safe, you can always start the VM first (step 5) and run this build on it.

> In our experience, the command above often fails on an unclean `racket/`
> directory. If something goes wrong, make a new clone before retrying.


### Step 4: Serve the modified Racket
<!-- TODO does step3 make build/site ??? -->

Go inside the cloned repo and run the following command to make an install
 web page:

```
 cd ~/my-build/racket/build/site
 make site-from-installers
```

After, this `site/` directory should contain a file `index.html` that resembles
 the picture below, along with a few other files.
In particular, the link _localhost_ on the index page should point to an install script.

<!-- also build/site/installers/table.rktd -->

<img src="/img/build-site-index-example.png"
     alt="Example index.html"
     border="1"
     style="width: 70%" />

The final step is to start a local web server to host the install.
One way to run a server is with Python:

```
 cd ~/my-build/racket/build/site
 python -m http.server 8000
```

To double-check the server, open <http://localhost:8000> in a web browser.


### Step 5: Configure a VM
<!-- TODO -->

VBoxManage, follow pkg-build instructions, hostname -I,
   ssh test?, host-only network : Tools, add network (not preferences), VM settings, Adaptor 2, host only vboxnet0, ....

FRED
 #:host "192.168.99.100"
 for that part, you need to make sure which networking mode you are using for
  your virtualbox instance
 Usually, I would use bridge mode and then ssh into the guest os and use
  ifconfig to get the ip address of the guest os
 (updated: #:host is the ip address of you vm
  (https://github.com/racket/pkg-build/blob/master/main.rkt#L328) )

follow pkg-build do vbox stuff
- host-only network
- hostname -I 192.___


### Step 6: Set up pkg-build

   [pkg-build](https://github.com/racket/pkg-build), but you need to point the
   catalog url to the address of the http server.

Alex:
- replace the #:name string with the name of the VM as normal
- replace the #:host IP address string with the hostname -I after running the
   ... down and ... up commands, as I said about step iv of Prerequisites
- replace the #:snapshot-url string with the url string returned by the python
   http server still running on the host, including the port with the colon
- replace the #:installer-platform-name string with "localhost"
- before running run.rkt on the host, make sure the VM is shut down and the
   python http server is still running

note: main.rkt pretty well documented


### Step 7: Run the build

 run outside the VM,
 begin with VM off (update pkg-build to say this!)


## Ways to Fail

We failed many times 


PROBLEM
└─(13:02:%)── /Applications/Racket\ v7.6/bin/raco pkg catalog-copy --from-config my-catalog                                                                                      ──(Sun,Mar01)─┘
osx-ssl-connect: connection failed
  message: The operation couldn’t be completed. (kCFErrorDomainCFNetwork error 2.)
  address: download.racket-lang.org
  port number: 443
  context...:
   /Applications/Racket v7.6/collects/net/osx-ssl.rkt:281:0: osx-ssl-connect
   /Applications/Racket v7.6/collects/net/http-client.rkt:67:0: http-conn-open!
   /Applications/Racket v7.6/collects/net/http-client.rkt:272:0
   /Applications/Racket v7.6/collects/racket/contract/private/arrow-val-first.rkt:555:3
   /Applications/Racket v7.6/collects/net/url.rkt:201:0: http://getpost-impure-port
   /Applications/Racket v7.6/collects/net/url.rkt:308:2: redirection-loop
   /Applications/Racket v7.6/collects/racket/contract/private/arrow-val-first.rkt:555:3
   /Applications/Racket v7.6/collects/pkg/private/network.rkt:59:3
   /Applications/Racket v7.6/collects/pkg/private/catalog.rkt:218:0: read-from-server
   /Applications/Racket v7.6/collects/pkg/private/catalog.rkt:299:2: for-loop
   /Applications/Racket v7.6/collects/pkg/private/catalog-copy.rkt:33:0: pkg-catalog-copy
   /Applications/Racket v7.6/collects/racket/contract/private/arrow-val-first.rkt:555:3
   (submod "/Applications/Racket v7.6/collects/pkg/main.rkt" main): [running body]
   temp35_0
   for-loop
   run-module-instance!
   ...
SOLUTION
 connect to net

PROBLEM
 hash-ref: no value found for key
   key: "{1} Racket | {3} Linux | {4} x64_64 (64-bit), natipkg; built on Debian 7 (Wheezy)"
SOLUTION
 look at ???? edit run.rkt

PROBLEM
 So following the steps in pkg-build running run.rkt, It went for a while, including archiving, downloading, packing, and writing checksums for all the packages from a-to-z, but then failed with this error message:
 Creating catalog /home/racket/racket-pkg-build/server/archive/catalog
 >> Starting server at locahost:18333 for /home/racket/racket-pkg-build/server/archive
 >> Starting VM pkg-build
 Stopping VirtualBox machine "pkg-build"
 system*: contract violation
   expected: path-string?
   given: #f
   context...:
    /usr/racket-7.5.0.10/collects/racket/system.rkt:181:19
    /usr/racket-7.5.0.10/collects/racket/system.rkt:174:0: do-system*/exit-code
    /usr/racket-7.5.0.10/collects/racket/system.rkt:211:0: system*
    /home/racket/.racket/snapshot/pkgs/remote-shell-lib/vbox.rkt:112:0: stop-vbox-vm
    /usr/racket-7.5.0.10/collects/racket/contract/private/arrow-val-first.rkt:555:3
    /home/racket/.racket/snapshot/pkgs/pkg-build/main.rkt:445:2: install
    /home/racket/.racket/snapshot/pkgs/pkg-build/main.rkt:515:2: check-and-install
    /home/racket/.racket/snapshot/pkgs/pkg-build/main.rkt:123:0: build-pkgs
    "/home/racket/racket-pkg-build/run.rkt": [running body]
    temp35_0
    for-loop
    run-module-instance!
    perform-require!
SOLUTION
 It appears to be the line (system* VBoxManage "controlvm" vbox what) here: https://github.com/racket/remote-shell/blob/71cb7647c90851fac4629523d34983375fc2caa3/remote-shell-lib/vbox.rkt#L52
 remote-shell-lib/vbox.rkt:52
   (system* VBoxManage "controlvm" vbox what))
 <https://github.com/racket/remote-shell|racket/remote-shell>racket/remote-shell | Added by GitHub
 Where VMoxManage is defined with (define VBoxManage (find-executable-path "VBoxManage")) here: https://github.com/racket/remote-shell/blob/71cb7647c90851fac4629523d34983375fc2caa3/remote-shell-lib/vbox.rkt#L29
 remote-shell-lib/vbox.rkt:29
 (define VBoxManage (find-executable-path "VBoxManage"))
 <https://github.com/racket/remote-shell|racket/remote-shell>racket/remote-shell | Added by GitHub
 ah ok, the pkg-build instructions did ask for VBoxManage to be on your PATH
 on the HOST not the VM
TODO
 is this one better now?
 not yet https://github.com/racket/remote-shell/pull/5

PROBLEM
 Okay, same place, different error this time:
 Creating catalog /Users/Alex/racket-pkg-build/server/archive/catalog
 >> Starting server at locahost:18333 for /Users/Alex/racket-pkg-build/server/archive
 >> Starting VM pkg-build
 Stopping VirtualBox machine "pkg-build"
 VBoxManage: error: Could not find a registered machine named 'pkg-build'
 VBoxManage: error: Details: code VBOX_E_OBJECT_NOT_FOUND (0x80bb0001), component VirtualBoxWrap, interface IVirtualBox, callee nsISupports
 VBoxManage: error: Context: "FindMachine(Bstr(a->argv[0]).raw(), machine.asOutParam())" at line 382 of file VBoxManageControlVM.cpp
 VBoxManage: error: Could not find a registered machine named 'pkg-build'
 VBoxManage: error: Details: code VBOX_E_OBJECT_NOT_FOUND (0x80bb0001), component VirtualBoxWrap, interface IVirtualBox, callee nsISupports
 VBoxManage: error: Context: "FindMachine(Bstr(VMNameOrUuid).raw(), machine.asOutParam())" at line 2621 of file VBoxManageInfo.cpp
 vbox-state: could not get virtual machine status: "pkg-build"
   context...:
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/remote-shell-lib/vbox.rkt:112:0: stop-vbox-vm21
    /Applications/Racket/2019-10-21/Racket v7.5.0.3/collects/racket/contract/private/arrow-val-first.rkt:555:3
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/pkg-build/main.rkt:445:2: install64
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/pkg-build/main.rkt:515:2: check-and-install70
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/pkg-build/main.rkt:123:0: build-pkgs57
    "/Users/Alex/racket-pkg-build/run.rkt": [running body]
    temp37_0
    for-loop
    run-module-instance!125
    perform-require!78

 Looks like the error is (error 'vbox-state "could not get virtual machine status: ~s" vbox) from https://github.com/racket/remote-shell/blob/71cb7647c90851fac4629523d34983375fc2caa3/remote-shell-lib/vbox.rkt#L49
 remote-shell-lib/vbox.rkt:49
      (error 'vbox-state "could not get virtual machine status: ~s" vbox)]))
 <https://github.com/racket/remote-shell|racket/remote-shell>racket/remote-shell | Added by GitHub

 Which is triggered by the state not being one of the symbols (|powered off| aborted running saved paused restoring) from the case expression https://github.com/racket/remote-shell/blob/71cb7647c90851fac4629523d34983375fc2caa3/remote-shell-lib/vbox.rkt#L43-L49
 remote-shell-lib/vbox.rkt:43-49
   (case state
     [(|powered off| aborted) 'off]
     [(running saved paused) state]
     [(restoring) (vbox-state vbox)]
     [else 
  Show more
 <https://github.com/racket/remote-shell|racket/remote-shell>racket/remote-shell | Added by GitHub

 My probably-wrong-or-incomplete assumption was that the name pkg-build in the
 error message comes from (vbox-vm #:name "pkg-build" #:host "0.0.0.0:8000"),
 where the host 0.0.0.0:8000 comes from the IP address and port that the Python
 web server gave me. So when it says it can't get the state of that machine, is
 it saying it can't get the state from that Python web server?

 I'm trying again with a different #:host field value taken from the result of running hostname -I on the VM

 Well, even with that, same error:
 Creating catalog /Users/Alex/racket-pkg-build/server/archive/catalog
 >> Starting server at locahost:18333 for /Users/Alex/racket-pkg-build/server/archive
 >> Starting VM pkg-build
 Stopping VirtualBox machine "pkg-build"
 VBoxManage: error: Could not find a registered machine named 'pkg-build'
 VBoxManage: error: Details: code VBOX_E_OBJECT_NOT_FOUND (0x80bb0001), component VirtualBoxWrap, interface IVirtualBox, callee nsISupports
 VBoxManage: error: Context: "FindMachine(Bstr(a->argv[0]).raw(), machine.asOutParam())" at line 382 of file VBoxManageControlVM.cpp
 VBoxManage: error: Could not find a registered machine named 'pkg-build'
 VBoxManage: error: Details: code VBOX_E_OBJECT_NOT_FOUND (0x80bb0001), component VirtualBoxWrap, interface IVirtualBox, callee nsISupports
 VBoxManage: error: Context: "FindMachine(Bstr(VMNameOrUuid).raw(), machine.asOutParam())" at line 2621 of file VBoxManageInfo.cpp
 vbox-state: could not get virtual machine status: "pkg-build"
   context...:
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/remote-shell-lib/vbox.rkt:112:0: stop-vbox-vm21
    /Applications/Racket/2019-10-21/Racket v7.5.0.3/collects/racket/contract/private/arrow-val-first.rkt:555:3
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/pkg-build/main.rkt:445:2: install64
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/pkg-build/main.rkt:515:2: check-and-install70
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/pkg-build/main.rkt:123:0: build-pkgs57
    "/Users/Alex/racket-pkg-build/run.rkt": [running body]
    temp37_0
    for-loop
    run-module-instance!125
    perform-require!78

 I think in (vbox-vm #:name "pkg-build" #:host "192.168.99.100")
 the value for #:name should be the same as the VM’s name (edited) 
SOLUTION
 ????

PROBLEM
 The #:host "192.168.99.100" is supposed to be the IP address taken from running hostname -I on the VM, right?
 I'm having some problems with that, getting this error where 10.0.2.15 is both hostname -I on the VM and #:host in run.rkt, after archiving, downloading, packing, and writing checksums for all the packages:
 Waiting for VM "racket-pkg-build" to power on...
 VM "racket-pkg-build" has been successfully started.
 /usr/bin/ssh -R 18333:localhost:18333 racket@10.0.2.15 '/usr/bin/env' 'PLTUSERHOME=/home/racket/build-pkgs/user' 'PLT_PKG_BUILD_SERVICE=1' 'CI=true' 'PLTSTDERR=debug@pkg error' 'PLT_INFO_ALLOW_VARS=;PLT_PKG_BUILD_SERVICE' '/bin/sh' '-c' 'echo hello'
 ssh: Could not resolve hostname 10.0.2.15: nodename nor servname provided, or not known
 /usr/bin/ssh -R 18333:localhost:18333 racket@10.0.2.15 '/usr/bin/env' 'PLTUSERHOME=/home/racket/build-pkgs/user' 'PLT_PKG_BUILD_SERVICE=1' 'CI=true' 'PLTSTDERR=debug@pkg error' 'PLT_INFO_ALLOW_VARS=;PLT_PKG_BUILD_SERVICE' '/bin/sh' '-c' 'echo hello'
 ssh: Could not resolve hostname 10.0.2.15: nodename nor servname provided, or not known
 /usr/bin/ssh -R 18333:localhost:18333 racket@10.0.2.15 '/usr/bin/env' 'PLTUSERHOME=/home/racket/build-pkgs/user' 'PLT_PKG_BUILD_SERVICE=1' 'CI=true' 'PLTSTDERR=debug@pkg error' 'PLT_INFO_ALLOW_VARS=;PLT_PKG_BUILD_SERVICE' '/bin/sh' '-c' 'echo hello'
 ssh: Could not resolve hostname 10.0.2.15: nodename nor servname provided, or not known
 /usr/bin/ssh -R 18333:localhost:18333 racket@10.0.2.15 '/usr/bin/env' 'PLTUSERHOME=/home/racket/build-pkgs/user' 'PLT_PKG_BUILD_SERVICE=1' 'CI=true' 'PLTSTDERR=debug@pkg error' 'PLT_INFO_ALLOW_VARS=;PLT_PKG_BUILD_SERVICE' '/bin/sh' '-c' 'echo hello'
 ssh: Could not resolve hostname 10.0.2.15: nodename nor servname provided, or not known
 ssh: failed
   context...:
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/remote-shell-lib/ssh.rkt:180:2: loop
    /Applications/Racket/2019-10-21/Racket v7.5.0.3/collects/racket/contract/private/arrow-val-first.rkt:555:3
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/pkg-build/main.rkt:452:5
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/pkg-build/main.rkt:445:2: install64
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/pkg-build/main.rkt:515:2: check-and-install70
    /Users/Alex/Library/Racket/snapshot-7.5.0.3--2019-10-21/pkgs/pkg-build/main.rkt:123:0: build-pkgs57
    "/Users/Alex/racket-pkg-build/run.rkt": [running body]
    temp37_0
    for-loop
    run-module-instance!125
    perform-require!78
 Stopping VirtualBox machine "racket-pkg-build"

 Though I suppose it's also possible I did the OpenSSH step wrong

 capfredf
 what’s the network mode your VM is using? (edited) 
 I confused  bridge mode for host-only adapter yesterday.
 Sorry about that
 In the setting for network, you can add a new adapter using host-only
 then start your vm again
 run hostname -I in the vm (edited) 
SOLUTION
 ????
 for step iv I was having trouble, my dad helped me do two commands sudo
   ifconfig ... down and sudo ifconfig ... up where ... is from the interface
   name, to refresh something after switching the vm to host-only network
   settings, before running hostname -I to get the IP address

PROBLEM
 make-sure-remote-is-ready is now timing out for me:
 /usr/bin/ssh -R 18333:localhost:18333 racket@192.168.99.100 '/usr/bin/env' 'PLTUSERHOME=/home/racket/build-pkgs/user' 'PLT_PKG_BUILD_SERVICE=1' 'CI=true' 'PLTSTDERR=debug@pkg error' 'PLT_INFO_ALLOW_VARS=;PLT_PKG_BUILD_SERVICE' '/bin/sh' '-c' 'echo hello'
 hm, maybe ssh was waiting for a "yes/no" to add a known host
 maybe the vm & host need to exchange authorized keys? (I only put the host key
  in the vm's .ssh/ dir)
 or does the host need to change its .ssh/known_hosts file? (would both work?)
SOLUTION
 if you can ssh into the vm as the user `racket` (or any other user you specify
 in (vbox-vm …) ) without answering yes or no, then I think there is nothing
 else to be done for the ssh part

 Ben did (it's still running). Alex is having very strange issues with the VM host network.
 [[ He adds a vboxnet0, notes the hostname -I , restarts the machine, and now the hostname -I is different ]]
 I'm going to send Alex my output so that he can start fixing any TR-opaque-related errors (edited) 

PROBLEM
 ????
SOLUTION
 for step v I was also having trouble, so this is where I deleted all previous
   snapshots called init, before creating the new snapshot called init, otherwise
   the run.rkt script might use the earlier snapshot not the later one


## Contributions

If you can think of improvements, send them these ways
(maybe delete this section)

- racket/remote-shell
- racket/pkg-build
- racket/racket-lang-org
- racket/racket ???


[pkg-build]: https://github.com/racket/pkg-build
[pkgd]: https://pkgd.racket-lang.org
[opaque-pr]: https://github.com/racket/typed-racket/pull/882

[`run.rkt`](https://github.com/racket/pkg-build/blob/master/README.md#running-a-build)
