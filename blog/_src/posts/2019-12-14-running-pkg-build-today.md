    Title: Running a `pkg-build` today
    Date: 2019-12-14T17:47:39
    Tags: 

*posted by Fred Fu, Ben Greenman, and Alex Knauth*

<!-- PRE -->
<!-- - blog/pkg-build/RUN-LOG-7 -->
<!-- - blog/pkg-build/edit-catalog-for-opaque.rkt -->
<!-- - https://pkg-build.racket-lang.org/about.html -->
<!--  -->
<!-- DIFF -->
<!-- - bump timeout, retries (checking ready. PR?) -->
<!-- - remote-shell pull requests -->
<!-- - can improve "echo hello" timeout for missing auth? forward prompt to user? -->


Suppose you've made a change to Racket and want to test it against all
 packages in the [main catalog][pkgd].
The [`pkg-build`][pkg-build] package can help --- provided you supply: (1) a
 modified version of Racket and (2) a suitable VM.
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

<!-- Manual: see the [`pkg/lib`](https://docs.racket-lang.org/pkg/lib.html) API -->
<!-- get-pkg-names-from-catalog ... easy -->
<!-- 2020-02-25 have 2446 names -->


### Motivating Example: Typed Racket

As of February 2020, Typed Racket pull request [882][opaque-pr] weakens
 the relation between untyped predicates and the type checker.
This change fixes a soundness bug, but may break programs that rely on the
 old behavior.
See the pull request for details.

Since we recently ran `pkg-build` on a modified Typed Racket for that pull
 request, this post uses Typed Racket as a concrete example.
If you copy/paste code below, look out for parts that need to be specialized
 for different modifications.


## How to run `pkg-build`

In short, the goal is to:
 create a modified distribution of Racket,
 configure a VM,
 and run the [`run.rkt`](https://github.com/racket/pkg-build/blob/master/README.md#running-a-build)
 script described by the `pkg-build` README file.
Creating the modified Racket is the most involved part.

**Note:** for the fastest results, the "host" OS on your computer and
 the "guest" OS on the VM must be the same (ideally Linux).
If the host and guest OS are different, then build the modified Racket
 on the VM and copy it to the host before setting up a web server in
 step 4 below.


#### Table of Contents

1. create a base catalog
2. edit the catalog
3. build racket from the catalog
4. serve a download site
5. set up a VM
6. set up pkg-build
7. run


### Step 1: generate a modern catalog

to copy cataglog:
`raco pkg catalog-copy --from-config <target-catalog-dir>`

dont need to download and build probably want snapshot don't need vm
(may want to use vm anyway but its optional until pkg-build step)

> _if the pkd-build runs on Ubuntu, then `make installers` and `make
> site-from-installer` must be run on Ubuntu as well_
> ASSUME YOU HAVE LINUX EVERYTHING --- thats what I did --- else you can ... Alex? ...
> build a racket installer on a linux machine (via make installers)
> without linux, can do 1-3 on the VM, compress build/site & send to host,
>  serve from host


### Step 2: edit the catalog to reflect your changes

delete <target-catalog-dir/pkgs-all>

ignore pkgs file

adjust entries to the typed-racket collections in the <target-catalog-dir/pkg/>
and point them to the version you want to test, i.e. the git url of your
 on-going typed racket

catalog files meant for Racket not humans,
may crash editor

- blog/pkg-build/edit-catalog-for-opaque.rkt

beware
 https://.....#commit?path=....
 vs
 https://....?path=....#commit

Q. does Fred have a test to validate the edits?
 "you can verify the changes by running
  raco pkg install typed-racket --catalog your-local-catalog-directory
  and see if you get the version you are working on"

Fred example edit: https://gist.github.com/capfredf/f6588f8096ffc6cbbc3a8adb80486b9d
"In short, I changed source part and the default in versions.  The checksum is
 the commit hash"


### Step 3: build Racket from the edited catalog

get a clean Racket repo
 (fails if unclean, if you run make first so be sure its clean SRC_CATALOG=)
 ... 2020-02-24 not exactly sure what SRC_CATALOG should be!

run `make installers  PKG="typed-racket" SRC_CATALOG= <target-catalog-dir>`


### Step 4: serve the edited Racket

go to the build/site
run `make site-from-installers`

(site-from-installers what does this do?)

python -m http.server <port>
   ... must be a racket way
   python 3 works, maybe earlier
   default python port 8000 worked for us
   build/site directory, should contain index = download page, table.rktd

   Alex "no, but there is table.rktd in the separate directory where I'm running run.rkt"
   Ben "I do have build/site/installers/table.rktd with 1 entry, localhost -> racket-7.5........sh"

Make sure the server is accessible from other vms.

@ben if you start a http server with port 8000 in the directory build/site and open
“http://localhost:8000/” in your browser, you should be able to see something similar to https://www.cs.utah.edu/plt/snapshots/current/installers/ (edited) 

### Step 5: set up a VM

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


### Step 6: set up pkg-build (sync with VM)

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


### Step 7: run pkg-build

 run outside the VM,
 begin with VM off (update pkg-build to say this!)


## Ways to Fail

We failed many times 

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

