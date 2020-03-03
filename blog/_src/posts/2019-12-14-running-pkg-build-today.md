    Title: Running a `pkg-build` today
    Date: 2019-12-14T17:47:39
    Tags: 

*posted by Ben Greenman, Alex Knauth, and Fred Fu*

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
This post explains how to meet these requirements.

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
If the host and guest OS are different, then follow steps 1-3 on the VM.
Once you have built a modified Racket, copy it to the host and follow steps
 4, 6, and 7 on the host.

> We have tested with:
> host Linux + guest Linux, and
> host macOS + guest Linux


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
  '("source-syntax" "typed-racket" "typed-racket-doc"
    "typed-racket-lib" "typed-racket-more" "typed-racket-test"))

(define tgt-repo "typed-racket")

(define tgt-branch "<branch-name>")
(define tgt-user "<username>")
(define tgt-commit "<commit-hash>")
;; ---

; format a GitHub package URL for a branch, see:
; https://docs.racket-lang.org/pkg/getting-started.html#(part._github-deploy)
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

```
 cd ~/my-build/
 git clone git://github.com/racket/racket
 cd racket
 make installers PKG="typed-racket" SRC_CATALOG=~/my-build/my-catalog
```

This will take some time.

Make sure that the build happens on the same kind of system that the VM
 will eventually use.
To be safe, you can always start the VM first (step 5) and run this build on it.

> `make installers` must start with a clean `racket/` clone.
> If something goes wrong along the way, make a new clone before retrying.
> Refer to the [Racket Build Guide](https://docs.racket-lang.org/racket-build-guide/index.html)
> (aka [build.md](https://github.com/racket/racket/blob/master/build.md)) for more information.


### Step 4: Serve the modified Racket

Go inside the cloned repo and run the following command to make an install
 web page:

```
 cd ~/my-build/racket/
 make site-from-installers
```

This command makes a new directory `~/my-build/racket/build/site/` with a
 few files, including:

- `build/site/installers/table.rktd` must contain a hash from strings
  to paths; ours contains `#hash(("localhost" . "racket-7.5.0.11-x86_64-linux.sh"))`
- `build/site/index.html` should resemble the picture below; for us, the
  link [localhost]() points to an install script

<img src="/img/build-site-index-example.png"
     alt="Example"
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

Install [VirtualBox](https://www.virtualbox.org/wiki/Downloads).
Make sure the `VBoxManage` executable is on your path.

Create a VM, either using [Vagrant](https://www.vagrantup.com) according to
 the [`pkg-build/example/` README](https://github.com/racket/pkg-build/blob/master/example/README.md)
 or [manually](https://github.com/racket/pkg-build/blob/master/README.md#local-builds).
After following those instructions, you should have:

- a VM named `pkg-build`,
- with a user named `racket` that can run `sudo` without a password,
- with [host-only networking](https://www.virtualbox.org/manual/ch06.html#network_hostonly) enabled,
- and one snapshot of the VM named `init`.

If you have a Linux VM, run `hostname -I` on the VM to get its IP address.
Then, with the VM running, try the following command on your host machine to test
 the network connection --- after replacing the sample address (`192....`)
 with your VM's address:

```
 ssh racket@192.168.99.100
```

Running `ssh` for the first time may raise a yes/no prompt about the login.
Type "y".
Future logins must succeed with no prompt.
 

### Step 6: Set up pkg-build

Clone and install the [`pkg-build`][pkg-build] repo.

```
  cd ~/my-build
  git clone git://github.com/racket/pkg-build
  raco pkg install ./pkg-build
```

Create a file named `run.rkt` that starts from the template in the
 [pkg-build README](https://github.com/racket/pkg-build#running-a-build).
Edit this file:

- the vbox `#:host` must match your VM's IP address,
- the `#:snapshot-url` must point to your web server from step 4 (likely <http://localhost:8000>),
- the `#:installer-platform-name` must match a key in the `table.rktd` file from step 4

Here is one `run.rkt` that we used for a successful build.

```
#lang racket/base

(require pkg-build)

(build-pkgs
 #:vms (list (vbox-vm #:name "pkg-build" #:host "192.168.99.100"))
 #:snapshot-url "http://0.0.0.0:8000"
 #:timeout 2100
 #:installer-platform-name "localhost")
```


### Step 7: Run the build

Make sure the VM is off.
Run the `run.rkt` file above.
We recommend redirecting the output to a log:

```
  racket run.rkt > pkg-build.log
```

This command should run for hours,
 starting and stopping the VM periodically.
If all goes well, the final log will contain details on any new package errors.


## Example log output

Our log for a modified Typed Racket began with the following lines,
 and then proceeded to archive every package:

```
>> Getting installer table
Installer is racket-7.5.0.11-x86_64-linux.sh
>> Downloading installer racket-7.5.0.11-x86_64-linux.sh
>> Archiving packages from
 http://0.0.0.0:8000/catalog/ https://pkgs.racket-lang.org/
== Archiving 1d6 ==
checksum: ae3bf1fc265bd1815dc8f9d6bbb153afdbf3a53d
Downloading repository https://github.com/jessealama/1d6.git
....
```

Some package archives failed, for example:

```
== Archiving simple-csv ==
checksum: f71d9b92826203cacf483ab5b2379fd18f8585d3
Downloading repository git://github.com/pragun/simple-csv
git-checkout: could not parse ref pkt
  pkt: "Repository not found.\n"
SKIPPING simple-csv
```

But most succeeded, including the Typed Racket packages.

After the archive, the build first tests the connection to the VM:

```
Creating catalog /home/ben/my-build/server/archive/catalog
>> Starting server at locahost:18333 for /home/ben/my-build/server/archive
>> Starting VM pkg-build
Stopping VirtualBox machine "pkg-build"
VBoxManage: error: Machine 'pkg-build' is not currently running
0%...10%...20%...30%...40%...50%...60%...70%...80%...90%...100%
Restoring snapshot 'init' (52bd14a8-783c-4e06-b60f-14730464f196)
Starting VirtualBox machine "pkg-build"
Waiting for VM "pkg-build" to power on...
VM "pkg-build" has been successfully started.
/usr/bin/ssh -R 18333:localhost:18333 racket@192.168.99.100
  '/usr/bin/env' 'PLTUSERHOME=/home/racket/build-pkgs/user'
  'PLT_PKG_BUILD_SERVICE=1' 'CI=true' 'PLTSTDERR=debug@pkg error'
  'PLT_INFO_ALLOW_VARS=;PLT_PKG_BUILD_SERVICE' '/bin/sh' '-c' 'echo hello'
hello
```

then installs Racket and starts building individual packages.

For each package, the script starts the VM for a setup,
 restarts to run tests, and finally shuts down the VM:

```
>> ========================================
>> Building unstable-contract-lib
0%...10%...20%...30%...40%...50%...60%...70%...80%...90%...100%

....

Stopping VirtualBox machine "pkg-build"
0%...10%...20%...30%...40%...50%...60%...70%...80%...90%...100%
>> ========================================
>> Testing unstable-contract-lib
0%...10%...20%...30%...40%...50%...60%...70%...80%...90%...100%

....

Stopping VirtualBox machine "pkg-build"
0%...10%...20%...30%...40%...50%...60%...70%...80%...90%...100%
```

Our change led to a few "Type Checker" errors during calls to `raco setup`.
These errors did not stop the build, which went on to render documentation
 and generate a few HTML pages to summarize the results.
Here are the final lines of output:

```
Rendering...
  pkg-build/robots.txt
  pkg-build/favicon.ico
  pkg-build/page-not-found.html
  pkg-build/.htaccess
  pkg-build/index.html
  pkg-build/about.html
Rendering done.
```


## Ways to fail

Before the successful run, we hit many problems.
Here are a few error messages and solutions.


#### Step 1: ssl-connect error

```
$ /Applications/Racket\ v7.6/bin/raco pkg catalog-copy --from-config my-catalog
osx-ssl-connect: connection failed
  message: The operation couldn’t be completed.
           (kCFErrorDomainCFNetwork error 2.)
  address: download.racket-lang.org
  port number: 443
```

Solution: connect to the Internet and try again.


#### Step 7: hash-ref failed

```
$ racket run.rkt
hash-ref: no value found for key
  key: "{1} Racket | {3} Linux | {4} x64_64 (64-bit), natipkg; built on Debian"
```

Open `racket/build/site/installers/table.rktd`, copy the key from the hashtable
 inside, and paste it into `run.rkt` for the `#:installer-platform-name` keyword argument.


#### Step 7: system* got `#f`

```
$ racket run.rkt
Creating catalog /home/racket/racket-pkg-build/server/archive/catalog
>> Starting server at locahost:18333 for /home/ben/my-build/server/archive
>> Starting VM pkg-build
Stopping VirtualBox machine "pkg-build"
system*: contract violation
  expected: path-string?
```

Put the `VBoxManage` executable on your host-machine PATH and try again.
(This error comes from code in the [`racket/remote-shell`](https://github.com/racket/remote-shell) repo.


#### Step 7: could not find a registered machine


```
$ racket run.rkt
Creating catalog /Users/Alex/racket-pkg-build/server/archive/catalog
>> Starting server at locahost:18333 for /home/ben/my-build/server/archive
>> Starting VM pkg-build
Stopping VirtualBox machine "pkg-build"
VBoxManage: error: Could not find a registered machine named 'pkg-build'
VBoxManage: error: Details: code VBOX_E_OBJECT_NOT_FOUND (0x80bb0001),
            component VirtualBoxWrap, interface IVirtualBox, callee nsISupports
VBoxManage: error: Context: "FindMachine(Bstr(a->argv[0]).raw(),
            machine.asOutParam())" at line 382 of file VBoxManageControlVM.cpp
VBoxManage: error: Could not find a registered machine named 'pkg-build'
VBoxManage: error: Details: code VBOX_E_OBJECT_NOT_FOUND (0x80bb0001),
            component VirtualBoxWrap, interface IVirtualBox, callee nsISupports
VBoxManage: error: Context: "FindMachine(Bstr(VMNameOrUuid).raw(),
            machine.asOutParam())" at line 2621 of file VBoxManageInfo.cpp
vbox-state: could not get virtual machine status: "pkg-build"
```

Here, `pkg-build` is the `#:name` argument in the `run.rkt` script.
This error can occur when the VM is on when the script runs.
Shut down the VM and try again.


#### Step 7: ssh could not resolve hostname

```
$ racket run.rkt
....
Waiting for VM "racket-pkg-build" to power on...
VM "racket-pkg-build" has been successfully started.
/usr/bin/ssh -R 18333:localhost:18333 racket@10.0.2.15
  '/usr/bin/env' 'PLTUSERHOME=/home/racket/build-pkgs/user'
  'PLT_PKG_BUILD_SERVICE=1' 'CI=true' 'PLTSTDERR=debug@pkg error'
  'PLT_INFO_ALLOW_VARS=;PLT_PKG_BUILD_SERVICE' '/bin/sh' '-c' 'echo hello'
ssh: Could not resolve hostname 10.0.2.15: nodename nor servname provided,
     or not known
....
ssh: failed
```

Double-check that the VM's IP address --- taken from `hostname -I` or `ifconfig`
 --- matches the `#:host` argument in the `run.rkt` script.

#### Step 7: ssh timeout

The error message is similar to the previous one, but `ssh` fails with a timeout.

Try turning on the VM and connecting from your host machine --- without running
 `run.rkt`.
If `ssh` asks for a yes/no response, then say "y", disconnect, turn off the VM,
 and try again.
That was the issue.



[pkg-build]: https://github.com/racket/pkg-build
[pkgd]: https://pkgd.racket-lang.org
[opaque-pr]: https://github.com/racket/typed-racket/pull/882
