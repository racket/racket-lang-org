    Title: Running a `pkg-build` today
    Date: 2019-12-14T17:47:39
    Tags: 

*posted by Fred Fu, Ben Greenman, and Alex Knauth*

How to run a pkg-build

TODO try Bogdan's example script
 looks great, helps make a VM for bros

<!-- more -->

pkg-build is a service to run all Racket packages

if you have a change to Racket and want to test agaist all packages then you
 want to use pkg-build

#### How it works, Basics


package catalogs
easy to get a list

VM because

(Ok you can go ahead now and follow instructions if you like)

main.rkt pretty well documented


## How to run

;; maybe use big headings for each step ... I definitely like headings to split,
  because these steps are huge ... and perhaps first step is the motivation
  premises assumptions

Youve changed typed racket, committed to XXXXXX and pushed to YYYY

1. get a modern racket, you may already have one, copy the catalog

2. catalog files are XXX beware, meant for Racket not humans,
   delete one file, edit others, how to find others, script to edit

   is there a Fred test for this?

3. see fred

4. see fred

5. be sure its clean, fails if you don't

6. site-from-installers what does this do?

7. python -m http.server ... must be a racket way
   python 3 works, maybe earlier
   default python port 8000 worked for us
   build/site directory, should contain index = download page, table

8. get run script, edit accordingly

9. VBoxManage, follow pkg-build instructions, hostname -I,
   ssh test?, host-only network : Tools, add network (not preferences), VM settings, Adaptor 2, host only vboxnet0, ....

NOW you need the VM, follow instructions on package 

- - -

bump timeout, retries (checking ready. PR?)

NOTE !!! build a racket installer on a linux machine (via make installers)


- - -

FredF thread
@capfredf Alex and I are trying to follow your pkg-build instructions ....
93 replies

ben:cacutar:  3 days ago
we're stuck on step 2
we see a pkgs file, and a pkgs-all file, and a pkg/ directory, and we're not sure what to edit and how

ben:cacutar:  3 days ago
pkgs-all seems generated, and probably not what we want to edit

AlexKnauth  3 days ago
The pkgs file seems to be just the names, no pkg sources to edit. The pkg directory has files that contain package sources, but also other things such as the checksums and dependencies... would we have to edit those too?

ben:cacutar:  3 days ago
should we edit pkg/typed-racket-more (and others) so the source field points to our github URL and leave the rest of the file the same?

ben:cacutar:  3 days ago
(btw how did you edit the pkgs files? the graphical text editor on the VM we made crashes thanks to the long lines)
:cry:
1


capfredf  3 days ago
I think the pkgs file is irrelevant, but you got to delete the pkgs-all file, because raco would first use the information from the pkgs-all file. If the file doesn’t exist, it will use the package information from files in pkg/ directory.
At first, I changed the pkg sources in pkg/{typed-racket, typed-racket-more, source-syntax, typed-racket-lib, typed-racket-doc}/} , but it didn’t work. so I looked into the code and found out the reason above. I didn’t want to do the the same changes to the pkgs-all file, so I simply deleted it.  So I haven’t tried using pkgs-all to do the job, but it should work.

capfredf  3 days ago
IIRC I didn’t use an editor. I wrote a racket program to update the source information, i.e. read the hash and change some values and write the hash back, but I can’t find the code right now. (edited) 
:+1:
3


capfredf  3 days ago
As for the changes to the files, here is the modified pkgs/typed-racket   I used back then:
https://gist.github.com/capfredf/f6588f8096ffc6cbbc3a8adb80486b9d (search for “capfredf”) (edited) 
:+1:
2


capfredf  3 days ago
In short, I changed source part and the default in versions.  The checksum is the commit hash
:+1:
1


capfredf  3 days ago
you can verify the changes by running raco pkg install typed-racket --catalog your-local-catalog-directory and see if you get the version you are working on (edited) 

capfredf  3 days ago
FYI,
https://github.com/racket/racket/blob/master/racket/collects/pkg/private/catalog.rkt#L327
this is the code that reads pkgs/… or pkg-all (edited) 
racket/collects/pkg/private/catalog.rkt:327
         (define pkgs-all-path (build-path path "pkgs-all"))
<https://github.com/racket/racket|racket/racket>racket/racket | Added by GitHub

ben:cacutar:  2 days ago
we're having trouble verifying the changes with raco pkg install and with step 5

ben:cacutar:  2 days ago
step5 gives errors that typed-racket & other packages can't be found

AlexKnauth  2 days ago
Some examples of errors I'm getting during step 5 include:
raco setup: error: during making for <pkgs>/errortrace-lib/errortrace
raco setup:   open-input-file: cannot open module file
raco setup:     module path: syntax/source-syntax
raco setup:     path: /home/racket/racket/racket/collects/syntax/source-syntax.rkt
raco setup:     system error: no such file or directory; rktio_err=3
raco setup:     compiling: <pkgs>/errortrace-lib/errortrace/stacktrace.rkt
and:
raco setup: error: during making for <pkgs>/source-syntax/typed-racket-test/performance
raco setup:   standard-module-name-resolver: collection not found
raco setup:     for module path: typed/racket/base
raco setup:     collection: "typed/racket"
raco setup:     in collection directories:
raco setup:      /home/racket/racket/build/user/7.5.0.11/collects
raco setup:      /home/racket/racket/racket/collects
raco setup:      ... [29 additional linked and package directories]
raco setup:     compiling: <pkgs>/source-syntax/typed-racket-test/performance/function-contract.rkt

ben:cacutar:  2 days ago
okay maybe those step5 issues were our mistake ... the urls we put in the catalog were wrong (we had https://.....#commit?path=.... instead of https://....?path=....#commit ) (edited) 

ben:cacutar:  2 days ago
sigh, maybe both fails were because of that

ben:cacutar:  2 days ago
success on 5 and 6!

ben:cacutar:  2 days ago
how do we start an http server?

ben:cacutar:  2 days ago
phew we used python3.7 -m http.server (also tried & failed with plt-web-server)

AlexKnauth  2 days ago
I'm at the step where I'm following the pkg-build instructions, and I'm getting this error when running run.rkt:
>> Getting installer table
hash-ref: no value found for key
  key: "{1} Racket | {3} Linux | {4} x64_64 (64-bit), natipkg; built on Debian 7 (Wheezy)"
  context...:
   /home/racket/.racket/snapshot/pkgs/pkg-build/main.rkt:123:0: build-pkgs
   "/home/racket/racket-pkg-build/run.rkt": [running body]
   temp35_0
   for-loop
   run-module-instance!
   perform-require!

capfredf  2 days ago
I usually use python3 -m http.server <port> (edited) 

ben:cacutar:  2 days ago
whats a good port? do we need to tell pkg-build the port?

ben:cacutar:  2 days ago
the default is 8000 and seems okay; we tried 80 and get permission errors

AlexKnauth  2 days ago
The default port seems to be 8000, I'm getting permission errors when trying other things like 80

capfredf  2 days ago
Yes, 8000 is fine

capfredf  2 days ago
is there table.rktd under the directory build/site?

AlexKnauth  2 days ago
no, but there is table.rktd in the separate directory where I'm running run.rkt (edited) 

capfredf  2 days ago
weird, IIRC, after make site-from-installers, there should be a table.rktd under build/site

AlexKnauth  2 days ago
The table.rktd in that separate directory where run.rkt is is a hash-table where the first key-value pair is ("{1} Racket | {1} Windows | x86 (32-bit)" . "racket-7.5-i386-win32.exe")

ben:cacutar:  2 days ago
I'm not sure about run.rkt ; I do have build/site/installers/table.rktd with 1 entry, localhost -> racket-7.5........sh

AlexKnauth  2 days ago
What if I change the run.rkt to use "{1} Racket | {3} Linux | {3} x64_64 (64-bit), natipkg; built on Debian 8 (Jessie)" in the #:installer-platform-name?
Update: that seems to be an improvement (edited) 

capfredf  2 days ago
I’m a little confused. What is run.rkt?

AlexKnauth  2 days ago
From the instructions on pkg-build, in https://github.com/racket/pkg-build/blob/master/README.md#running-a-build (edited) 

capfredf  2 days ago
I see.

capfredf  2 days ago
@ben if you start a http server with port 8000 in the directory build/site and open “http://localhost:8000/” in your browser, you should be able to see something similar to https://www.cs.utah.edu/plt/snapshots/current/installers/ (edited) 

capfredf  2 days ago
or under build/site/installers

capfredf  2 days ago
then run the run.rkt, see if it works this time

AlexKnauth  2 days ago
It seems to have started working after we changed the string "{1} Racket | {3} Linux | {3} x64_64 (64-bit), natipkg; built on Debian 7 (Wheezy)" in run.rkt to use "{1} Racket | {3} Linux | {3} x64_64 (64-bit), natipkg; built on Debian 8 (Jessie)" instead

ben:cacutar:  2 days ago
yes I do see the installers page
minutes ago, we both started a run.rkt  and it's doing things (like Alex says)
I gave it the #:host address for my computer though, instead of localhost:8000 so idk if things are going to break later

ben:cacutar:  2 days ago
but we're about to stop working on this for today. Thanks for the help!

capfredf  2 days ago
Oh, I forgot to mention that for the
#:installer-platform-name, you have to use a name listed in your table.rktd . Sorry about that….. (edited) 

ben:cacutar:  2 days ago
(well the author of pkg-build forgot too)

capfredf  2 days ago
#:host "192.168.99.100"
for that part, you need to make sure which networking mode you are using for your virtualbox instance

ben:cacutar:  2 days ago
we should try to improve these readmes, and/or write a post for blog.racket-lang.org

capfredf  2 days ago
Usually, I would use bridge mode and then ssh into the guest os and use ifconfig to get the ip address of the guest os
(updated: #:host is the ip address of you vm (https://github.com/racket/pkg-build/blob/master/main.rkt#L328) ) (edited) 

capfredf  2 days ago
then use that address in run.rkt

capfredf  2 days ago
:cry:(looks like I forgot to mention more things than I thought) (edited) 

AlexKnauth  2 days ago
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
:disappointed:
1


AlexKnauth  2 days ago
It appears to be the line (system* VBoxManage "controlvm" vbox what) here: https://github.com/racket/remote-shell/blob/71cb7647c90851fac4629523d34983375fc2caa3/remote-shell-lib/vbox.rkt#L52
remote-shell-lib/vbox.rkt:52
  (system* VBoxManage "controlvm" vbox what))
<https://github.com/racket/remote-shell|racket/remote-shell>racket/remote-shell | Added by GitHub

AlexKnauth  2 days ago
Where VMoxManage is defined with (define VBoxManage (find-executable-path "VBoxManage")) here: https://github.com/racket/remote-shell/blob/71cb7647c90851fac4629523d34983375fc2caa3/remote-shell-lib/vbox.rkt#L29
remote-shell-lib/vbox.rkt:29
(define VBoxManage (find-executable-path "VBoxManage"))
<https://github.com/racket/remote-shell|racket/remote-shell>racket/remote-shell | Added by GitHub

ben:cacutar:  2 days ago
ah ok, the pkg-build instructions did ask for VBoxManage to be on your PATH

ben:cacutar:  2 days ago
I think that means we don't want to run run.rkt in the VM

AlexKnauth  2 days ago
It is on my path outside the VM, but not inside

capfredf  2 days ago
Does (find-executable-path "VBoxManage") return #f on the host os? @AlexKnauth

AlexKnauth  2 days ago
Depends on DrRacket vs Command-Line.
DrRacket: yes, it returns #f
Command-Line: no, it returns #<path:/usr/local/bin/VBoxManage> outside the VM

capfredf  2 days ago
so if you run racket run.rkt at command line outside the VM, are you still getting the error message you posted ? (edited) 

AlexKnauth  2 days ago
I'm in the middle of trying that now

AlexKnauth  2 days ago
I accidentally installed the wrong pkg-build package, and then I had a weird extra invisible character to delete, and now I'm running it

AlexKnauth  2 days ago
It's going through archiving, downloading, packing, and writing checksums for all the packages again, I'll update once it gets past that step to see if this error is fixed now

AlexKnauth  1 day ago
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

AlexKnauth  1 day ago
Looks like the error is (error 'vbox-state "could not get virtual machine status: ~s" vbox) from https://github.com/racket/remote-shell/blob/71cb7647c90851fac4629523d34983375fc2caa3/remote-shell-lib/vbox.rkt#L49
remote-shell-lib/vbox.rkt:49
     (error 'vbox-state "could not get virtual machine status: ~s" vbox)]))
<https://github.com/racket/remote-shell|racket/remote-shell>racket/remote-shell | Added by GitHub

AlexKnauth  1 day ago
Which is triggered by the state not being one of the symbols (|powered off| aborted running saved paused restoring) from the case expression https://github.com/racket/remote-shell/blob/71cb7647c90851fac4629523d34983375fc2caa3/remote-shell-lib/vbox.rkt#L43-L49
remote-shell-lib/vbox.rkt:43-49
  (case state
    [(|powered off| aborted) 'off]
    [(running saved paused) state]
    [(restoring) (vbox-state vbox)]
    [else 
 Show more
<https://github.com/racket/remote-shell|racket/remote-shell>racket/remote-shell | Added by GitHub

AlexKnauth  1 day ago
My probably-wrong-or-incomplete assumption was that the name pkg-build in the error message comes from (vbox-vm #:name "pkg-build" #:host "0.0.0.0:8000"), where the host 0.0.0.0:8000 comes from the IP address and port that the Python web server gave me. So when it says it can't get the state of that machine, is it saying it can't get the state from that Python web server?

AlexKnauth  1 day ago
I'm trying again with a different #:host field value taken from the result of running hostname -I on the VM

AlexKnauth  1 day ago
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

capfredf  1 day ago
@AlexKnauth what’s the name you gave to your guest os in virtual box? (edited) 

capfredf  1 day ago
is it “pkg-build”?

capfredf  1 day ago
I think in (vbox-vm #:name "pkg-build" #:host "192.168.99.100")
the value for #:name should be the same as the VM’s name (edited) 

capfredf  1 day ago
Screen Shot 2019-12-13 at 7.18.53 AM.png 
Screen Shot 2019-12-13 at 7.18.53 AM.png



AlexKnauth  1 day ago
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

AlexKnauth  1 day ago
Though I suppose it's also possible I did the OpenSSH step wrong

capfredf  1 day ago
what’s the network mode your VM is using? (edited) 

capfredf  1 day ago
I confused  bridge mode for host-only adapter yesterday.
Sorry about that
In the setting for network, you can add a new adapter using host-only

capfredf  1 day ago
image.png 
image.png



capfredf  1 day ago
then start your vm again
run hostname -I in the vm (edited) 

capfredf  1 day ago
you should be able to see two addresses

capfredf  1 day ago
use the one starting with “192.” in your run.rkt
:+1:
1


ben:cacutar:  1 day ago
make-sure-remote-is-ready is now timing out for me:
/usr/bin/ssh -R 18333:localhost:18333 racket@192.168.99.100 '/usr/bin/env' 'PLTUSERHOME=/home/racket/build-pkgs/user' 'PLT_PKG_BUILD_SERVICE=1' 'CI=true' 'PLTSTDERR=debug@pkg error' 'PLT_INFO_ALLOW_VARS=;PLT_PKG_BUILD_SERVICE' '/bin/sh' '-c' 'echo hello'

ben:cacutar:  1 day ago
hm, maybe ssh was waiting for a "yes/no" to add a known host

ben:cacutar:  1 day ago
maybe the vm & host need to exchange authorized keys? (I only put the host key in the vm's .ssh/ dir)

capfredf  1 day ago
yes

ben:cacutar:  1 day ago
or does the host need to change its .ssh/known_hosts file? (would both work?)

capfredf  1 day ago
I don’t think you need to change the host’s .ssh/known_hosts

capfredf  1 day ago
if you can ssh into the vm as the user `racket` (or any other user you specify in (vbox-vm …) ) without answering yes or no, then I think there is nothing else to be done for the ssh part

ben:cacutar:  1 day ago
ugh I should have changed the #:snapshot-url

capfredf  1 day ago
did you guys make your package building work?

ben:cacutar:  1 day ago
I did (it's still running). Alex is having very strange issues with the VM host network.
[[ He adds a vboxnet0, notes the hostname -I , restarts the machine, and now the hostname -I is different ]]
I'm going to send Alex my output so that he can start fixing any TR-opaque-related errors (edited) 
:open_mouth:
1


capfredf  1 day ago
@AlexKnauth can you post the content of  /etc/network/interfaces on the vm? (edited) 

AlexKnauth  1 day ago
it says command not found

AlexKnauth  1 day ago
Oh that's because it's a file, not a command. The file has the contents:
# interfaces(5) file used by ifup(8) and ifdown(8)
auto lo
iface lo inet loopback

capfredf  13 hours ago
@AlexKnauth  It doesn’t look right to me.
here is mine:
# /etc/netplan for current configuration.
# To re-enable ifupdown on this system, you can run:
#    sudo apt install ifupdown
# The loopback network interface
auto lo
iface lo inet loopback
# Host-only interface
auto enp0s8
iface enp0s8 inet static
        address         192.168.56.100
        netmask         255.255.255.0
        network         192.168.56.0
        broadcast       192.168.56.255
# NAT interface
auto eth2
iface eth2 inet dhcp

capfredf  13 hours ago
Are you on Ubuntu? what version are you using?

AlexKnauth  6 hours ago
My VM is Ubuntu 18.04.3 LTS

- - -

> @AlexKnauth Before building packages with an on-going version of typed-racket, you need to build a racket bundled with that version of TR. Here is what I did 8 months ago:
> 1. dowload/get latest version of racket. Use the corresponding raco to copy cataglog:
> `raco pkg catalog-copy --from-config <target-catalog-dir>`
> 2. adjust entries to the typed-racket collections in the <target-catalog-dir/pkg/> and point them to the version you want to test, i.e. the git url of your on-going typed racket
> 3. delete <target-catalog-dir/pkgs-all>
> 4. get a clean Racket repo
> 5. run `make installers  PKG="typed-racket" SRC_CATALOG= <target-catalog-dir>`
> 6. run `make site-from-installers`
> 7. go to the build/site and start a http server
> 8. follow the steps in the readme of [pkg-build](https://github.com/racket/pkg-build), but you need to point the catalog url to the address of the local http server.
> 
> if you run into any trouble, I'll be very happy to help you work them out.

- - -

Alex got it working

On @capfredf’s comment, I did:
steps 1-6 on the VM so that it uses Linux
before step 7, I compressed the build/site folder into a tgz, copied the tgz to my host computer, and unzipped it in the host
on step 7, I run the python http server on the host using the files copied from the vm
On the pkg-build readme instructions on Prerequisites, I did:
steps i-iii normally on the VM
for step iv I was having trouble, my dad helped me do two commands sudo ifconfig ... down and sudo ifconfig ... up where ... is from the interface name, to refresh something after switching the vm to host-only network settings, before running hostname -I to get the IP address
after step iv make sure you can manually ssh into the VM using the IP address you got from step iv
for step v I was also having trouble, so this is where I deleted all previous snapshots called init, before creating the new snapshot called init, otherwise the run.rkt script might use the earlier snapshot not the later one
for step vi it should be made clear that the VM should not be running when you start run.rkt on the host later
On the pkg-build readme instructions on Running a Build, I did:
replace the #:name string with the name of the VM as normal
replace the #:host IP address string with the hostname -I after running the ... down and ... up commands, as I said about step iv of Prerequisites
replace the #:snapshot-url string with the url string returned by the python http server still running on the host, including the port with the colon
replace the #:installer-platform-name string with "localhost"
before running run.rkt on the host, make sure the VM is shut down and the python http server is still running

- - -

From paper, "How to pkg-build"

1. dont need to download and build probably want snapshot don't need vm
   (may want to use vm anyway but its optional until pkg-build step)
2. not human readable may crash editor provide script
5. fails if you run make first so be sure its clean SRC_CATALOG=
7. python -m http.server ___
   3 works maybe 2.5 too default port maybe fine, 80 error, 8000 ok
   build/site should contain intex.html = download racket page
   ... other things?

follow pkg-build do vbox stuff
- host-only network
- hostname -I 192.___

[ ] can improve "echo hello" timeout for missing auth? forward prompt to user?

