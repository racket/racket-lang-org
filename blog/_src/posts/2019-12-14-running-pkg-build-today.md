    Title: Running a `pkg-build` today
    Date: 2019-12-14T17:47:39
    Tags: 

*posted by Fred Fu, Ben Greenman, and Alex Knauth*

How to run a pkg-build

<!-- more -->

pkg-build is a service to run all Racket packages

if you have a change to Racket and want to test agaist all packages then you
 want to use pkg-build

#### How it works, Basics


package catalogs
easy to get a list

VM because

(Ok you can go ahead now and follow instructions if you like)


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


