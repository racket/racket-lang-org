Sources:
========

* www: actual content of main site.

* blog: actual content of blog.

* download: content of download site (only installer pages and such)

* */all.rkt: files that just require all the necessary modules to build
  the whole site or sub-sites

* */resources.rkt: files that define the resources for a site (icon,
  css, logo)

* minis: smaller one-source-file sites

* stubs: template "sites" that are intended to be hooked into other
  systems to get the racket look.

To build:
=========

* Run "./all.rkt" to build all pages.

* Run any "*.rkt" to build a page and things that it references.
  (Use `-h' as usual.)

* You can choose `-w' (the default) for web mode, `-l' for local using
  "file://" references, or `-r` for' local mode using relative
  references. Normally, you'll want to use `-l' for testing, and then
  use `-w' for deployment.

* Use `-o <dir>' to specify a directory where the built contents is
  placed, otherwise the content will be placed in the current
  directory.  As a safety measure, the target directory must not
  overlap with any installed directory.  (Use `-f' in scripts to avoid
  answering the question about deleting existing files.)

* Set the $GIT_DIR environment variable to point to the ".git"
  directory of a Racket repository if you want to extract release
  information from a repository other than the enclosing one.

To upload:
==========

* You'll need credentials to upload to S3, and those credentials
  should be in "~/.aws-keys".

 * You'll need the "s3-sync" Racket package installed.

* Run the "sync.rkt" script: racket -l racket-lang-org/sync
