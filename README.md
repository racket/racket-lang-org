# racket-lang-org

This the source for the Racket package: "racket-lang-org".

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

 [See "upload" and `--dry-run` for an alternative]

* The simplest way to build the whole site is with 

  [the below command requires you to first install the racket-lang-org directory as a package]

  racket -l- racket-lang-org/sync --save-temps --render-locally Web ; open Web/www/index.html

  This renders the site in some temp directory and then moves the directory
  to 'Web` here. It will also open the front-page index file in your
  default browser on the Mac.

* `pollen` problems 

  `pollen` occasionally fails with really strange "deep in the guts"
  error messages. `pollen` changes and doesn't work right with its old
  files. After double-checking that you have everything in a new
  commit, does **DANGER!** `git clean -d -x -f`. 

  **DANGER!** Really make sure that you don't have any files you want
  to keep that aren't committed! This command also rm's uncommitted files. 

* Run "racket all.rkt -o <dir>" to build all pages to subdirectories of
  <dir>. As a safety measure, the target directory must not overlap
  with any installed directory. (Use `-f' in scripts to avoid
  answering the question about deleting existing files.)

* Running any "*.rkt" might build a page and things that it
  references. (Use `-h` as usual.) That's less true for newer pages.

* For older pages, you can choose `-w` (the default) for web mode,
  `-l` for local using "file://" references, or `-r` for' local mode
  using relative references. Normally, you'll want to use `-l` for
  testing, and then use `-w` for deployment.

* Set the $GIT_DIR environment variable to point to the ".git"
  directory of a Racket repository if you want to extract release
  information from a repository other than the enclosing one.

To upload:
==========

* You'll need credentials to upload to S3, and those credentials
  should be in "~/.aws-keys".

* You'll need the "s3-sync" Racket package installed.

* Run the "sync.rkt" script: racket -l- racket-lang-org/sync

* To build without uploading, use `--save-temps --render-locally <directory-name>`. If you
  don't have AWS credentials, the pages will still build, and look for
  "Files so far written to" for the temporary directory that contains
  the rendered pages.

### Contributing

Contribute to Racket by submitting a [pull request], reporting an
[issue], joining the [development mailing list], or visiting the
IRC or Slack channels.

### License

Racket, including these packages, is free software, see [LICENSE]
for more details.

By making a contribution, you are agreeing that your contribution
is licensed under the [Apache 2.0] license and the [MIT] license.

[MIT]: https://github.com/racket/racket/blob/master/racket/src/LICENSE-MIT.txt
[Apache 2.0]: https://www.apache.org/licenses/LICENSE-2.0.txt
[pull request]: https://github.com/racket/racket-lang-org/pulls
[issue]: https://github.com/racket/racket-lang-org/issues
[development mailing list]: https://lists.racket-lang.org
[LICENSE]: LICENSE
