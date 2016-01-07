#lang racket/base

(provide sites)
(define sites
  '(("www"           "https://racket-lang.org/")
    ("download"      "https://download.racket-lang.org/")
    ("bugs"          "https://bugs.racket-lang.org/")
    ("lists"         "https://lists.racket-lang.org/")
    ("drracket"      "https://drracket.org/")
    ;; stubs usually use absolute paths for resources, since they're
    ;; templates that often get used in sub-dir pages too
    ("stubs/planet"  "https://planet.racket-lang.org/"   abs)
    ("stubs/pre"     "https://pre.racket-lang.org/"      abs)
    ("stubs/git"     "https://git.racket-lang.org/"      abs)
    ("stubs/blog"    "https://blog.racket-lang.org/"     abs)
    ("stubs/mailman" "https://lists.racket-lang.org/"    abs)
    ("stubs/dirlist" "https://download.racket-lang.org/" abs)
    ("stubs/docs"    "https://docs.racket-lang.org/"     abs)
    ("stubs/wiki"    "https://wiki.racket-lang.org/"     abs)))

(provide distributions)
(define distributions
  (make-parameter
   ;; Each is a "hostname:dest-path", and then a list of directories to put in
   ;; that path.  Warning: distributed directories are replicated from the
   ;; source, including removing material that is not distributed.  A directory
   ;; can also have "*" parts which will be expanded recursively -- useful to
   ;; avoid deletions in case a target directory has additional materials.
   '(["winooski:/www" "www" "download" "bugs" "lists" "drracket" "stubs"])))
