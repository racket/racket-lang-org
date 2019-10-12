#lang plt-web
(require "../www/resources.rkt"
         "../testing.rkt"
         "../identity.rkt"
         plt-web/style)

(provide installers)

(define snapshot-site (site "snapshot"
                            #:url (rewrite-for-testing "https://snapshot.racket-lang.org/")
                            #:page-headers (identity-headers)
                            #:share-from www-site))

;; Old:
(define pre-site (site "pre"
                       #:url (rewrite-for-testing "https://pre.racket-lang.org/")
                       #:page-headers (identity-headers)
                       #:share-from www-site))

(register-identity pre-site)

(define (main path #:site [site pre-site])
  @page[#:site site
        #:file path
        #:title "Racket Snapshots"
        #:width 'full]{
  @columns[10 #:center? #t #:row? #t #:center-text? #f]{
   @h3{Snapshot Builds}}
  @columns[8 #:center? #t #:row? #t #:center-text? #f]{
      @ul{@li{@a[href: "https://www.cs.utah.edu/plt/snapshots/"]{
                University of Utah}}
          @li{@a[href: "http://plt.eecs.northwestern.edu/snapshots/"]{
                Northwestern University}}}}})

(define installers (main "index.html" #:site snapshot-site))

;; At the old site, generate at both "installers/" (traditional path)
;; and "index.html" (old entry point, now subsumed). And
;; "installers.html", too:
(void (main "installers/index.html"))
(void (main "installers.html"))
(void (main "index.html"))
