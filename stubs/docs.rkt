#lang plt-web

(require (only-in "../www/resources.rkt" www-site)
         "../testing.rkt"
         plt-web/private/image-version)

(define docs-site (site "stubs/docs" 
                        #:url (rewrite-for-testing "https://docs.racket-lang.org/")
                        #:always-abs-url? #t
                        #:page-style? #f
                        #:meta? #t
                        #:share-from www-site))

(provide documentation)
(define documentation
  ;; This is a stub page to get the header for tweaked doc pages
  (page #:site docs-site
        #:file "" #:link-title "Documentation" #:window-title "{{{TITLE}}}"
        "\n{{{BODY}}}\n"))

(void
 (plain #:site docs-site
        #:file "doc-site.js"
        @list{
              function AddLogoToMainDiv()
              {
               var main_div = document.getElementsByClassName("main")[0];
               var h = document.createElement('div');
               h.setAttribute("class", "docsite-logo");
               h.innerHTML = "<a href=\"@(url-of (resource "www/" #f))\"><img src=\"@(url-of (resource "www/" #f))@(format "logo-and-text~a.png" (image-version-suffix))\" alt=\"Racket\" /></a>";
               main_div.insertBefore(h, main_div.firstChild);
               }
              AddOnLoad(AddLogoToMainDiv);
              }))
(void
 (plain #:site docs-site
        #:file "doc-site.css"
        @list{
              .docsite-logo
              {
               float: right;
               position: relative;
               top: -2.5em;
               left: 1.3em;
               }
              .docsite-logo img
              {
               width: 8em;
               }}))

(provide docs-path)
(define (docs-path [path ""])
  (string-append ((resource "stubs/docs/" #f)) path))