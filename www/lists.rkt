#lang plt-web

(require "resources.rkt"
         #;(prefix-in download: "../lists/index.rkt")
         #;"../lists/lists-pages.rkt"
         racket/runtime-path)

(provide lists)
(define lists lists:index)

;; is this relevant for lists? Assuming NOT? -- JBC, 2015-02-06
;; DELETE THIS IF WE CAN BUILD WITHOUT IT...
;; For old references that go to "www/lists/", make a copy of
;; the main download page:
#;(define www-lists-site (site "www/lists"
                             #:share-from www-site
                             #:meta? #f))
#;(void
 @page[#:site www-lists-site
       #:file "index.html"
       #:title "Lists" #:window-title "Racket Mailing Lists"
       #:part-of 'lists #:width 'full]{
    @(render-lists-page)})
