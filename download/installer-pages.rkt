#lang plt-web

(require "resources.rkt" "data.rkt" "mirror-link.rkt" "util.rkt" plt-web/style)

(define (render-installer-page installer)
  (define path      (installer-path installer))
  (define file      (installer-file installer))
  (define html-file (string-append (regexp-replace* #rx"\\." file "-") ".html"))
  (define release   (installer-release installer))
  (define version   (release-version release))
  (define date      (release-date-string release))
  (define package   (package->name (installer-package installer)))
  (define size      (installer-size installer))
  (define type      "") ;; (if (installer-binary? installer) "" " source")
  (define platform  (platform->name (installer-platform installer) package))
  (define variant   (installer-variant installer))
  (define title     @text{Download @package v@|version type| for @platform})
  (define suffix-desc (suffix->name (installer-suffix installer)))
  (define human-size (get-human-size size))
  (define (row label text)
    @tr[valign: 'top]{
      @td[align: 'right]{@b{@label}:}
      @td{@nbsp}
      @td[align: 'left]{@text}})
  (define (this url [mode #f])
    (case mode
      [(only-platform) (a href: url platform type)] ; when JS is not available
      [(get-url) url] ; when JS is available
      [(#f) @a[href: url]{@title}]
      [else (error 'installer-page "unknown mode: ~e" mode)]))
  @page[#:site download-site 
        #:file html-file #:title title #:referrer this #:part-of 'download]{
    @columns[10 #:center? #t #:row? #t #:center-text? #t]{
      @table[align: 'center style: "border: none;"]{
        @tr[valign: 'top]{
          @td[width: "50%" style: "border: none;"]{
            @table[class: "striped rounded"]{
                   @(row "Package"  package)
                   @(row "Version"  @list{@version (@date)})
                   @(row "Platform" (list platform type))
                   @(row "Variant"  (list variant))
                   @(row "Type"     suffix-desc)
                   @(row "File"     file)
                   @(row "Size"     @span[title: @list{Exact size: @size bytes}]{
                                    @human-size})}}
        @td[width: "50%" style: "border: none;"]{
          Download links:
          @div[style: "font-size: 75%; text-align: right; float: right;"]{
            (Choose the nearest site)}
          @ul{@(let ([mirrors
                      (filter-map
                       (λ (m)
                         (define url
                           (mirror-link
                            (string-append (mirror-url* m) path)
                            size
                            (λ () (format "~a <~a>"
                                          (mirror-person m)
                                          (mirror-email m)))))
                         (and url @li{@a[href: url]{@(mirror-location m)}}))
                       mirrors)])
                 (case (length mirrors)
                   [(0) (error 'installer-page "no available mirror for: ~e"
                               path)]
                   [(1) (list mirrors
                              @li{@small{(no additional mirrors, yet)}})]
                   [else mirrors]))}}}}
    @;TODO: decide whether this is really needed
    @; (looks redundant now that all of the installers are pretty standard)
    @;section{Installation instructions}
    @;(bundle-installation-instructions bundle)
    }})

(provide mirror-url*)
(define (mirror-url* m)
  (define u (if (eq? m 'main) m (mirror-url m)))
  (if (eq? u 'main)
      ;; redirects via S3 routing rules to the first mirror:
      ((resource "download/installers/" #f))
      u))

(provide installer->page)
(define installer->page
  (let ([t (make-hasheq)])
    (λ (inst . more)
      (let ([page (hash-ref! t inst (λ () (render-installer-page inst)))])
        (if (null? more) page (apply page more))))))
