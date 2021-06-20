#lang plt-web

(require "resources.rkt"
         "../identity.rkt"
         plt-web/style)

(provide index)

(register-identity lists-site)

(struct ML (name gmane-name google-name description mailman?))

(define MLs
  (list (ML "users" "user" "racket-users"
            @text{A discussion list for all things related to Racket.
                  Ask your questions here!}
            #f)
        (ML "announce" "announce" "racket-announcements"
            @text{A low-volume, moderated list for announcements, only.
                  @small{(These are posted on the @TT{users} list too, no need
                         to subscribe to both.)}}
            #f)
        (ML "dev" "devel" "racket-dev"
            @text{A mailing list for Racket development.
                  @small{(For people who want to see how the sausages are made
                         — and help make them.)}}
            #f)))

(define index
  @page[#:site lists-site
        #:link-title "Lists" #:window-title "Racket Mailing Lists"
        ;; attrs from old page. still necessary / useful? -- JBC, 2015-02-06
        ;#:title "Mailing Lists" #:file "" #:part-of 'community
        ;#:description
        ;@'{Racket mailing lists for users, developers, announcements, and more.}
        ;; copied from downloads...
        #:part-of 'lists #:width 'full
        (define (list-cells what) (map (λ (r) (r what)) list-renderers))
    ]{
    @columns[12 #:center? #t #:row? #t]{
      @p{We have several public mailing lists.}}
    @(define gap1 (tr (map (λ (_) @td{@div[style: "height: 1ex;"]{}}) MLs)))
    @(define gap2 (tr (map (λ (_) @td{}) MLs)))
    @(define (sec . text)
       @list{@gap1
             @tr{@td[style: '("background-color: #dddddd; font-weight: bold;"
                              " padding: 0;")
                     colspan: (add1 (length MLs))]{@text}}
             @gap2})
    @columns[12 #:center? #t #:row? #t]{
    @table[style: "width: 100%; margin: auto; text-align: center;"
           frame: 'box rules: 'cols cellpadding: 5]{
      @tr[style: "border-bottom: 1px solid; background-color: #ccccff;"]{
        @td[] @(list-cells 'header-cell)}
      @tr[valign: 'top style: "text-align: left;"]{@td[] @(list-cells 'description)}
      @tr{@td[] @(list-cells 'main-page-cell)}
      @sec{Subscribe to a mailing list}
      @tr{@td[] @(list-cells 'subscribe-cell)}
      @sec{Archive at mail-archive.com}
      @tr{@td[] @(list-cells 'mail-archive-cell)}}}})

;; given a mailing list structure, produce a renderer that can produce
;; the required components on demand
(define (list-renderer ml)
  (define name (ML-name ml))
  (define at-domain "@racket-lang.org")
  (define email (list name at-domain))
  (define description (ML-description ml))
  (define gmane
    (let ([gm (ML-gmane-name ml)])
      (and gm @list{gmane.comp.lang.racket.@(ML-gmane-name ml)})))
  (define (gmane-link base . body)
    (unless gmane
      (error 'list-renderer "internal error: no gmane info for ~a" name))
    (let* ([path (if (pair? base) (list "/" (cdr base) "/") "/")]
           [base (if (pair? base) (car base) base)]
           [pfx (if (regexp-match? #rx"://" base) base (list "http://" base))])
      @a[href: (list pfx ".gmane.io" path gmane)]{@body}))
  (define (mail-archive-link suffix . body)
    @a[href: (list "https://www.mail-archive.com/" email "/" suffix)]{@body})
  (define google-groups-name
    (ML-google-name ml))
  (define google-groups-url
    (and google-groups-name (list "https://groups.google.com/forum/#!forum/" google-groups-name "/")))
  (define google-groups-join-url
    (and google-groups-url
         (append google-groups-url (list "join"))))
  (define google-groups-join-text
    @span[style: "font-style: italic"]{With Google account: })
  (define google-groups-join-link-text
    (string-append "join the "name" mailing list"))
  (define google-groups-join-no-account-text
    (and google-groups-name
         @span{@span[style: "font-style: italic"]{Without Google account: }send email to @tt[google-groups-name "+subscribe@googlegroups.com"]}))
  (define ((mk-form make) url #:method [method 'get] . body)
    (make @form[action: url method: method
                style: "display: inline; clear: none;"]{
            @div{@body}}))
  ;; this form URL is unused for google groups...
  (define (mk-subscribe mk)
    (cond
      [(ML-mailman? ml)
       @(mk-form mk)[(list (url-of index #t) name "/subscribe") #:method 'post]{
                       @input[type: 'text name: 'email size: 20 value: ""
                                    placeholder: "Email to Subscribe"
                                    title: @list{Enter your email to subscribe
                                                 to the "@name" mailing list.}]}]
      [else ;; it must be a google group
       @td{@div{@|google-groups-join-text|@a[href: google-groups-join-url]{@google-groups-join-link-text}}
           @div{@nbsp}
           @div[google-groups-join-no-account-text]}]))
  (define form-cell (mk-form td))
  (λ (what)
    (case what
      [(header-cell)
       @th[style: "width: 33%;"]{
         @; the mixed styles will help against some spam harvesting too
         @span[style: '("font-size: x-large;"
                        " font-family: monospace; font-weight: bold;")]{
           @TT{@name}@;
           @span[style: "font-size: small;"]{@at-domain}}}]
      [(description) @td{@description}]
      [(main-page-cell)
       (cond
         [(ML-mailman? ml) 
          @td{@a[href: (list name "/")]{@big{@b{@TT{@name}}} page}
               @bull
               @a[href: (list name "/archive/")]{archive}}]
         [else 
          (unless google-groups-url
            (error 'main-page-cell-renderer 
                   "must have a google group URL for non-mailman list"))
          @td{@a[href: google-groups-url]{@big{@b{@TT{@name}}} page}
               @bull
               @a[href: (list name "/archive/")]{old archive}}])]
      [(subscribe-cell) (mk-subscribe td)]
      #;
      [(google-cell)
       (if google-groups-url
         @form-cell[(list google-groups-url "search")]{
           @a[href: google-groups-url]{
             @(string-titlecase
               (regexp-replace* #rx"-" (ML-google-name ml) " "))}
           @br
           @span[style: "white-space: nowrap;"]{
             Search: @input[type: 'text name: 'q value: "" size: 20].}}
         @td{@small{—none—}})]
      [(mail-archive-cell)
       @form-cell["https://www.mail-archive.com/search"]{
         @input[type: 'hidden name: 'l value: email]
         @mail-archive-link["info.html"]{Archive}
         @bull
         @mail-archive-link[""]{Browse}
         @bull
         @mail-archive-link["maillist.xml"]{RSS}
         @br
         @span[style: "white-space: nowrap;"]{
           Search: @input[type: 'text name: 'q value: "" size: 20].}}]
      ;; looks like gmane is pretty much gone...
      #;
      [(gmane-cell)
       @form-cell["http://search.gmane.org/"]{
         @input[type: 'hidden name: 'group value: gmane]
         @gmane-link["dir"]{@TT{@gmane}}
         @br
         @gmane-link["news"]{threaded}
         @bull
         @gmane-link["blog"]{blog}
         @bull
         @gmane-link["nntp://news"]{newsgroup}
         @br
         Feed:
         @gmane-link['("rss" "messages/complete")]{messages}
         @bull
         @gmane-link['("rss" "topics/complete")]{topics}
         @br
         @;Excerpt feed:
         @;@gmane-link['("rss" "messages/excerpts")]{messages}
         @;@bull
         @;@gmane-link['("rss" "topics/excerpts")]{topics}
         @;@br
         @span[style: "white-space: nowrap;"]{
           Search: @input[type: 'text name: 'query value: "" size: 20].}}]
      [(quick)
       @text{
         @big{@TT{@b{@name}}@small{@tt{@at-domain}}}
         @div[style: "margin-left: 2em;"]{
           @description
           @br
           @div[style: "float: right;"]{@(mk-subscribe values)}
           [@a[href: (list (url-of index) name "/")]{list page},
            @gmane-link["dir"]{gmane mirror},
            @mail-archive-link[""]{mail-archive}@;
            @(and google-groups-url
                  @text{, @a[href: google-groups-url]{google group mirror}})]}}]
      [else (error 'list-cell "internal error")])))

(define list-renderers (map list-renderer MLs))

(define (mailing-lists-quick)
  @text{@(apply parlist @strong{Mailing Lists}
                (map (λ (r) (r 'quick)) list-renderers))
        @p{See the @index{mailing list server} for more details.}})
