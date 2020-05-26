#lang plt-web

(require "resources.rkt" "data.rkt" "installer-pages.rkt" "symlinks.rkt"
         plt-web/style
         version/utils
         net/base64
         (prefix-in pre: "../minis/pre.rkt"))

(define docs "docs")

(define releases "releases")
(define first-version-with-releases-page "5.92")
(define first-version-with-generic-linux "6.5")
(define version-with-touchbar-bug "6.7")

(define (encode s)
  (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 s) #"")))

(provide render-download-page)
(define (render-download-page [release current-release] [package 'racket]
                              #:at-download [at-download list])
  (define version (release-version release))
  (define all-packages (sort (hash-map (for/hash ([i (in-list all-installers)]
                                                  #:when (equal? release (installer-release i)))
                                         (values (installer-package i)
                                                 (installer->page i 'render-package-option)))
                                       cons)
                             (lambda (a b)
                               (cond
                                [(equal? (car a) package) #t]
                                [(equal? (car b) package) #f]
                                [else (string<? (cdr a) (cdr b))]))))
  (define (literal-link url) @a[href: url url])
  (define note-style '("font-size: 85%; display: none;"
                       " margin-top: 1ex;"
                       " padding: 1ex 1ex 1ex 1ex;"
                       " text-align: left;"
                       " line-height: 1.5em; "
                       " background-color: #edd"))
  (define was-initial-variant? #t)
  (define initial-platform #f)
  (list
   @columns[10 #:center? #t #:row? #t #:center-text? #t]{
    @h3[style: "text-align: center"]{Version @version (@(release-date-string release))}
    @div[id: "download_panel" align: "center" style: "display: none; margin-bottom: 20px;"]{
      @div[align: "center"]{
        Distribution:
        @select[id: "package_selector"
                onchange:   "selection_changed();"
                onkeypress: "selection_changed();"]{
          @(for/list ([i (in-list all-packages)])
             (cdr i))}
        @(for/list ([i (in-list all-packages)]
                    [n (in-naturals)])
           (define this-package (car i))
           @div[id: (format "platform_selector_panel_~a" this-package)
                style: (if (zero? n) "display: block;" "display: none;")]{
              Platform:
              @select[id: (format "platform_selector_~a" this-package)
                      onchange:   "selection_changed();"
                      onkeypress: "selection_changed();"]{                                                          
                @(for/list ([i (in-list all-installers)]
                            #:when (and (equal? release (installer-release i))
                                        (equal? this-package (installer-package i))
                                        (equal? "Regular" (installer-variant i))))
                   (installer->page i 'render-direct-option))}
              @(for/list ([i (in-list all-installers)]
                          #:when (and (equal? release (installer-release i))
                                      (equal? this-package (installer-package i))
                                      (equal? "Regular" (installer-variant i))))
                 (define initial-variant? was-initial-variant?)
                 (define variants (for/list ([j (in-list all-installers)]
                                             #:when (and (equal? release (installer-release j))
                                                         (equal? this-package (installer-package j))
                                                         (equal? (installer-platform i) (installer-platform j))))
                                    j))
                 (unless initial-platform (set! initial-platform (installer-platform i)))
                 (set! was-initial-variant? #f)
                 (cond
                   [(= 1 (length variants)) ""]
                   [else
                    @div[id: (format "variant_selector_panel_~a~a" this-package (encode (platform->name (installer-platform i) package)))
                         style: (if initial-variant? "display: block;" "display: none;")]{
                      Variant:
                      @select[id: (format "variant_selector_~a~a" this-package (encode (platform->name (installer-platform i) package)))
                              onchange:   "selection_changed();"
                              onkeypress: "selection_changed();"]{
                      @(for/list ([i (in-list variants)])
                         (installer->page i 'render-direct-variant-option))}}]))})}
      @br
      @navigation-button[@(a href: (resource "download/" #f)
                             id: "download_link"
                             "Download")]
      @br
      or @a[href: (resource "download/" #f) id: "mirror_link"]{mirror}
      @span[id: "linux_ppa"]{
        or
        @a[href: "https://launchpad.net/~plt/+archive/ubuntu/racket"]{Ubuntu PPA}}
     }}
  @columns[8 #:center? #t #:center-text? #t #:row? #t]{
      @(let* ([sep   @list{@nbsp @bull @nbsp}]
              [links (λ links @(div style: "margin: 1ex 4ex;" (add-between links sep)))]
              [docs  (html-docs-link version)])
         (list
          @row{
            @links[@list{Release: @nbsp @(release-page release){Announcement}}
                   @a[href: @at-download{@|docs|/release/}]{Notes}
                   @list{@a[href: @at-download{@docs}]{Documentation}
                         @(if (version<? @|version| first-version-with-releases-page)
                              null
                              @list{@br @nbsp @a[href: @at-download{@|releases|/@version}]{More Variants and Checksums}})}]}
          @row{@links[@license{License}
                       all-version-pages
                       @pre:installers{Snapshot Builds}]}))}
  @columns[6 #:center? #t #:center-text? #t #:row? #t]{
      @div[id: "minimal_racket_explain"
           style: note-style]{
        @div{@b{About Minimal Racket:}} Minimal Racket includes just enough of Racket that you can use
        @div{@nbsp @nbsp @tt{raco pkg}} to install more.}
      @div[id: "linux_explain"
           style: note-style]{
        @div{@b{About the Linux installers:}}
        @(if (version<? @|version| first-version-with-generic-linux)
             @list{If you don't see an option for
                   your particular platform, try other Linux installers, starting from
                   similar ones.  Very often, a build on one Linux variant will work on
                   others too.}
             @list{The Linux build is generic enough that it should work on most
                   distributions, including relatively old distributions.})}
      @(if (equal? version version-with-touchbar-bug)
           @div[id: "macos_touchbar_explain"
                style: note-style]{
                @div{@b{MacBook Pro with Touch Bar users:}}
                @list{To avoid a bug in this version that prevents programs from working
                      with the Touch Bar, use the 32-bit version or try a
                      @pre:installers{snapshot build}.}}
           null)
      @div[id: "builtpkgs_explain"
           style: note-style]{
        @div{@b{About source distributions:}} The @b{Source + built packages}
           distribution is recommended, instead of the selected @b{Source} distribution.
           The @b{Source + built packages} distribution includes pre-built,
           platform-independent bytecode@";" it installs much faster than
           plain source, and it is also compatible with fast installs of
           additional Racket packages.}
      @div[id: "source_explain"
           style: note-style]{
        @div{@b{About sources for Windows and Mac OS:}} To build from source for
           Windows or Mac OS, download and build a @b{Minimal Racket} distribution
           instead of a @b{Racket} distribution, then (when on Windows) install the @tt{racket-lib} package
           with @div{@nbsp @nbsp @tt{raco pkg update --auto racket-lib}}
           and then (on both Windows and Mac OS) install packages
           with @div{@nbsp @nbsp @tt{raco pkg install -i main-distribution}}}
      @div[id: "win_source_explain"
           style: note-style]{
        @div{@b{About source builds on Windows:}} After building @b{Minimal Racket}
           from source, install the @tt{racket-lib} package with
           with @div{@nbsp @nbsp @tt{raco pkg update --auto racket-lib}}
           before installing other packages.}
    @downloader-script[package initial-platform (map car all-packages) version]
    @noscript{
      Installers are available for the following platforms:
      @ul{@(for/list ([i (in-list all-installers)]
                      #:when (and (equal? release (installer-release i))
                                  (equal? package (installer-package i))))
             @li{@(installer->page i 'only-platform)})}}}))

(define (release-page* rel)
  (define ver (release-version rel))
  (define title @list{v@ver Release Notes})
  @page[#:site download-site
        #:file (format "v~a.html" ver) #:title title #:part-of 'download]{
    @table[align: 'center]{
      @tr{@td{@h2{Release Announcement for Version @ver}}}
      @tr{@td{@pre{@release-announcement[rel]}}}}
  })
(define release-page
  (let ([t (make-hash)])
    (λ (rel) (hash-ref! t rel (λ () (release-page* rel))))))

(define (html-docs-link ver)
  (if (version<? ver first-version-with-releases-page)
      @list{@|docs|/@|ver|/html}
      @list{@|releases|/@|ver|/doc}))

(define (pdf-docs-link ver)
  (if (version<? ver first-version-with-releases-page)
      @list{@|docs|/@|ver|/pdf}
      @list{@|releases|/@|ver|/pdf-doc}))

(define all-version-pages
  (let ()
    (define (make-page rel pkg)
      (define ver   (release-version rel))
      (define file  (format "~a-v~a.html" pkg ver))
      (define title @list{Download @(package->name pkg) v@ver})
      @page[#:site download-site #:file file #:title title #:width 'full #:part-of 'download]{
        @(render-download-page rel pkg)})
    (define style
      @style/inline[type: 'text/css]{
       })
    (define-values (main-package alt-packages)
      (cond [(null? all-packages)
             (eprintf "Warning: all-packages is empty\n")
             (values 'racket null)]
            [else
             (values (car all-packages) (cdr all-packages))]))
    @page[#:site download-site
          #:id 'all-versions #:title "All Versions" #:part-of 'download
          #:extra-headers style #:width 'full]{
     @columns[10 #:center? #t #:row? #t]{
      @table[class: "striped rounded"]{
        @thead{
          @tr{@th{Version}
              @th{Announcement}
              @th{Download}
              @th{Alternative}
              @th{Documentation}}}
       @tbody{
        @(let ([sep null])
           ;; release=>packages : hash[release => (listof package)]
           ;; Indicates what packages actually exist (have installers) for a given release.
           (define release=>packages (make-hash))
           (for ([i (in-list all-installers)])
             (define r (installer-release i))
             (define prev-packages (hash-ref release=>packages r null))
             (unless (member (installer-package i) prev-packages)
               (hash-set! release=>packages r (cons (installer-package i) prev-packages))))
           (define (cell rel pkg)
             @td[align: 'center]{
               @(make-page rel pkg){@(package->name main-package)}})
             @sep
             @(for/list ([r (in-list all-releases)])
                (define ver (release-version r))
                  @list{
                    @tr[class: 'version-row]{
                      @td{@strong{Version @ver}}
                      @td{@span[style: "font-size: 80%"]{@(release-page r){@release-date-string[r]}}}
                      @(if (member main-package (hash-ref release=>packages r))
                           (cell r main-package)
                           @td[])
                      @td[align: 'center]{
                        @(add-between
                          (for/list ([p (in-list alt-packages)]
                                     #:when (member p (hash-ref release=>packages r)))
                            ((make-page r p) (package->name p)))
                          " ")}
                      @td{@a[href: @html-docs-link[ver]]{[HTML]} @;
                          @nbsp @;
                          @a[href: @pdf-docs-link[ver]]{[PDF]}}}
                    @sep}))
          @tr[class: 'version-row]{
            @td{Development}
            @td{}
            @td{@pre:installers{Snapshots}}
            @td{}
            @td{}}}}}}))

(define license
  @page[#:site download-site
        #:title "Software License" #:part-of 'download]{
    @columns[10 #:center? #t #:row? #t]{
    @p*{
    @~ Racket is primarily distributed under the 
       @a[href: "https://www.apache.org/licenses/LICENSE-2.0"]{
         Apache License, version 2.0} and the
       @a[href: "https://github.com/racket/racket/blob/master/racket/src/LICENSE-MIT.txt"]{
         MIT License}, at your option.
       Some components of the Racket distribution, including the
       compiled @tt{racket} executable for the "Regular" variant of
       Racket and packages that distribute
       third-party libraries, are distributed under the
       @a[href: "http://www.gnu.org/licenses/lgpl-3.0.html"]{
         GNU Lesser General Public License (LGPL) version 3.0}.
    @~ See the @a[href: "https://github.com/racket/racket/blob/master/LICENSE"]{
       @tt{LICENSE}} file in the source code for more information.
                                                                                    @~ Any program written in Racket that does not distribute the
    "Regular" variant @tt{racket} binary itself is not affected by the
    license of that binary.

    Versions of Racket prior to version 7.5 are distributed under the
    GNU LGPL, version 3.0.
    }}})

(define (downloader-script package initial-platform packages version)
  @script/inline[type: 'text/javascript]{@||
    var selection_changed;
    var package_selector = document.getElementById("package_selector");
    var packages = [@(string-join (map (lambda (s) (format "\"~s\"" s)) packages) ", ")];
    var current_package = "@|package|";
    var current_platform = "@(or initial-platform "")";
    (function() {
    // show the download panel, since JS is obviously working
    document.getElementById("download_panel").style.display = "block";
    //
    var selector = document.getElementById("platform_selector_@|package|");
    // returns an ordering for the platform names, an array of regexps
    // note that the entries are sorted in a good order, so return an order
    // that only brings the locally desired entries to the top
    function getPlatformOrder() {
      var p = navigator.platform;
      var p2 = navigator.appVersion;
      var p3 = navigator.userAgent;
      var l = function(str) { return (p.indexOf(str) != -1) || (p2.indexOf(str) != -1) || (p3.indexOf(str) != -1) @";" }
      var Win      = /Windows/,
          Win64    = /Windows.*64/,
          Win32    = /Windows.*32/,
          Mac      = /Mac/,
          MacIntel = /Mac.*Intel/,
          MacIntel64 = /Mac.*Intel.*64/,
          MacIntel32 = /Mac.*Intel.*32/,
          MacPPC   = /Mac.*PPC/,
          Linux    = /Linux/,
          Linux64  = /Linux.*x86_64/,
          Linux32  = /Linux.*i386/,
          Unix     = /Unix/,
          Solaris  = /Solaris/;
      if (p == null) return [];
      else if (l("SunOS")) return [Solaris, Unix];
      else if (l("Win64")) return [Win64, Win];
      else if (l("WOW64")) return [Win64, Win];
      else if (l("Win"))   return [Win32, Win];
      else if (l("Mac"))   return [(l("Intel")?MacIntel64:MacPPC), (l("Intel")?MacIntel32:MacPPC), Mac, Unix];
      else if (l("Linux")) {
        // also show the linux explanation if it's a linux
        document.getElementById("linux_explain").style.display = "block";
        document.getElementById("linux_ppa").style.display = "block";
        return [(l("_64")?Linux64:Linux32), Linux, Unix];
      } else return [];
    }
    // show the linux explanation on change too (do it with a timeout so it
    // changes even when the arrow keys are used to move the selection -- since
    // then onchange is called only on blur)
    linux_expl_s = document.getElementById("linux_explain").style;
    linux_ppa_s = document.getElementById("linux_ppa").style;
    source_expl_s = document.getElementById("source_explain").style;
    win_source_expl_s = document.getElementById("win_source_explain").style;
    minimal_racket_expl_s = document.getElementById("minimal_racket_explain").style;
    builtpkgs_expl_s = document.getElementById("builtpkgs_explain").style;
    @(if (equal? version version-with-touchbar-bug)
         @list{
            macos_touchbar_expl_s = document.getElementById("macos_touchbar_explain").style;
         }
         null)
    selection_changed_timer = false;
    selection_changed = function() {
      var package = packages[package_selector.selectedIndex];
      var old_package = current_package;
      if (current_package != package) {
         var panel = document.getElementById("platform_selector_panel_" + current_package);
         panel.style.display = "none";
         current_package = package;
         panel = document.getElementById("platform_selector_panel_" + package);
         panel.style.display = "block";
         selector = document.getElementById("platform_selector_" + package);
      }
      var download_link = document.getElementById("download_link");
      var selected = selector[selector.selectedIndex];
      var old_variant_panel = document.getElementById("variant_selector_panel_" + old_package + btoa(current_platform));
      if (old_variant_panel)
         old_variant_panel.style.display = "none";
      current_platform = selected.textContent;
      var variant_panel = document.getElementById("variant_selector_panel_" + current_package + btoa(current_platform));
      if (variant_panel)
         variant_panel.style.display = "block";
      var variant_selector = document.getElementById("variant_selector_" + current_package + btoa(current_platform));
      if (variant_selector)
         selected = variant_selector[variant_selector.selectedIndex];
      var path = selected.value;
      download_link.href = path;
      download_link.innerHTML = path.replace(/.*\//, "") + " (" + selected.getAttribute("x-installer-size") + ")";
      var mirror_link = document.getElementById("mirror_link");
      mirror_link.href = selected.getAttribute("x-mirror");
      if (selection_changed_timer) clearTimeout(selection_changed_timer);
      selection_changed_timer = setTimeout(do_selection_changed, 250);
    }
    function some_selector_matches(rx) {
       for (i = 0@";" i < selector.length@";" i++) {
         if (selector[i].text.search(rx) >= 0)
          return true;
       }
       return false;
    }
    function do_selection_changed() {
      linux_expl_s.display =
        (selector[selector.selectedIndex].text.search(/Linux/) >= 0)
        ? "block"
        : "none";
      linux_ppa_s.display =
        (selector[selector.selectedIndex].text.search(/Linux/) >= 0)
        ? "block"
        : "none";
      source_expl_s.display =
        (selector[selector.selectedIndex].text.search(/Unix Source/) >= 0
         && !some_selector_matches(/(Windows|Mac OS) Source/))
        ? "block"
        : "none";
      win_source_expl_s.display =
        (selector[selector.selectedIndex].text.search(/Source/) >= 0
         && !some_selector_matches(/Unix Source/)
         && !some_selector_matches(/Windows Source/))
        ? "block"
        : "none";
      minimal_racket_expl_s.display =
        (package_selector[package_selector.selectedIndex].text.search(/Minimal/) >= 0)
        ? "block"
        : "none";
      builtpkgs_expl_s.display =
        (selector[selector.selectedIndex].text.search(/Source/) >= 0
         && selector[selector.selectedIndex].text.search(/built/) < 0
         && some_selector_matches(/built/))
        ? "block"
        : "none";
      @(if (equal? version version-with-touchbar-bug)
           @list{
             macos_touchbar_expl_s.display =
              (selector[selector.selectedIndex].text.search(/Mac.*64/) >= 0)
               ? "block"
               : "none";
           }
           null)
    }
    //
    function initialize_selector(selector) {
      var opts = selector.options;
      var len = opts.length;
      // get the order and a make a sorting function
      var order = getPlatformOrder();
      function getOrder(str) {
        for (var i=0@";" i<order.length@";" i++)
          if (str.search(order[i]) >= 0) return i;
        return 999;
      }
      function isBetter(opt1,opt2) {
        // sort first by the order, then by how they were placed originally
        var ord1 = getOrder(opt1[0]), ord2 = getOrder(opt2[0]);
             if (ord1 < ord2)       return -1;
        else if (ord1 > ord2)       return +1;
        else if (opt1[4] < opt2[4]) return -1;
        else if (opt1[4] > opt2[4]) return +1;
        else                        return  0;
      }
      // sort the options, need to use a temporary array
      var tmps = new Array(len);
      for (var i=0@";" i<len@";" i++)
        tmps[i]=[opts[i].text,
                 opts[i].value,
                 opts[i].getAttribute("x-installer-size"),
                 opts[i].getAttribute("x-mirror"),
                 i];
      tmps.sort(isBetter);
      for (var i=0@";" i<len@";" i++) {
        opts[i].text  = tmps[i][0];
        opts[i].value = tmps[i][1];
        opts[i].setAttribute("x-installer-size", tmps[i][2]);
        opts[i].setAttribute("x-mirror", tmps[i][3]);
      }
      opts.selectedIndex = 0;
    }
    for (var i = 0; i < packages.length; i++) {
      initialize_selector(document.getElementById("platform_selector_" + packages[i]));
    }
    selection_changed();
    })();
    @||})
