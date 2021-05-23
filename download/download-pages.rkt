#lang plt-web

(require "resources.rkt" "data.rkt" "installer-pages.rkt" "util.rkt"
         "symlinks.rkt" ; has side effect of registering some links
         racket/dict
         racket/match
         plt-web/style
         version/utils
         json
         (prefix-in pre: "../minis/pre.rkt"))

(define docs "docs")

(define releases "releases")
(define first-version-with-releases-page "5.92")
(define first-version-with-generic-linux "6.5")
(define first-version-without-alternative-page "8.0")
(define version-with-touchbar-bug "6.7")
(define version-before-m1-support "7.9")

;; use a list of cons instead of hash to preserve the order
(define (group-by/dict grouper xs f)
  (define all (group-by grouper xs))
  (for/list ([group (in-list all)])
    (cons (grouper (first group)) (f group))))

;; finalize-variant :: (listof installer?) -> (listof installer?)
;; when there are multiple installers, one of them must be exe,
;; and we prefer it over others.
(define (finalize-variant group)
  (define ret
    (match group
      [(list _) group]
      [_ (filter (λ (i) (equal? "exe" (installer-suffix i))) group)]))
  (unless (= (length ret) 1)
    (error 'finalize-variant "~a can't be processed" group))
  ret)

(provide render-download-page)
(define (render-download-page [release current-release] [package 'racket]
                              #:at-download [at-download list])
  (define version (release-version release))
  (define ok-release-installers
    (filter (λ (i) (equal? release (installer-release i))) all-installers))
  (define grouped-by-dist+platform
    (group-by/dict
     installer-package ok-release-installers
     (λ (group)
       (group-by/dict
        installer-platform group
        (λ (group)
          (append-map cdr
                      (group-by/dict installer-variant group finalize-variant)))))))

  (define note-style '("font-size: 85%; display: none;"
                       " margin-top: 1ex;"
                       " padding: 1ex 1ex 1ex 1ex;"
                       " text-align: left;"
                       " line-height: 1.5em; "
                       " background-color: #edd"))
  (define (more-installers)
    @a[href: @at-download{@|releases|/@version}]{More Installers and Checksums})
  (append
   (list
   @columns[10 #:center? #t #:row? #t #:center-text? #t]{
    @h3[style: "text-align: center"]{Version @version (@(release-date-string release))}
    @div[id: "download_panel" align: "center" style: "display: none; margin-bottom: 10px;"]{
      @div[id: "control"]{}
      @br
      @navigation-button[@(a href: (resource "download/" #f)
                             id: "download_link"
                             "Download")]
      @br
      or @a[href: (resource "download/" #f) id: "mirror_link"]{mirror}
      @span[id: "linux_ppa"]{
        or
        @a[href: "https://launchpad.net/~plt/+archive/ubuntu/racket"]{Ubuntu PPA}}
     }})
 (if (version<? @|version| first-version-with-releases-page)
     null
     (list
      @columns[8 #:center? #t #:center-text? #t #:row? #t]{
        @row[style: "margin: 1ex;"]{@more-installers[]}}))
 (list
  @columns[6 #:center? #t #:center-text? #t #:row? #t]{
      @div[id: "minimal_racket_explain"
           style: note-style]{
        @div{@b{About Minimal Racket:}} Minimal Racket includes just enough of Racket that you can use @tt{raco pkg} to install more.}
      @div[id: "linux_explain"
           style: note-style]{
        @div{@b{About the Linux installer:}}
        @(if (version<? @|version| first-version-with-generic-linux)
             @list{If you don't see an option for
                   your particular platform, try other Linux installers, starting from
                   similar ones.  Very often, a build on one Linux variant will work on
                   others too.}
             @list{The Linux build is generic enough that it should work on most
                   distributions, including relatively old distributions. Racket
                   may also be available through your distribution's package manager,
                   although it may be older than the latest Racket version.})}
      @div[id: "linux_install_explain"
           style: note-style]{
             @div{@b{Running the Linux installer:}}
             @list{After downloading the installer file, run it with
                         @div{@nbsp @nbsp @tt{sh @tt[id: "installer_name"]{racket.sh}}}
                         to install, possibly adding @tt{sudo} to the start of the command
                         to install to a location that requires adminstrator access.}}
      @(if (equal? version version-with-touchbar-bug)
           @div[id: "macos_touchbar_explain"
                style: note-style]{
                @div{@b{MacBook Pro with Touch Bar users:}}
                @list{To avoid a bug in this version that prevents programs from working
                      with the Touch Bar, use the 32-bit version or try a
                      @pre:installers{snapshot build}.}}
           null)
      @(cond
         [(version<=? version version-before-m1-support)
          @div[id: "m1_mac_explain"
               style: note-style]{
               @div{@b{M1 Mac users:}}
               @list{The latest version of Racket supports Apple Silicon directly.

               For version @version, the Mac OS (Intel 64-bit) variant requires macOS Big Sur 11.1 or above.}}]
         [(version<? version-before-m1-support version)
          @div[id: "m1_mac_explain"
               style: note-style]{
               @div{@b{M1 Mac users:}}
               @list{Select the @b{Apple Silicon} option in @b{Platform}.}}]
         [else null])
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
           @(if (not (for/or ([i (in-list all-installers)])
                       (and (equal? release (installer-release i))
                            (equal? 'racket-minimal (installer-package i)))))
                @span{ (see @more-installers[])}
                "")
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
    @script/inline[type: 'text/javascript]{

// higher-order functions

function map(xs, f) {
    var result = [];
    for (var i = 0; i < xs.length; i++) {
        result.push(f(xs[i], i));
    }
    return result;
}

function forEach(xs, f) {
    for (var i = 0; i < xs.length; i++) {
        f(xs[i], i);
    }
}

function filter(xs, f) {
    var result = [];
    for (var i = 0; i < xs.length; i++) {
        if (f(xs[i], i)) {
            result.push(xs[i]);
        }
    }
    return result;
}


// big-bang for HTML

// bigbang :: (HTMLElement, 'state, ('state -> Elem)) -> void

var bigbang = null;

// elem :: (HTMLType, Props, Array of (Elem or Text)) -> Elem
//
// type HTMLType = String
// type Props = Object; entries whose key starts with 'on' are considered handlers

var elem = null;

var property = null;

(function() {
    function Elem(type, attrs, children) {
        this.type = type;
        this.attrs = attrs;
        this.children = children;
        this.node = null;
        this.handlers = {};
    }

    function Text(s) {
        this.s = s;
        this.node = null;
    }

    function Property(value) {
        this.value = value;
    }

    var globalState = null;
    var rerender = null;
    var currentTree = null;

    function setAttribute(tree) {
        for (var key in tree.attrs) {
            var value = tree.attrs[key];
            if (key.substring(0, 2) === 'on') {
                key = key.substring(2);
                var closure = makeClosure(value);
                tree.handlers[key] = closure;
                if (tree.node.addEventListener) {
                    tree.node.addEventListener(key, closure, false);
                } else {
                    tree.node.attachEvent('on' + key, closure);
                }
            } else if (value instanceof Property) {
                tree.node[key] = value.value;
            } else {
                tree.node.setAttribute(key, value);
            }
        }
    }

    function makeClosure(value) {
        return function (e) {
            e.target = e.target || e.srcElement; // IE uses srcElement
            globalState = value(globalState, e);
            rerender();
        };
    }

    function render(tree) {
        if (tree instanceof Text) {
            tree.node = document.createTextNode(tree.s);
            return tree.node;
        }

        var e = document.createElement(tree.type);
        tree.node = e;
        setAttribute(tree);

        forEach(tree.children, function (child) {
            e.appendChild(render(child));
        });
        return e;
    }

    // here's a crappy reconciliation algorithm to avoid losing focus
    // update :: (Elem, Elem) -> void
    function update(oldTree, newTree) {
       var i = null;
       if (oldTree.type === newTree.type) {
           newTree.node = oldTree.node;
           for (var key in oldTree.attrs) {
               var value = oldTree.attrs[key];
               if (key.substring(0, 2) === 'on') {
                   key = key.substring(2);
                   var closure = oldTree.handlers[key];
                   if (newTree.node.removeEventListener) {
                       newTree.node.removeEventListener(key, closure, false);
                   } else {
                       newTree.node.detachEvent('on' + key, closure);
                   }
               } else if (newTree.node.hasAttribute(key)) {
                   newTree.node.removeAttribute(key);
               }
           }
           setAttribute(newTree);
           var limit = Math.min(oldTree.children.length, newTree.children.length);
           for (i = 0; i < limit; i++) {
               if (oldTree.children[i] instanceof Elem &&
                   newTree.children[i] instanceof Elem) {
                   update(oldTree.children[i], newTree.children[i]);
               } else {
                   newTree.children[i].node = render(newTree.children[i]);
                   newTree.node.replaceChild(newTree.children[i].node, oldTree.children[i].node);
               }
           }
           // remove extras
           for (i = limit; i < oldTree.children.length; i++) {
               oldTree.children[i].node.parentNode.removeChild(oldTree.children[i].node);
           }
           // add extras
           for (i = limit; i < newTree.children.length; i++) {
               newTree.children[i].node = render(newTree.children[i]);
               newTree.node.appendChild(newTree.children[i].node);
           }
       } else {
           newTree.node = render(newTree);
           oldTree.node.parentNode.replaceChild(newTree.node, oldTree.node);
       }
    }

    bigbang = function(elem, initialState, toDraw) {
        globalState = initialState;
        currentTree = toDraw(globalState);
        elem.appendChild(render(currentTree));
        rerender = function() {
            var newTree = toDraw(globalState);
            update(currentTree, newTree);
            currentTree = newTree;
        };
    };

    elem = function (type, attrs, children) {
        return new Elem(type, attrs, map(children, function (child) {
            return (typeof child === 'string') ?
                new Text(child) :
                child;
        }));
    };

    property = function(value) {
        return new Property(value);
    };
})();
  }
    @downloader-script[package version grouped-by-dist+platform]
    @noscript{
      Installers are available for the following platforms:
      @ul{@(for/list ([i (in-list all-installers)]
                      #:when (and (equal? release (installer-release i))
                                  (equal? package (installer-package i))))
             @li{@(installer->page i 'only-platform)})}}}
  @columns[8 #:center? #t #:center-text? #t #:row? #t]{
      @(let* ([sep   @list{@nbsp @bull @nbsp}]
              [links (λ links @(div style: "margin: 4ex 4ex;" (add-between links sep)))]
              [docs  (html-docs-link version)])
          (list
           @row{@links[@license{License}
                       @span{@all-version-pages
                             @span[style: "font-size: 80%"]{with notes and documentation}}
                       @pre:installers{Snapshot Builds}]}))})))

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
                        @(if (version<? ver first-version-without-alternative-page)
                             (add-between
                              (for/list ([p (in-list alt-packages)]
                                         #:when (member p (hash-ref release=>packages r)))
                                ((make-page r p) (package->name p)))
                              " ")
                             @a[href: @list{@|releases|/@|ver|/}]{All Installers})}
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
    @~ Some components of some Racket distributions, including the
       compiled @tt{racket} executable for the BC variant of
       Racket (@|ldquo|Regular@|rdquo| before version 8.0) and packages that distribute
       third-party libraries, are distributed under the
       @a[href: "http://www.gnu.org/licenses/lgpl-3.0.html"]{
                                                             GNU Lesser General Public License (LGPL) version 3.0}.
       Any program written in Racket that does not distribute the
       BC variant @tt{racket} binary itself is not affected by the
       license of that binary.
    @~ Versions of Racket prior to version 7.5 are distributed under the
       GNU LGPL, version 3.0.
    @~ See the @a[href: "https://github.com/racket/racket/blob/master/LICENSE"]{
       @tt{LICENSE}} file in the source code for more information.
    }}})

(define (release-> r)
  (hash 'version (release-version r)
        'datestring (release-date-string r)))

(define (downloader-script package version grouped-by-dist+platform)
  (define all-installers-json
    (for/list ([(dist grouped-by-platform) (in-dict grouped-by-dist+platform)])
      (define distname (package->name dist))
      (hash 'distName distname
            'dist (symbol->string dist)
            'installers
            (for/list ([(platform grouped-by-variants) (in-dict grouped-by-platform)])
              (define installers
                (for/list ([i (in-list grouped-by-variants)])
                  (hash 'path (installer-path i)
                        'mirrorUrl (installer->page i 'get-url)
                        'file (installer-file i)
                        'release (release-> (installer-release i))
                        'size (installer-size i)
                        'humanSize (get-human-size (installer-size i))
                        'package (symbol->string (installer-package i))
                        'binary (installer-binary? i)
                        'platform (installer-platform i)
                        'variant (installer-variant i)
                        'suffix (installer-suffix i))))
               (hash 'platform platform
                     'platformName (platform->name platform distname)
                     'installers installers)))))

  @script/inline[type: 'text/javascript]{@||
    var initialDist = @jsexpr->string[(symbol->string package)];
    var mirrorUrl = @jsexpr->string[(mirror-url* (first mirrors))];
    var allInstallers = @jsexpr->string[all-installers-json];

    // returns an ordering for the platform names, an array of regexps
    // note that the entries are sorted in a good order, so return an order
    // that only brings the locally desired entries to the top
    function getPlatformOrder() {
      var p = navigator.platform;
      var p2 = navigator.appVersion;
      var p3 = navigator.userAgent;
      function l(str) {
        return (p.indexOf(str) != -1) || (p2.indexOf(str) != -1) || (p3.indexOf(str) != -1);
      }
      var Win      = /Windows/,
          Win64    = /Windows.*64/,
          Win32    = /Windows.*32/,
          Mac      = /Mac/,
          MacIntel = /Mac.*Intel/,
          MacIntel64 = /Mac.*Intel.*64/,
          MacARM64 = /Mac.*Apple Silicon.*64/,
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
      else if (l("Mac"))   return [
        l("Intel") ? MacIntel64 : MacPPC,
        l("Intel") ? MacARM64 : MacPPC,
        l("Intel") ? MacIntel32 : MacPPC,
        Mac,
        Unix
      ];
      else if (l("Linux")) return [(l("_64") ? Linux64 : Linux32), Linux, Unix];
      else return [];
    }

    function orderPlatform(platforms) {
      var len = platforms.length;
      // get the order and a make a sorting function
      var order = getPlatformOrder();
      function getOrder(str) {
        for (var i = 0; i < order.length; i++)
          if (str.search(order[i]) >= 0) return i;
        return 999;
      }
      function isBetter(opt1, opt2) {
        // sort first by the order, then by how they were placed originally
        var ord1 = getOrder(opt1.name), ord2 = getOrder(opt2.name);
        if (ord1 < ord2) return -1;
        else if (ord1 > ord2) return +1;
        else if (opt1.index < opt2.index) return -1;
        else if (opt1.index > opt2.index) return +1;
        else return  0;
      }
      // sort the options, need to use a temporary array
      var tmps = map(platforms, function (platform, i) {
        return {
          name: platform.platformName,
          index: i,
          platform: platform
        };
      });
      tmps.sort(isBetter);
      forEach(tmps, function (platformData, i) {
        platforms[i] = platformData.platform;
      });
    }

    forEach(allInstallers, function (dist) {
      orderPlatform(dist.installers);
    });

    function getAllPlatforms(allInstallers, currentDist) {
      return filter(allInstallers, function (group) {
        return group.dist === currentDist;
      })[0].installers;
    }

    function getAllVariants(allPlatforms, currentPlatform) {
      return filter(allPlatforms, function (group) {
        return group.platform === currentPlatform;
      })[0].installers;
    }

    function getPackage(allVariants, currentVariant) {
      return filter(allVariants, function (group) {
        return computeVariant(group) === currentVariant;
      })[0];
    }

    function computeVariant(package) {
      return package.variant + '+' + package.suffix;
    }

    function toDraw(state) {
      var currentDist = state.dist;
      var allPlatforms = getAllPlatforms(allInstallers, currentDist);
      var currentPlatform = state.platform;
      var allVariants = getAllVariants(allPlatforms, currentPlatform);
      var currentVariant = state.variant;

      var currentPackage = getPackage(allVariants, currentVariant);

      function handleDistChange(state, e) {
        var currentDist = e.target.value;
        var allPlatforms = getAllPlatforms(allInstallers, currentDist);
        var currentPlatform = allPlatforms[0].platform;
        var allVariants = getAllVariants(allPlatforms, currentPlatform);
        var currentVariant = computeVariant(allVariants[0]);
        return {
          dist: currentDist,
          platform: currentPlatform,
          variant: currentVariant
        };
      }

      function handlePlatformChange(state, e) {
        var currentDist = state.dist;
        var allPlatforms = getAllPlatforms(allInstallers, currentDist);
        var currentPlatform = e.target.value;
        var allVariants = getAllVariants(allPlatforms, currentPlatform);
        var currentVariant = computeVariant(allVariants[0]);
        return {
          dist: currentDist,
          platform: currentPlatform,
          variant: currentVariant
        };
      }

      function handleVariantChange(state, e) {
        return {
          dist: state.dist,
          platform: state.platform,
          variant: e.target.value
        };
      }

      update(currentPackage);

      var children = [];

      if (allInstallers.length !== 1) {
        children.push(
        elem('div', {}, [
          'Distribution: ',
          elem('select', {onchange: handleDistChange},
            map(allInstallers, function (group) {
              return elem('option',
                group.dist === currentDist ?
                  {selected: property('selected'), value: group.dist} :
                  {value: group.dist},
                [group.distName]);
            }))
          ]));
        }

      children.push(
        elem('div', {}, [
          'Platform: ',
          elem('select', {onchange: handlePlatformChange},
            map(allPlatforms, function (group) {
              return elem('option',
                group.platform === currentPlatform ?
                  {selected: property('selected'), value: group.platform} :
                  {value: group.platform},
                [group.platformName]);
            }))
        ])
      );

      if (allVariants.length !== 1) {
        children.push(
          elem('div', {}, [
            'Variant: ',
            elem('select', {onchange: handleVariantChange},
              map(allVariants, function (group) {
                var theVariant = computeVariant(group);
                return elem('option',
                  theVariant === currentVariant ?
                    {selected: property('selected'), value: theVariant} :
                    {value: theVariant},
                  [group.variant]);
              }))
          ]));
      }
      return elem('div', {}, children);
    }

    function init() {
      var currentDist = initialDist;
      var allPlatforms = getAllPlatforms(allInstallers, currentDist);
      var currentPlatform = allPlatforms[0].platform;
      var allVariants = getAllVariants(allPlatforms, currentPlatform);
      var currentVariant = computeVariant(allVariants[0]);
      bigbang(document.getElementById('control'), {
        dist: currentDist,
        platform: currentPlatform,
        variant: currentVariant
      }, toDraw);
    }

    init();

    function showWhen(e, b) {
      document.getElementById(e).style.display = b ? 'block' : 'none';
    }

    function update(package) {
      var dist = package.package;
      var platform = package.platform;

      showWhen('minimal_racket_explain', dist === 'racket-minimal');

      showWhen('linux_explain', platform.search(/linux/) >= 0);
      showWhen('linux_install_explain', platform.search(/linux/) >= 0);
      showWhen('linux_ppa', platform.search(/linux/) >= 0);

      // NOTE: there used to be a condition that there must not be
      // Windows/Mac source packages in the selector (e.g., no "Windows Source")
      // to prevent versions prior 5.92 from showing the explanation.
      // However, this condition is never triggered because it's actually spelled
      // "Windows source". Nonetheless, it accidentally worked as intended because
      // the other condition is to match for "Unix Source", but versions prior 5.92
      // spelled "Unix source". This means the condition only applies to versions
      // after Racket 5.92 already.
      showWhen('source_explain',
               (platform === 'src-builtpkgs' || platform === 'src') &&
               dist === 'racket');

      showWhen('win_source_explain',
               (platform === 'src-builtpkgs' || platform === 'src') &&
               dist === 'racket-minimal');

      // NOTE: there used to be a condition that there must be 'builtpkgs' to show
      // this explanation. However, this condition is redundant, because 'src'
      // indicates that it's after 5.92 already (prior 5.92, there would be
      // 'win', `mac`, and `unix` instead)
      showWhen('builtpkgs_explain', platform === 'src');

      // NOTE: there used to be a condition that 'x86_64-osx-mac' will also trigger
      // this explanation, but 'x86_64-osx-mac' only exists before Racket 5.92
      // which is way before the version with the touchbar bug (6.7)
      @(if (equal? version version-with-touchbar-bug)
           @list{
             showWhen('macos_touchbar_explain', platform === 'x86_64-macosx');
           }
           null)

      @(if (version<=? version-before-m1-support version)
           @list{
                 showWhen('m1_mac_explain', platform === 'x86_64-macosx');
           }
           null)

      var download_link = document.getElementById('download_link');
      var path = mirrorUrl + package.path;
      var package_file_name = path.replace(/.*\//, "");
      download_link.href = path;
      download_link.innerHTML = package_file_name + " (" + package.humanSize + ")";
      var mirror_link = document.getElementById("mirror_link");
      mirror_link.href = package.mirrorUrl;
      document.getElementById("installer_name").innerHTML = package_file_name;
    }

    // show the download panel, since JS is obviously working
    showWhen('download_panel', true);
    @||})
