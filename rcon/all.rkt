#lang plt-web

(require plt-web/style
         racket/dict racket/match racket/runtime-path
         "resources.rkt"
         "utils.rkt"
         "../identity.rkt"
         (prefix-in 2011: "2011/all.rkt")
         (prefix-in 2012: "2012/all.rkt")
         (prefix-in 2013: "2013/all.rkt")
         (prefix-in 2014: "2014/all.rkt"))

(provide index)

(register-identity con-site)

(define-runtime-path 2015-mb "2015-mb/index.html")
(define-runtime-path 2016-index "2016/index.html")

(define index
  (page* #:site con-site
         #:link-title "RacketCon" #:title "RacketCon"
         #:extra-headers style-header
         #:id 'con
         @copyfile[#:site con-site 2016-index]))
;; TODO build the pollen file (and the SVGs) when the site is built, and remove
;;  the generated files from the repo

(define-runtime-path 2015-dir "2015-mb")

(for ([f (in-list '("eero.svg" "eero.svgz" "cubit.png" "pattern.png" "styles.css"))])
  (void (copyfile #:site con-site
                  (build-path 2015-dir f) (string-append "2015/" f)))
  (void (copyfile #:site con-site ; root of con-site, for current year
                  (build-path 2015-dir f))))

(void (copyfile #:site con-site
                (build-path 2015-dir "index.html") "2015/index.html"))

(define-runtime-path fonts "2015-mb/fonts/")
(for ([f (in-directory fonts)])
  (define-values (base name _) (split-path f))
  (copyfile #:site con-site f (string-append "fonts/" (path->string name)))
  (copyfile #:site con-site f (string-append "2015/fonts/" (path->string name))))
