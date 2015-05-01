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

(define index
  (page* #:site con-site
         #:link-title "RacketCon" #:title "RacketCon"
         #:extra-headers style-header
         #:id 'con
         @copyfile[#:site con-site 2015-mb]))
;; TODO build the pollen file (and the SVGs) when the site is built, and remove
;;  the generated files from the repo

(define-runtime-path eero "2015-mb/eero.svg")
(define-runtime-path eero-z "2015-mb/eero.svgz")
(define-runtime-path cubit "2015-mb/cubit.png")
(define-runtime-path pattern "2015-mb/pattern.png")
(define-runtime-path styles "2015-mb/styles.css")
(void (copyfile #:site con-site eero))
(void (copyfile #:site con-site eero-z))
(void (copyfile #:site con-site cubit))
(void (copyfile #:site con-site pattern))
(void (copyfile #:site con-site styles))

(define-runtime-path fonts "2015-mb/fonts/")
(for ([f (in-directory fonts)])
  (define-values (base name _) (split-path f))
  (copyfile #:site con-site f (string-append "fonts/" (path->string name))))
