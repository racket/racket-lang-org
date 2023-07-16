#!/usr/bin/env racket
#lang plt-web
(require racket/runtime-path
         racket/file
         "../identity.rkt"
         "../testing.rkt"
         "../www/resources.rkt"
         "../annual-utils.rkt"
         "../scribble.rkt")

(define school-site
  (site "school"
        #:url (rewrite-for-testing "https://school.racket-lang.org/")
        #:page-headers (identity-headers)
        #:share-from www-site))

(register-identity school-site)

(define-syntax-rule (copy-school-site! ARG ...)
  (copy-annual-site! school-site ARG ...))

;; 2019 
(define-runtime-path 2019-dir "2019")
(pollen-rebuild! 2019-dir)
(copy-school-site! 2019-dir 2019)

;; 2020
(define-runtime-path 2020-dir "2020")
(scribble-page (build-path 2020-dir "index.scrbl") 2020-dir)
(copy-school-site! 2020-dir 2020 #:current #t) ; change to `#:current` needs change in "sync.rkt"

;; The rest of this is the wrong idea. See "../rcon/all.rkt".
;; The current year's directory is redirected through a routing
;; rule in "../sync.rkt".

#|
;; On web server, redirect 2020/index.html to root index.html
;; (these refer to remote paths)
(void
 (symlink #:site school-site
          "../index.html"
          "2020/index.html"))

(define-runtime-path current-school-index "2020/index.html")
(provide index)
(define index
  (page* #:site school-site
         #:link-title "Racket School" #:title "Racket School"
         #:id 'school
         @copyfile[#:site school-site current-school-index]))
|#
