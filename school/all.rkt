#!/usr/bin/env racket
#lang plt-web
(require racket/runtime-path
         "../identity.rkt"
         "../testing.rkt"
         "../www/resources.rkt"
         "../annual-utils.rkt")

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
(copy-school-site! 2019-dir 2019 #:current #t)

;; On web server, redirect 2019/index.html to root index.html
;; (these refer to remote paths)
(void
 (symlink #:site school-site
          "index.html"
          "2019/index.html"))

(define-runtime-path current-school-index "2019/index.html")
(provide index)
(define index
  (page* #:site school-site
         #:link-title "Racket School" #:title "Racket School"
         #:id 'school
         @copyfile[#:site school-site current-school-index]))

