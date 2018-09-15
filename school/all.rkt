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

;; 2019 (placeholder page)
(define-runtime-path 2019-dir "2019")
(pollen-rebuild! 2019-dir)
(copy-school-site! 2019-dir 2019)

;; Redirect root index.html to 2019/index.html
(void
 (symlink #:site school-site
          "2019/index.html"
          "index.html"))

;; suppress index page
#|
(define-runtime-path 2019-index "2019/index.html")
(provide index)
(define index
  (page* #:site school-site
         #:link-title "Racket School" #:title "Racket School"
         #:id 'school
         @copyfile[#:site school-site 2019-index]))

|#
