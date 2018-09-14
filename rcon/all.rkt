#!/usr/bin/env racket
#lang plt-web
(require plt-web/style
         racket/runtime-path
         racket/format
         "resources.rkt"
         "utils.rkt"
         "../identity.rkt"
         "../annual-utils.rkt"
         (prefix-in 2011: "2011/all.rkt")
         (prefix-in 2012: "2012/all.rkt")
         (prefix-in 2013: "2013/all.rkt")
         (prefix-in 2014: "2014/all.rkt"))

(provide index)

(register-identity con-site)

(define-syntax-rule (copy-con-site! ARG ...)
  (copy-annual-site! con-site ARG ...))

(define-runtime-path 2015-dir "2015")
(pollen-rebuild! 2015-dir)
(copy-con-site! 2015-dir 2015)

(define-runtime-path 2016-dir "2016")
(pollen-rebuild! 2016-dir)
(copy-con-site! 2016-dir 2016)

;; 2017
(define-runtime-path 2017-dir "2017")
(pollen-rebuild! 2017-dir)
(copy-con-site! 2017-dir 2017)

;; 2018
(define-runtime-path 2018-dir "2018")
(pollen-rebuild! 2018-dir)
(copy-con-site! 2018-dir 2018 #:current #t)

;; 2019 (placeholder page)
(define-runtime-path 2019-dir "2019")
(pollen-rebuild! 2019-dir)
(copy-con-site! 2019-dir 2019)


(define-runtime-path 2018-index "2018/index.html")
(define index
  (page* #:site con-site
         #:link-title "RacketCon" #:title "RacketCon"
         #:extra-headers style-header
         #:id 'con
         @copyfile[#:site con-site 2018-index]))
