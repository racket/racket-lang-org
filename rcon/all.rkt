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
(copy-con-site! 2018-dir 2018)

;; 2019
(define-runtime-path 2019-dir "2019")
(pollen-rebuild! 2019-dir)
(copy-con-site! 2019-dir 2019)

;; 2020
(require (prefix-in 2020: "2020/index.rkt"))
(define-runtime-path 2020-dir "2020")
(2020:make 2020-dir)
(copy-con-site! 2020-dir 2020)

;; 2021
(require (prefix-in 2021: "2021/index.rkt"))
(define-runtime-path 2021-dir "2021")
(2021:make 2021-dir)
(copy-con-site! 2021-dir 2021)

;; 2022
(require (prefix-in 2022: "2022/index.rkt"))
(define-runtime-path 2022-dir "2022")
(2022:make 2022-dir)
(copy-con-site! 2022-dir 2022)

;; 2023
(require (prefix-in 2023: "2023/index.rkt"))
(define-runtime-path 2023-dir "2023")
(2023:make 2023-dir)
(copy-con-site! 2023-dir 2023)

;; 2024
(require (prefix-in 2024: "2024/index.rkt"))
(define-runtime-path 2024-dir "2024")
(2024:make 2024-dir)
(copy-con-site! 2024-dir 2024)

;; 2025
(require (prefix-in 2025: "2025/index.rkt"))
(define-runtime-path 2025-dir "2025")
(2025:make 2025-dir)
(copy-con-site! 2025-dir 2025 #:current #t) ; change to `#:current` needs change in "sync.rkt"

;; A `#:current #t` above causes that year's content to be copied to
;; the root directory instead of the year's subdiredtory, except that
;; "index.html" is by itself created in the subdirectory. A
;; routing-rule redirect in "../sync.rkt" will send all references
;; into the subdirectory. A "index.html" file is written in the
;; subdirectory, anyway, because that is needed for a URL ending in
;; the directory and NO slash to be redirected to "index.html", which
;; is then redirected to the root directory.

;; This is a bad idea, because it creates a 301 (permanent)
;; redirect; instead, there's a routing rule added in "../sync.rkt"
;; to create a 302 (temporary) redirect:
#|
   ;; On web server, redirect 2023/index.html to root index.html
   ;; (these refer to remote paths)
   (void
    (symlink #:site con-site
             "../index.html"
             "2023/index.html"))
|#

;; Don't do this, either, because it doesn't work right with
;; extra files referenced relative to "index.html"
#|
(define-runtime-path current-con-index "2023/index.html")
(define index
  (page* #:site con-site
         #:link-title "RacketCon" #:title "RacketCon"
         #:extra-headers style-header
         #:id 'con
         @copyfile[#:site con-site current-con-index]))
|#
