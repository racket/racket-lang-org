#lang plt-web
(require "../testing.rkt"
         "../identity.rkt")

(provide lists-site)

(define lists-site (site "lists"
                         #:url (rewrite-for-testing "http://lists.racket-lang.org/")
                         #:page-headers (identity-headers)))
