#lang plt-web

(require "resources.rkt"
         "lists-pages.rkt"
         "../identity.rkt")

(provide index)
(define index
  @page[#:site lists-site
        #:link-title "Lists" #:window-title "Racket Mailing Lists"
        #:part-of 'lists #:width 'full]{
    @(render-lists-page)})

(register-identity lists-site)
