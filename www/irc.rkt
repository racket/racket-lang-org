#lang plt-web

(require "resources.rkt" plt-web/style)

(provide irc-content
         irc-quick)

(define webchat-link
  "https://kiwiirc.com/nextclient/irc.libera.chat/#racket")

(define irc-chat
  @page[#:site www-site #:title "IRC" #:part-of 'community]{
    @columns[12 #:row? #t #:center? #t]{
      @iframe[src: webchat-link width: "100%" height: "400"]}})

(define irc-content
  @list{Chat in the @tt[style: "background-color: #d8d8e8;"]{@big{@strong{#racket}}} channel on
@a[href: "https://libera.chat"]{@tt{libera.chat}}, an informal
discussion channel for all things related to Racket.})

(define irc-quick
  @text{@parlist[@strong{IRC}
                 @irc-content]})
