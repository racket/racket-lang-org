#lang plt-web
(require "www/all.rkt"
         "download/all.rkt"
         "minis/all.rkt"
         "stubs/all.rkt"
         ;; the next two both export `index` (hence the prefixing)
         ;; but here, they're just imported for their side effects
         (prefix-in rcon: "rcon/all.rkt")
         (prefix-in school: "school/all.rkt")
         "blog/all.rkt")
