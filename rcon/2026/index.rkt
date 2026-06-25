#lang at-exp racket/base

(require
  racket/runtime-path
  "lib.rkt"
  (rename-in "announcement.rkt" [page page_announcement]))

(define-runtime-path here ".")

(make here "index_new.html" page_announcement)
