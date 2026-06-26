#lang at-exp racket/base

(require
  racket/runtime-path
  "lib.rkt"
  (rename-in "announcement.rkt" [page page_announcement])
  (rename-in "venue.rkt" [page page_venue])
  (rename-in "schedule.rkt" [page page_schedule]))

(define-runtime-path here ".")

(make here "index.html" page_announcement)
(make here "localinfo.html" page_venue)
(make here "schedule.html" page_schedule)
