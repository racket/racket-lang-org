#lang at-exp racket/base

(require
  racket/runtime-path
  "lib.rkt"
  (rename-in "announcement.rkt" [page page_announcement])
  (rename-in "venue.rkt" [page page_venue])
  (rename-in "schedule.rkt" [page page_schedule]))

(define-runtime-path here ".")

;; page_announcement is just the pure announcement and call
;; for participation for the conference. It is the default index.html

;; But, there is a preference for a single page organisation, therefore
;; page_schedule becomes index.html during schedule formation.

;(make here "index.html" page_announcement)
(make here "localinfo.html" page_venue)
(make here "index.html" page_schedule)
