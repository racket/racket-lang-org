#lang at-exp racket/base

(require
  (rename-in "lib.rkt" [make lib:make])
  (rename-in "announcement.rkt" [page page_announcement])
  (rename-in "venue.rkt" [page page_venue])
  (rename-in "schedule.rkt" [page page_schedule]))

(provide make) ; used by "../all.rkt"

;; page_announcement is just the pure announcement and call
;; for participation for the conference. It is the default index.html

;; But, there is a preference for a single page organisation, therefore
;; page_schedule becomes index.html during schedule formation.

(define (make dest)
  ;(make dest "index.html" page_announcement)
  (lib:make dest "localinfo.html" page_venue)
  (lib:make dest "index.html" page_schedule))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path here ".")
  (make here))
