#lang racket
(require racket/cmdline
         s3-sync/web
         s3-sync/routing-rule)
;; For routing rules:
(require "download/data.rkt"
         version/utils)

(define dry-run? #f)
(define check-metadata? #f)
(define save-temps? #f)
(define jobs 1)

(command-line
 #:once-each
 [("--dry-run") "Don't actually upload"
  (printf "Dry-run mode enabled\n")
  (set! dry-run? #t)]
 [("--check-metadata") "Check metadata of otherwise unmodified files"
  (set! check-metadata? #t)]
 [("-j" "--jobs") n "Download/upload with <n> parallel jobs"
  (set! jobs (string->number n))
  (unless (exact-positive-integer? jobs)
    (raise-user-error 's3-sync "bad number for --jobs: ~s" n))]
 [("--save-temps") "Preserve generated files"
  (printf "Saving generated files\n")
  (set! save-temps? #t)])

(define (step . s)
  (displayln (make-string 72 #\=))
  (for-each displayln s)
  (displayln (make-string 72 #\-)))

(define orig-dir (current-directory))
(define tmp-dir (make-temporary-file "sync~a" 'directory))

(current-directory tmp-dir)

;; ----------------------------------------

(step "Generate web pages")
(parameterize ([current-namespace (make-base-namespace)]
               [current-command-line-arguments (vector "-w"
                                                       "-o" "generated"
                                                       "-f")])
  (dynamic-require 'racket-lang-org/all #f)
  (dynamic-require '(submod racket-lang-org/all main) #f))

(define (upload dir site #:shallow? [shallow? #f])
  (step (format "Uploading ~a" site))
  (s3-web-sync (build-path "generated" dir)
               site
               #f
               #:dry-run? dry-run?
               #:shallow? shallow?
               #:upload? #t
               #:link-mode 'redirect
               #:check-metadata? check-metadata?
               #:jobs jobs
               #:log displayln))
(upload "www" "racket-lang.org")
(upload "www" "www.racket-lang.org")
(upload "pre" "pre.racket-lang.org")
(upload "con" "con.racket-lang.org")
(upload "blog" "blog.racket-lang.org")
(upload "drracket" "www.drracket.org")
(upload "download" "download.racket-lang.org" #:shallow? #t)

;; ----------------------------------------

(step "Additional Routing Rules")

(define routing-rules
  (for/list ([r (in-list all-releases)]
             #:when (version<=? "5.92" (release-version r)))
    (redirect-prefix-routing-rule #:old-prefix (format "releases/~a/installers" (release-version r))
                                  #:new-prefix (format "installers/~a" (release-version r))
                                  #:new-host "mirror.racket-lang.org")))

(unless dry-run?
  (add-routing-rules "download.racket-lang.org"
                     routing-rules
                     #:log-info displayln))


;; ----------------------------------------

(current-directory orig-dir)

(if save-temps?
    (printf "Files saved in ~a\n" tmp-dir)
    (delete-directory/files tmp-dir))
