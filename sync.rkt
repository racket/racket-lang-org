#lang racket
(require racket/cmdline
         s3-sync/web
         s3-sync/routing-rule
         net/url-string)
;; For routing rules:
(require "download/data.rkt"
         version/utils)

(define render-locally? #false)
(define dry-run? #f)
(define check-metadata? #f)
(define save-temps? #f)
(define jobs 1)

(command-line
 #:once-each
 [("--check-metadata")
  "Check metadata of otherwise unmodified files"
  (set! check-metadata? #t)]
 [("-j" "--jobs")
  n
  "Download/upload with <n> parallel jobs"
  (set! jobs (string->number n))
  (unless (exact-positive-integer? jobs)
    (raise-user-error 's3-sync "bad number for --jobs: ~s" n))]
 [("--save-temps")
  "Preserve generated files"
  (printf "Saving generated files\n")
  (set! save-temps? #t)]
 #:once-any
 [("--dry-run")
  "Don't actually upload"
  (printf "Dry-run mode enabled\n")
  (set! dry-run? #t)]
 [("--render-locally") new-dir
  "Render but don't dry-run the uploads"
  (printf "Render-locally mode enabled, move files to ~a\n" new-dir)
  (set! render-locally? new-dir)
  (set! dry-run? #true)])

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

(printf "Files so far written to ~a\n" tmp-dir)

(define (upload dir site
                #:shallow? [shallow? #f]
                #:link-mode [link-mode 'redirect])
  (step (format "Uploading ~a" site))
  (s3-web-sync (build-path "generated" dir)
               site
               #f
               #:dry-run? dry-run?
               #:shallow? shallow?
               #:upload? #t
               #:link-mode link-mode
               #:check-metadata? check-metadata?
               #:jobs jobs
               #:log displayln))

(unless render-locally?
  (upload "www" "racket-lang.org")
  (upload "www" "www.racket-lang.org")
  (upload "pre" "pre.racket-lang.org")
  (upload "snapshot" "snapshot.racket-lang.org")
  (upload "con" "con.racket-lang.org" #:link-mode 'redirects)
  (upload "school" "school.racket-lang.org" #:link-mode 'redirects)
  (upload "blog" "blog.racket-lang.org")
  (upload "drracket" "www.drracket.org")
  (upload "download" "download.racket-lang.org" #:shallow? #t #:link-mode 'redirects)
  (upload "lists" "lists.racket-lang.org" #:shallow? #t))

;; ----------------------------------------

(step "Additional Routing Rules")

(define download-mirror-url
  (string->url (mirror-url (car mirrors))))

(define download-mirror-host (url-host download-mirror-url))
(define download-mirror-path
  (let ([l (reverse
            ;; Strip "installers/" from path:
            (cddr (reverse (url-path download-mirror-url))))])
    (if (null? l)
        ""
        (string-append (url->string (url #f #f #f #f #f l null #f))
                       "/"))))

(define routing-rules
  (cons
   (redirect-prefix-routing-rule #:old-prefix "installers"
                                 #:new-prefix (format "~ainstallers" download-mirror-path)
                                 #:new-host download-mirror-host
                                 #:redirect-code "302"
                                 #:new-protocol 'https)
   (for/list ([r (in-list all-releases)]
              #:when (version<=? "5.92" (release-version r)))
     (redirect-prefix-routing-rule #:old-prefix (format "releases/~a/installers" (release-version r))
                                   #:new-prefix (format "~ainstallers/~a"
                                                        download-mirror-path
                                                        (release-version r))
                                   #:new-host download-mirror-host
                                   #:redirect-code "302"
                                   #:new-protocol 'https))))

(unless dry-run?
  (add-routing-rules "download.racket-lang.org"
                     routing-rules
                     #:log-info displayln))

(unless dry-run?
  ;; Redirect current year's "index.html" to the root "index.html":
  (add-routing-rules "con.racket-lang.org"
                     (list
                      (redirect-prefix-routing-rule #:old-prefix "2023/index.html"
                                                    #:new-prefix "index.html"
                                                    #:redirect-code "302"))
                     #:log-info displayln))

;; ----------------------------------------

(current-directory orig-dir)

(cond
  [render-locally?
    ;; move rendered pages into place
    (when (directory-exists? render-locally?)
      (delete-directory/files render-locally?))
    (define tmp-src (build-path tmp-dir "generated"))
    (define local-dest render-locally?)
    (copy-directory/files tmp-src local-dest #:preserve-links? #t)]
  [else
    (printf "Files saved in ~a\n" tmp-dir)])

(unless save-temps?
  (delete-directory/files tmp-dir))

(printf "\n\nIf you updated any CSS file, please purge it from the Cloudflare cache.\n\n")
