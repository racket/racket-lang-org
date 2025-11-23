#lang racket/base

;; How to use this script:
;;
;;  * Add new versions to "version-redirect.txt".
;;    The original context of that file was created by
;;    looking at a mirror of "mirror.racket-lang.org",
;;    which was made with
;;
;;      wget --no-check-certificate -nv -m -X "/docs" --reject-regex ".*[?].*" \
;;            https://mirror.racket-lang.org/installers/
;;
;;    and running `find .` to get the list, and then pruning
;;    some garbage.
;;
;;  * Adjust the `redirect-before-version` definition below.
;;
;; The script will create new redirection objects, but it does not
;; remove bucket-wide redirection rules that are meant to be covered
;; by the added objects, so remove those manually via the S3 console.

(define redirect-before-version "7.0")

(define bucket "download.racket-lang.org")
(define target "mirror.racket-lang.org")

(define bucket-pattern "releases/~a/installers")
(define target-pattern "installers/~a")

(require version/utils
         racket/runtime-path
         aws/keys
         aws/s3)

(define-runtime-path version-redirect.txt "version-redirect.txt")

(define version+paths
  (call-with-input-file*
   version-redirect.txt
   (lambda (i)
     (for/list ([line (in-lines i)]
                #:do [(unless (regexp-match? #rx"^[.]/" line)
                        (error 'redirect "bad line in \"version-redirect.txt\": ~s" line))
                      (define elems (explode-path line))
                      (define vers (path->string (cadr elems)))]
                #:when (version<? vers redirect-before-version)
                #:unless (equal? line (string-append "./" vers)))
       (list vers (substring line (+ 3 (string-length vers))))))))

version+paths

(with-handlers ([exn:fail? (lambda _ (ensure-have-keys))])
  (credentials-from-environment!))

(s3-host "s3.amazonaws.com")

(s3-region (bucket-location bucket))

(for ([version+path (in-list version+paths)])
  (define vers (car version+path))
  (define path (cadr version+path))
  (define redirect-to (format "https://~a/~a/~a" target (format target-pattern vers) path))
  (define bucket+path (format "~a/~a/~a" bucket (format bucket-pattern vers) path))
  (define data #"")
  (define mime-type "application/octet-stream")
  (define heads (hash 'x-amz-website-redirect-location redirect-to
                      'x-amz-storage-class "REDUCED_REDUNDANCY"
                      'x-amz-acl "public-read"))
  (printf "linking ~a to ~a\n" bucket+path redirect-to)
  (put/bytes bucket+path data mime-type heads))
