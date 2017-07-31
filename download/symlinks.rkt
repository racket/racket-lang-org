#lang plt-web
(require "resources.rkt"
         "data.rkt"
         plt-web/indexes
         version/utils)

(provide (all-defined-out))

(define download-index-site (index-site download-site))

(define (new-version? v)
  (version<=? "5.92" v))

(define (need-html-redirect-version? v)
  (and (new-version? v)
       (version<=? v "6.10")))

;; Map the old layout to new layout, at least for HTML up to v6.10,
;; because those URLs are used in documentation redirects. The
;; distribution after v6.10 doesn't rely on the redirects, so
;; we leave out the symlinks to conserve S3 redirects.
(define (version-symlink src-v dest-v)
  (symlink #:site download-site
           (format "../../releases/~a/doc" dest-v)
           (format "docs/~a/html" src-v))
  (when #f
    (symlink #:site download-site
             (format "../../releases/~a/pdf-doc" dest-v)
             (format "docs/~a/pdf" src-v))))

(for ([r (in-list all-releases)])
  (define v (release-version r))
  (when (need-html-redirect-version? v)
    (version-symlink v v)))

;; We used to add "recent" links for the latest version, but
;; this is now disabled
#;
(unless (null? all-releases)
  (version-symlink "recent" (release-version (car all-releases))))

;; We generally expect "index.html" files to be in place in "releases"
;; and for pre-v6.0 "docs", but keep "installers", "docs", and
;; "releases" directory lists up-to-date as we add new versions:
(for ([r (in-list all-releases)])
  (define v (release-version r))
  (index-page download-index-site
              (format "docs/~a" v)
              (if new-version?
                  ;; "Redirect" to new layout:
                  (let ([dest (lambda (sub)
                                (format "../releases/~a/~a" v sub))])
                    `(("html" ,(dest "doc") . dir)
                      ("pdf" ,(dest "pdf-doc") . dir)))
                  ;; Old layout
                  '(("html" . dir)
                    ("pdf" . dir)))))
(void
 (index-page download-index-site
             "installers"
             (for/list ([r (in-list all-releases)])
               (cons (release-version r) 'dir))))
(void
 (index-page download-index-site
             "docs"
             (for/list ([r (in-list all-releases)])
               (cons (release-version r) 'dir))))
(void
 (index-page download-index-site
             "releases"
             (for/list ([r (in-list all-releases)]
                        #:when (version<=? "5.92" (release-version r)))
               (cons (release-version r) 'dir))))
