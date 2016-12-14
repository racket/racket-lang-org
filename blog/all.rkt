#lang plt-web
(require plt-web/style
         racket/dict racket/match racket/runtime-path racket/path
         "../identity.rkt"
         "../testing.rkt")

(define blog-site
  (site "blog"
        #:url (rewrite-for-testing "https://blog.racket-lang.org/")))
(register-identity blog-site)

(define (excluded-path? path)
  (define-values (base name _) (split-path path))
  (or
   ;; hidden path (starts with dot)
   (regexp-match #rx"^\\." (path->string name))
   ;; path in _src directory
   (member (string->path "_src") (explode-path path))
   ;; racket source
   (regexp-match #rx"\\.rkt$" (path->string name))))

(define-runtime-path blog-dir ".")
(for ([path (in-directory blog-dir)]
      #:unless (or (directory-exists? path)
                   (excluded-path? path)))
     (define relpath (find-relative-path (simplify-path blog-dir) (simplify-path path)))
     (copyfile #:site blog-site path (path->string relpath)))