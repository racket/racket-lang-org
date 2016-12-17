#lang plt-web
(require plt-web/style
         racket/runtime-path
         racket/path
         raco/all-tools
         "../identity.rkt"
         "../testing.rkt"
         "index-illos.rkt")

(generate-svg-illos)

(define www-2016-site
  (site "www" ; deliberately write into existing "www" dir
        #:url (rewrite-for-testing "https://www.racket-lang.org/")))
(register-identity www-2016-site)

(define-runtime-path www-dir ".")
(define img-dir (simplify-path (build-path www-dir "img/")))
(define fonts-dir (simplify-path (build-path www-dir "fonts/")))

(define v (all-tools))
;; pollen rebuild
(parameterize ([current-directory (simplify-path www-dir)])
  (parameterize ([current-command-line-arguments (vector "render" "-r")])
    (dynamic-require (second (hash-ref v "pollen")) #f)))

(define (excluded-path? path)
  (define-values (base name _) (split-path path))
  (or
   ;; hidden path (starts with dot)
   (regexp-match #rx"^\\." (path->string name))
   ;; path in `private` directory
   (member (string->path "private") (explode-path path))
   ;; path in `compiled` directory
   (member (string->path "compiled") (explode-path path))
   ;; source files
   (member (path-get-extension name) '(#".rkt" #".p" #".pp" #".pm"))))

(for ([path (in-directory www-dir)]
      #:unless (or (directory-exists? path)
                   (excluded-path? path)))
     (define relpath (find-relative-path (simplify-path www-dir) (simplify-path path)))
     (copyfile #:site www-2016-site path (path->string relpath)))