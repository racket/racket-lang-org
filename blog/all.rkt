#lang plt-web
(require plt-web/style
         racket/runtime-path
         racket/path
         racket/system
         raco/all-tools
         "python.rkt"
         "../identity.rkt"
         "../testing.rkt")

(define blog-site
  (site "blog"
        #:url (rewrite-for-testing "https://blog.racket-lang.org/")))
(register-identity blog-site)

(define-runtime-path blog-dir ".")
(define css-dir (simplify-path (build-path blog-dir "css/")))


(unless (system* python-executable "-c" "import pygments")
  (error 'python "pygments required to preserve doc links for code on the blog"))

(define v (all-tools))
;; frog rebuild: generates blog html
(parameterize ([current-directory (simplify-path blog-dir)]
               [current-command-line-arguments (vector "-b")]
               [current-namespace (make-base-namespace)])
  (dynamic-require (second (hash-ref v "frog")) #f))
;; pollen rebuild: generates css
(parameterize ([current-directory (simplify-path blog-dir)]
               [current-command-line-arguments (vector "render" (path->string css-dir))]
               [current-namespace (make-base-namespace)])
  (dynamic-require (second (hash-ref v "pollen")) #f))

(define (excluded-path? path)
  (define-values (base name _) (split-path path))
  (or
   ;; hidden path (starts with dot)
   (regexp-match #rx"^\\." (path->string path))
   ;; hidden file (starts with dot)
   (regexp-match #rx"^\\." (path->string name))
   ;; path in _src directory
   (member (string->path "_src") (explode-path path))
   ;; path in compiled dir
   (member (string->path "compiled") (explode-path path))
   ;; racket or pollen source
   (regexp-match #rx"\\.(rkt|pp)$" (path->string name))))


(for ([path (in-directory blog-dir)]
      #:unless (or (directory-exists? path)
                   (excluded-path? path)))
  (define relpath (find-relative-path (simplify-path blog-dir) (simplify-path path)))
  (copyfile #:site blog-site path (path->string relpath)))
