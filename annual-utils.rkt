#lang racket/base
(require raco/all-tools
         racket/format
         plt-web
         racket/path
         racket/list)
(provide (all-defined-out))

;; Utilities shared by `rcon` and `school`
;; for handling annually updated sites.

(define (pollen-rebuild! dir)
  (define v (all-tools))
  (parameterize ([current-directory (simplify-path dir)]
                 [current-command-line-arguments (vector "render" "-r")]
                 [current-namespace (make-base-namespace)])
    (dynamic-require (second (hash-ref v "pollen")) #f)))


(define (filename path)
  (define-values (_ name __) (split-path path))
  name)

(define (excluded-path? path)
  (define name (filename path))
  (define sploded (explode-path path))
  (or
   ;; hidden path (starts with dot)
   (regexp-match #rx"^\\." (path->string name))
   ;; path in `private` directory
   (member (string->path "private") sploded)
   ;; path in `compiled` directory
   (member (string->path "compiled") sploded)
   ;; source files
   (member (path-get-extension name) '(#".rkt" #".p" #".pp" #".pm"))))



(define (copy-annual-site! site starting-dir year
                           #:current [current? #f]
                           #:copy-current-index? [copy-current-index? #t])
  (define subdirs '("fonts" "slides"))
  (for* ([p (in-directory starting-dir (lambda (dir)
                                         (not (for/or ([subdir (in-list subdirs)])
                                                (equal? (build-path starting-dir subdir) dir)))))]
         [fn (in-value (filename p))]
         [ext (in-list '(#".html" #".css" #".svg" #".png" #".jpg" #".pdf"))]
         #:unless (or (not (path-has-extension? fn ext))
                      (excluded-path? fn)
                      (and current?
                           (not copy-current-index?)
                           (equal? fn (string->path "index.html")))))
    (define (copy current?)
      (copyfile #:site site (build-path starting-dir fn)
                (string-join (map ~a (append
                                      (if current? null (list year))
                                      (list fn))) "/")))
    (copy current?)
    (when (and current?
               copy-current-index?
               (equal? fn (string->path "index.html")))
      (copy #f)))

  (define (copy-subdir-if-extant subdir-name)
    (define subdir (build-path starting-dir subdir-name))
    (when (directory-exists? subdir)
      (for ([p (in-directory subdir)])
           (copyfile #:site site p
                     (string-join
                      (map ~a (append
                               (if current? null (list year))
                               (list subdir-name (filename p)))) "/")))))
  
  (for-each copy-subdir-if-extant subdirs))
