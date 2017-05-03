#lang plt-web
(require plt-web/style
         racket/runtime-path
         racket/path
         racket/format
         raco/all-tools
         "resources.rkt"
         "utils.rkt"
         "../identity.rkt"
         (prefix-in 2011: "2011/all.rkt")
         (prefix-in 2012: "2012/all.rkt")
         (prefix-in 2013: "2013/all.rkt")
         (prefix-in 2014: "2014/all.rkt"))

(provide index)

(register-identity con-site)

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

(define (copy-con-site! starting-dir year #:current [current? #f])
  (for* ([p (in-directory starting-dir)]
         [fn (in-value (filename p))]
         [ext (in-list '(#".html" #".css" #".svg" #".png"))]
         #:unless (or (not (path-has-extension? fn ext))
                      (excluded-path? fn)
                      (and current? (equal? fn (string->path "index.html")))))
        (copyfile #:site con-site (build-path starting-dir fn)
                  (string-join (map ~a (append
                                        (if current? null (list year))
                                        (list fn))) "/")))
  (for* ([p (in-directory (build-path starting-dir "fonts"))])
        (copyfile #:site con-site p
                  (string-join (map ~a (append
                                        (if current? null (list year))
                                        (list "fonts" (filename p)))) "/"))))

(define-runtime-path 2015-dir "2015")
(pollen-rebuild! 2015-dir)
(copy-con-site! 2015-dir 2015)


(define-runtime-path 2016-dir "2016")
(pollen-rebuild! 2016-dir)
(copy-con-site! 2016-dir 2016)


(define-runtime-path 2017-dir "2017")
(pollen-rebuild! 2017-dir)
(copy-con-site! 2017-dir 2017 #:current #t)

(define-runtime-path 2017-index "2017/index.html")
(define index
  (page* #:site con-site
         #:link-title "RacketCon" #:title "RacketCon"
         #:extra-headers style-header
         #:id 'con
         @copyfile[#:site con-site 2017-index]))