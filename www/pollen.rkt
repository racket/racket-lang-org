#lang racket/base
(provide (all-defined-out))
(require pollen/decode pollen/tag txexpr racket/list sugar/list racket/match sugar/debug)

(define-tag-function (feature attrs elems)
  (match-define (cons name xs) elems)
  (define expand-attr (and (assq 'expand attrs) "active_expander"))
  (define new-attrs (list* `(class ,(format "expander feature ~a" (or expand-attr ""))) `(onClick ,(format "handle_expander_click('~a')" (cadr (assq 'id attrs)))) attrs))
  `(div ,new-attrs (div ((class "name")) ,name) (div ((class "inner")) ,@xs)))

(define-tag-function (lang attrs elems)
  (define activate #f) ; make #f for real site
  (match-define (cons name xs) elems)
  (define new-attrs (list* `(class ,(format "expander lang ~a" (if activate " active_expander" ""))) `(onClick ,(format "handle_expander_click('~a')" (cadr (assq 'id attrs)))) attrs))
  `(div ,new-attrs (div ((class "name")) ,name (span ((class "click-here")) "[click for more]")) (div ((class "inner")) ,@xs)))

(define (root . elems)
  (list* 'div '((id "doc")) 
         (decode-elements elems
                          #:string-proc smart-quotes
                          #:exclude-tags '(style script tt))))


(define (cover path)
  `(img ((class "cover") (src ,(format "images/~a" path)))))

(define (detect-list-items elems)
  (define elems-merged (merge-newlines elems))
  (define (list-item-break? elem)
    (define list-item-separator-pattern (regexp "\n\n+"))
    (and (string? elem) (regexp-match list-item-separator-pattern elem)))
  (define list-of-li-elems (filter-split elems-merged list-item-break?))
  (define list-of-li-paragraphs
    (map (λ(li) (detect-paragraphs li #:force? #t)) list-of-li-elems))
  (define li-tag (make-default-tag-function 'li))
  (map (λ(lip) (apply li-tag lip)) list-of-li-paragraphs))

(define (section-content . xs)
  `(section-content ,@(decode-elements (detect-list-items xs) #:txexpr-elements-proc detect-paragraphs)))

(define (book . xs)
  `(div ((class "book")) ,@xs))


(define-tag-function (section attrs elems)
  (define trimmed-elems (dropf elems whitespace?))
  `(section ,attrs (section-title ,(car trimmed-elems)) ,(apply section-content (cdr trimmed-elems))))

(define-tag-function (special-section attrs elems)
  (define trimmed-elems (dropf elems whitespace?))
  `(section ,attrs ,(apply section-content trimmed-elems)))


(define-tag-function (link attrs url+elems)
  (match-define (cons url elems) url+elems)
  `(a ,(append `((href ,url)(onclick "javascript:cancel_bubble(event)")) attrs) ,@elems))

(define-tag-function (doclink attrs pkg+elems)
  (match-define (cons pkg elems) pkg+elems)
  (define doc-url (format "http://docs.racket-lang.org/~a/index.html" pkg))
  (apply link attrs (cons doc-url elems)))


(define-tag-function (doclinks attrs elems)
  (define (detect-list-items elems)
    (define elems-merged (merge-newlines elems))
    (define (list-item-break? elem)
      (define list-item-separator-pattern (regexp "\n+"))
      (and (string? elem) (regexp-match list-item-separator-pattern elem)))
    (append* (filter-split elems-merged list-item-break?)))
  
  `(ul ,(cons '(class "doclinks") attrs) ,@(map (λ(i) `(li ,i)) (detect-list-items elems))))

#|
`docs` creates links into Racket’s online documentation. This is fiddly because it’s specific to that system.
|#
(define docs-class "docs")
(require setup/xref scribble/xref sugar/coerce)
(define xref (load-collections-xref))

(define name->definition-tag
  (let ([tag-cache (make-hash)])
    (λ (name)
      (define xref (load-collections-xref))
      (define docs-to-search (list 'racket/base 'web-server/http/xexpr '2htdp/image 'scribble/base 'typed/racket 'racket/gui))
      (hash-ref! tag-cache name (λ _
                                  (for/or ([module-path (in-list docs-to-search)])
                                          (xref-binding->definition-tag xref (list module-path (->symbol name)) #f)))))))

(define (docs maybe-definable-name . text-args)
  (define linkname (if (empty? text-args)
                       (list maybe-definable-name)
                       text-args))
  (define definition-tag (name->definition-tag maybe-definable-name))
  (cond
    [definition-tag
      (define-values (path url-tag) (xref-tag->path+anchor xref definition-tag #:external-root-url "http://docs.racket-lang.org/"))
      (apply link (format "~a#~a" path url-tag) #:class docs-class linkname)]
    [else `(@ ,@linkname)]))