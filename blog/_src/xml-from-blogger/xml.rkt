#lang at-exp racket
(require xml txexpr pollen/unstable/convert pollen/core gregor frog/private/xexpr2text frog/private/bodies-page racket/runtime-path)
(provide write-mds)

(define-runtime-path blog-xml-file "blog.xml")
(define-runtime-path blog-data-file "blog.rktd")

(define (xml->datafile fn-in fn-out)
  (define x (xml->xexpr (document-element
                         (read-xml (open-input-file
                                    fn-in)))))
  
  ;; todo: heavier decoding / cleanup of incoming html
  ;; todo: html->markdown conversion
  (define xc (map-elements (λ(x) (if (and
                                      (txexpr? x)
                                      (eq? 'content (get-tag x))
                                      (member '(type "html") (get-attrs x)))
                                     (html->xexpr (format "<post-content>~a</post-content>" (string-append* (get-elements x))))
                                     x)) x))
  
  (write-to-file xc fn-out))

(unless (file-exists? blog-data-file)
  (displayln "making blog data file")
  (xml->datafile blog-xml-file blog-data-file))

(define blog-data (file->value blog-data-file))

(define es (drop (get-elements blog-data) 65)) ; 65 by trial & error

;; posts are latest to earliest
;; comments are earliest to latest
;; posts have 'id subxexpr
;; comments have 'thr:in-reply-to subxexpr with 'ref attr that refers to post id
(define-values (posts comments) (split-at es 131)) ; 131 by trial & error

(define (x->md x)
  (map-elements
   (λ(x)
     (match x    
       [`(a ((href ,url)) ,text) (format "[~a](~a)" text url)]
       ['(br) "\n"]
       ['nbsp ""]
       [(list* 'tt xexprs) (string-append* (append (list "`") (map x->md xexprs) (list "`")))]
       [(list* 'li stuff) `(span "\n* " (li ,@stuff))]
       [(list* 'span
               '((style "font-family: &quot;courier new&quot; , &quot;courier&quot; , monospace;"))
               xexprs) (string-append* (append (list "`") (map x->md xexprs) (list "` ")))]
       [else (xexpr->markdown x)])) x))

#|
"Post source files in markdown format should be named YYYY-MM-DD-TITLE.md and need to have some meta-data in the first few lines."
|#
(require sugar/debug)
(define markdown-horizontal-rule "\n\n* * *\n\n")
(define (post->md p)
  (define post-str (string-append*
                    (map ~a (list "\n    Title:" (select 'title p)
                                  "\n    Date:" (select 'published p)
                                  "\n    Tags:" ;; todo: extract labels as tags
                                  "\n\n"   (format "*posted by ~a*" (select 'name (select 'author p)))
                                  "\n\n" (x->md (get-post-content p))))))
  
  (define comments (sort (post->comments p) string<? #:key comment-date))
  (define comment-str
    (cond
      [(positive? (length comments)) (string-append* "\n\n" "<!-- more -->" "\n\n" (append (cons markdown-horizontal-rule (add-between (map (compose1 x->md get-comment-content) comments) markdown-horizontal-rule)) (list markdown-horizontal-rule)))]
      [else ""]))
  (displayln (format "with ~a comments" (length comments)))
  (let* ([md-str (string-replace (string-append post-str comment-str) "` ." "`.")]
         [md-str (string-replace md-str "` $" "`$")])
    md-str))

;; once this is in the filename, frog uses it as the permalink
(define (slugify str)
  (let* ([str (or str "untitled")]
         [str (string-replace str " " "-")]
         [str (regexp-replace* #px"[^-\\w\\d\\s]" str "")]
         [str (string-downcase str)])
    str))

(define-runtime-path frog-src-dir (build-path ".." "_src" "posts"))
(define (post->frogpath p)
  (unless (directory-exists? frog-src-dir)
    (make-directory frog-src-dir))
  (define pubdate (iso8601->date (select 'published p)))
  (define title (select 'title p))
  (build-path frog-src-dir
              (string-append (~t pubdate "YYYY-MM-dd-") (slugify title) ".md")))


(define (write-post-to-frog-src p)
  (unless (equal? (select 'title p) "Contracts for object-oriented programming") ; defective page
    (display-to-file (post->md p) (post->frogpath p) #:exists 'replace)))

(define (write-md post)
  (display "Writing: ")
  (displayln (select 'title post))
  (write-post-to-frog-src post))

(define (write-mds)
  (for-each write-md posts))

(define (post-id post)
  (select 'id post))

(define (comment-post-id comment)
  (attr-ref (findf-txexpr comment (λ(x) (and (txexpr? x) (eq? (car x) 'thr:in-reply-to)))) 'ref))

(define (comment-date comment)
  (select 'published comment))

(define (post->comments post)
  (filter (λ(p) (equal? (comment-post-id p) (post-id post))) comments))

(define (get-post-content post)
  (findf-txexpr post (λ(x)
                                  (and (txexpr? x)
                                       (eq? (car x) 'post-content)))))

(define (get-comment-content comment)
  (define post-content (get-post-content comment))
  (append post-content (list "\n\n" "— " "*" (select 'name comment) ", " (~t (iso8601->date (select 'published comment)) "d MMMM YYYY") "*")))

(define fish (list-ref posts 34)) ;; bad post

#|

2015 10 30 too many paragraph breaks, bad list parsing
2015 09 22 bad bullet list parsing
2015 05 03 missing paragraph breaks
2014 12 10 line length wack
2014 11 23 line length wack, missing paragraph breaks
2014 10 07 too many paragraph breaks
2014 07 26 too many paragraph breaks
2013 12 17 missing syntax blocks
2013 05 29 not enough paragraphs ; missing syntax blocks
2013 03 25 missing syntax blocks
2013 02 15 spurious syntax block
2012 12 22 missing paragraph breaks
2012 11 25 missing syntax blocks
2012 11 23 missing syntax blocks
2012 11 01 missing syntax blocks
2012 10 24 very messed up
2012 09 28 defective syntax blocks
2012 08 24 missing & defective syntax blocks
2012 06 03 defective syntax blocks
2012 04 02 missing syntax blocks
2012 02 01 line length wack
2011 10 18 missing syntax blocks
2011 08 03 missing paragraph breaks
2011 05 26 missing syntax block
2011 04 30 missing paragraph breaks
2011 04 04 missing paragraph breaks & syntax blocks
2011 03 19 missing paragraph breaks
2011 02 14 missing paragraph breaks
2010 11 10 missing paragraph breaks
2010 10 03 paragraphs & syntax defective
2010 09 15 missing syntax blocks
2010 08 03 missing paragraph breaks
2010 06 07 missing paragraph breaks
2010 02 01 missing paragraph breaks
2009 12 07 missing syntax blocks
2009 12 01 missing paragraph breaks
2009 10 04 missing paragraph breaks
2009 06 23 missing syntax blocks
2009 05 24 line length wack
2009 05 18 missing syntax blocks
2009 05 18 missing syntax blocks
2009 03 29 comment problem
2009 03 12 missing syntax blocks
2009 02 14 missing paragraph breaks and syntax blocks
2009 01 21 missing title "f"
2008 11 24 missing syntax blocks
2008 10 28 missing title "f"
2008 10 05 missing paragraph breaks
2008 08 12 missing paragraph breaks
2008 07 04 missing title "f"
2008 06 12 missing title "f"
2008 02 23 missing syntax blocks
2007 12 29 bullet list defective
2007 12 29 missing paragraph breaks
2007 12 19 missing syntax blocks
2007 11 12 missing syntax blocks; comment problems
2007 09 06 missing syntax blocks
2007 08 06 missing syntax blocks
2007 08 03 missing syntax blocks
2007 07 30 missing syntax blocks
2007 07 27 missing syntax blocks
2007 05 10 missing syntax blocks
|#