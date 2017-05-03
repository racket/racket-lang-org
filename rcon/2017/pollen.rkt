#lang racket
(require pollen/decode pollen/private/whitespace sugar/list sugar/unstable/string txexpr)
(provide (all-defined-out))

(define rcon-blue "rgb(52.941176%,80.784314%,98.039216%)")
(define rcon-red "rgb(86.27451%,7.843137%,23.529412%)")

(define (image src)
  `(img ((src ,src))))

(define (speaker time name . xs)
  (match-define (cons title desc)
    (if (pair? xs)
        xs
        (list "")))
  `(div ((class "speaker")) (span ((class "speaker-name")(decode "exclude")) (span ((class "time")) ,time) " " ,name) (span ((class "title")) ,title)  ,@(if (not (empty? desc)) (cons '(br) desc) empty)))

(define (keynote-speaker time name title . desc)
  (attr-set (apply speaker time name title desc) 'class "keynote-speaker"))

(define (xlink target . sources-in)
  (define sources (if (empty? sources-in) (list target) sources-in))
  `(a ((href ,(format "#~a" (string-downcase target)))) ,@sources))

(define (xtarget id . targets-in)
  (define targets (if (empty? targets-in) (list id) targets-in))
  `(span ((id ,(string-downcase id))) ,@targets))

(define (rlink target . sources-in)
  (define sources (if (empty? sources-in) (list target) sources-in))
  `(a ((href ,(string-downcase target))) ,@sources))


(define exclusion-mark-attr '(decode "exclude"))
(define (root . items)
  (decode `(decoded-root ,@items)
          #:txexpr-elements-proc detect-paragraphs
          #:string-proc (compose1 smart-quotes smart-dashes)
          #:exclude-tags '(style script pre)
          #:exclude-attrs (list exclusion-mark-attr)))


(define (inline-list tag . xs-in)
  (define xs (filter-split xs-in whitespace?))
  `(div ,@(map (λ(s) `(,tag ,@s)) xs)))


(define (link url-in . xs-in)
  ; "http" catches both "http" and "https" prefixes
  (define url (if (url-in . starts-with? . "http") url-in (format "http://~a" url-in)))
  (define xs (if (empty? xs-in) (list url) xs-in))
  `(a ((href ,url)) ,@xs))

(define foldable-class "foldable")

(define subhead-tag 'div)
(define subhead-class "subhead")
(define (subhead . xs)
  `(,subhead-tag ((class ,subhead-class)) ,@xs))


(define (foldable-subhead . xs)
  `(,subhead-tag ((class ,(string-join (list subhead-class foldable-class)))) ,@xs))


(define payload-tag 'div)
(define payload-class "payload")

(define (folded title #:open [open #f] . xs)
  (define openness (if open "block" "none"))
  (define div-name (symbol->string (gensym)))
  `(@
    ,(foldable-subhead `(a ((href ,(format "javascript:toggle_div('~a')" div-name))) ,title))
    (,payload-tag ((style ,(format "display:~a;" openness))(id ,div-name) (class ,payload-class)) ,@(detect-paragraphs xs #:force? #t))))

(define (folded-open title . xs)
  (apply folded title #:open #t xs))

(define (bio . xs)
  `(div ((class "bio")) ,@xs))

(define (gap [size 1.5])
  `(div ((style ,(format "height: ~arem" size)))))

(define (head which . xs)
  `(div ((class "head") (id ,(format "~a" which)))
        ,@(for/list ([c (in-string "cmyw")])
                    `(div ((class ,(format "movable ~a" c))
                           (style ,(format "transform: translate3d(~arem,~arem,0)" (- (random 2) 2) (- (random 2) 2)))
                           (id ,(symbol->string (gensym)))) ,@xs))))