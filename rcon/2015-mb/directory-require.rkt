#lang racket
(require pollen/decode sugar/list sugar/string txexpr)
(provide (all-defined-out))



(define (image src)
  `(img ((src ,src))))

(define (speaker time name title . desc)
  `(div ((class "speaker")) (span ((class "speaker-name")(decode "exclude")) (span ((class "time")) ,time) " " ,name) (span ((class "title")) ,title)  ,@(if (not (empty? desc)) (cons '(br) desc) empty)))

(define (keynote-speaker time name title . desc)
  (attr-set (apply speaker time name title desc) 'class "keynote-speaker"))

(define (xlink target . sources-in)
  (define sources (if (empty? sources-in) (list target) sources-in))
  `(a ((href ,(format "#~a" (string-downcase target)))) ,@sources))

(define (xtarget id . targets-in)
  (define targets (if (empty? targets-in) (list id) targets-in))
  `(span ((id ,(string-downcase id))) ,@targets))


(define (splice xs)
  (define tags-to-splice '(splice-me))
  (apply append (for/list ([x (in-list xs)])
                  (if (and (txexpr? x) (member (get-tag x) tags-to-splice))
                      (get-elements x)
                      (list x)))))

(define exclusion-mark-attr '(decode "exclude"))
(define (root . items)
  (decode `(decoded-root ,@items)
          #:txexpr-elements-proc (compose1 detect-paragraphs splice)
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
  `(splice-me
    ,(foldable-subhead `(a ((href ,(format "javascript:toggle_div('~a')" div-name))) ,title))
    (,payload-tag ((style ,(format "display:~a;" openness))(id ,div-name) (class ,payload-class)) ,@(detect-paragraphs xs #:force? #t))))

(define (folded-open title . xs)
  (apply folded title #:open #t xs))