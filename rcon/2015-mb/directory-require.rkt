#lang racket
(require pollen/decode sugar/list sugar/string)
(provide (all-defined-out))

(define (image src)
  `(img ((src ,src))))

(define (speaker time name title . desc)
  `(div ((class "speaker")) (h4 (span ((class "time")) ,time) " " ,name) ,title  ,@(if (not (empty? desc)) (cons '(br) desc) empty)))

(define (xlink target . sources-in)
  (define sources (if (empty? sources-in) (list target) sources-in))
  `(a ((href ,(format "#~a" (string-downcase target)))) ,@sources))

(define (xtarget id . targets-in)
  (define targets (if (empty? targets-in) (list id) targets-in))
  `(span ((id ,(string-downcase id))) ,@targets))

  
(define (root . items)
  (decode `(root () ,@items)
          #:string-proc (compose1 smart-quotes smart-dashes)
          #:exclude-tags '(style script)))


(define (inline-list tag . xs-in)
  (define xs (filter-split xs-in whitespace?))
  `(div ,@(map (λ(s) `(,tag ,@s)) xs)))


(define (link url-in . xs-in)
  ; "http" catches both "http" and "https" prefixes
  (define url (if (url-in . starts-with? . "http") url-in (format "http://~a" url-in)))
  (define xs (if (empty? xs-in) (list url) xs-in))
  `(a ((href ,url)) ,@xs))
