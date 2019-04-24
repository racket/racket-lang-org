#lang pollen/mode racket
(require (for-syntax racket/syntax) racket/runtime-path
         racket/draw json txexpr sugar/list pollen/decode racket/string)
(provide (all-defined-out) xexpr->html)

(define-syntax (ffd/rp stx)
  (syntax-case stx ()
    [(_ fam file kwargs ...)
     (with-syntax ([rp-name (generate-temporary)])
       #'(begin
           (require racket/runtime-path css-tools/font-face)
           (define-runtime-path rp-name (expand-user-path file))
           (font-face-declaration fam rp-name kwargs ...)))]))

(define-runtime-path styles-source "styles.css.pp")

(define (int->pixel-row int)
  (reverse (for/list ([bit (in-range 8)])
             (if (bitwise-bit-set? int bit) 1 0))))

(define cells (list->vector (file->value "vt220.rktd")))

(define (smear p) (bitwise-ior p (/ p 2)))

(define (make-matrix str)
  (append*
   (for/list ([str (in-list (string-split str "\n"))])
             (define cps (map char->integer (string->list str)))
             (define letter-array (apply map list (for/list ([cp (in-list cps)])
                                                            (vector-ref cells (if (= cp 32) 0 cp)))))
             (for/list ([rows (in-list letter-array)])
                       (for/list ([val (append* (add-between (map int->pixel-row (map smear rows)) '(0 0)))])
                                 val)))))

(define (make-jsexpr str)
  (jsexpr->string (make-matrix str)))

(define xunit 10)
(define yunit 16)


(define (default-cell-proc x y width)
  (define hstretch 1.5)
  `(g
    ,@(for/list ([delta (in-range 0 1 0.25)]
                 [count (in-naturals)])
                `(rect ((x ,(format "~a" (* xunit (- x delta))))
                        (y ,(format "~a" (* yunit (- y 0.5 delta))))
                        (width ,(format "~a" (* xunit (+ width hstretch))))
                        (height ,(format "~a" yunit))
                        (class ,(format "lower-shape layer-~a" count))
                        (rx ,(format "~a" (/ yunit 2)))
                        (ry ,(format "~a" (/ yunit 2))))))
    (rect ((x ,(format "~a" (* xunit (sub1 x))))
           (y ,(format "~a" (* yunit (- (sub1 y) 0.5))))
           (width ,(format "~a" (* xunit (+ width hstretch))))
           (height ,(format "~a" (- yunit 0)))
           (class "top-shape")
           (rx ,(format "~a" (/ yunit 2)))
           (ry ,(format "~a" (/ yunit 2)))))))

(define (make-dasharray)
  (string-join (map ~a (flatten (for/list ([i 20])
                                          (list 1 3)))) " "))

(define (make-svgs matrix [cell-proc default-cell-proc])
  (append
   (append*
    (for/list ([(row y) (in-indexed matrix)])
              (let loop ([x 0][cols row][gs null])
                (match cols
                  [(== empty) (reverse gs)]
                  [(list (and zeroes 0) ..1  rest ...)
                   (loop (+ x (length zeroes)) rest gs)]
                  [(list (and ones 1) ..1 rest ...)
                   (define new-g `(g ((class "pixel-on"))
                                     ,(cell-proc x y (length ones))))
                   (loop (+ x (length ones)) rest (cons new-g gs))]))))))


(require pollen/unstable/convert)
(define (string->svg #:width [width #f] . strs)
  (define str (apply string-append strs))
  (define max-line-chars (or width (apply max (map string-length strs))))
  (define line-count (length (string-split str "\n")))
  (html->xexpr
   ◊string-append{
 <div class="svg"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="-15 -20 ◊(number->string (+ 50 (* 100 max-line-chars))) ◊(number->string (+ 20 (* 10 yunit line-count)))" >
 ◊(string-join (map xexpr->html (make-svgs (make-matrix str))) "")
 </svg></div>}))


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
  `(div ((class "conlist")) ,@(map (λ(s) `(,tag ,@s)) xs)))


(define (link url-in . xs-in)
  ; "http" catches both "http" and "https" prefixes
  (define url (if (url-in . string-prefix? . "http") url-in (format "http://~a" url-in)))
  (define xs (if (empty? xs-in) (list url) xs-in))
  `(a ((href ,url)) ,@xs))

(define foldable-class "foldable")

(define subhead-tag 'div)
(define subhead-class "subhead")
(define (subhead . xs)
  `(,subhead-tag ((class ,subhead-class)) ,@xs))


(define (foldable-subhead div-name . xs)
  `(,subhead-tag ((class ,(string-join (list subhead-class foldable-class)))(onClick ,(format "javascript:toggle_div('~a')" div-name))) ,@xs))


(define payload-tag 'div)
(define payload-class "payload")

(define (folded title #:open [open #f] . xs)
  (define openness (if open "block" "none"))
  (define div-name (symbol->string (gensym)))
  `(div ((class "speaker-desc"))
    ,(foldable-subhead div-name title)
    (,payload-tag ((style ,(format "display:~a;" openness))(id ,div-name) (class ,payload-class)) ,@(detect-paragraphs xs #:force? #t))))

(define (folded-open title . xs)
  (apply folded title #:open #t xs))

(define (bio . xs)
  `(div ((class "bio")) ,@(detect-paragraphs xs #:force? #t)))

(define (gap [size 1.5])
  `(div ((style ,(format "height: ~arem" size)))))

(define (head which . xs)
  `(div ((class "head") (id ,(format "~a" which)))
        ,@(for/list ([c (in-string "cmyw")])
                    (define the-div
                      `(div ((class ,(format "movable ~a" c))
                             (style ,(format "transform: translate3d(~arem,~arem,0)" (- (random 2) 2) (- (random 2) 2)))
                             (id ,(symbol->string (gensym)))) ,@xs))
                    ;; use aria-hidden = "true" attribute to hide duplicates from screen readers
                    (if (not (char=? c #\w)) (attr-set the-div 'aria-hidden "true") the-div))))
