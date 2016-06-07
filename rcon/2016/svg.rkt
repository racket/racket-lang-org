#lang at-exp racket/base
(require racket/file racket/string racket/list racket/match sugar/xml sugar/coerce txexpr "pollen.rkt")
(provide make-doc-from-file)


(define (split-style-attrs e)
  (define style-string (attr-ref e 'style))
  (append-map (λ(substr) (match-define (list key val) (string-split substr ":"))
                (list (->symbol key) val)) (string-split style-string ";")))

(define (not-white e)
  (not (regexp-match #rx"rgb\\(100%,100%,100%\\)" (attr-ref e 'style))))

(define css-style-string @string-append{
 path {
  transition: stroke 2s linear;
 }
 path:hover {
  stroke-width: 25;
  transition: stroke-width 0.3s ease;
  }})

(define (pick-random-color)
  (list-ref (list rcon-red rcon-blue ) (random 2)))

(define (make-doc-from-file fn)
  (define-values (prolog body) (xml-string->xexprs (file->string fn)))
  
  (let ([body (map-elements (λ(e) (cond
                                    [(and (txexpr? e) (eq? (get-tag e) 'path)
                                          (not-white e))
                                     (define id (gensym 'recolor))
                                     (attr-set (apply attr-set* e 'id id 'style "" (split-style-attrs e)) 'stroke (pick-random-color))]
                                    [else e])) body)])
    (xexprs->xml-string prolog (append body (list `(style ,css-style-string))))))
