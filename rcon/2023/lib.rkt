#lang at-exp racket/base
(require xml
         (for-syntax racket/base))
(provide (all-defined-out))

(define fonts-url "https://fonts.googleapis.com/css2?family=Cutive+Mono&family=Montserrat:wght@400;700&display=swap")

(define-syntax-rule (define-tag* tag name extra ...)
  (define name
    (make-keyword-procedure
     (lambda (kws kw-args . content)
       (define attribs
         (append
          `(extra ...)
          (for/list ([kw (in-list kws)]
                     [kw-arg (in-list kw-args)])
            `[,(string->symbol (keyword->string kw))
              ,kw-arg])))
       `(tag ,attribs . ,(decode content))))))
(define-syntax-rule (define-tag name)
  (define-tag* name name))

(define-tag html)
(define-tag head)
(define-tag* meta head-meta)
(define-tag link)
(define-tag style)
(define-tag title)
(define-tag body)
(define-tag img)
(define-tag a)
(define-tag ul)
(define-tag li)
(define-tag br)
(define-tag em)

(define (decode l)
  (let loop ([l l])
    (cond
      [(null? l) null]
      [(string? (car l))
       (append (decode-string (car l))
               (loop (cdr l)))]
      [else (cons (car l) (loop (cdr l)))])))

(define (decode-string s)
  (cond
    [(regexp-match-positions #rx"'" s)
     => (lambda (m)
          (append (decode-string (substring s 0 (caar m)))
                  (list 'rsquo)
                  (decode-string (substring s (cdar m)))))]
    [else (list s)]))
     

;; ------------------------------------------------------------

(define classes-b (box '()))

(define (classes->string)
  (define o (open-output-string))
  (parameterize ([current-output-port o])
    (for ([c (in-list (unbox classes-b))])
      (printf ".~a {\n" (car c))
      (for ([p (in-list (cdr c))])
        (printf "~a: ~a;\n"
                (car p) (cadr p)))
      (printf "}\n")))
  (get-output-string o))

(begin-for-syntax
  (struct div (function-id class-id)
    #:property prop:procedure
    (lambda (self stx)
      (syntax-case stx ()
        [(_ arg ...) #`(#,(div-function-id self) arg ...)]
        [_ (div-function-id self)]))))

(define-syntax-rule (define-styled tag name desc ...)
  (begin
    (define-tag* tag div-function
      [class ,(symbol->string 'name)])
    (define div-class `(name desc ...))
    (set-box! classes-b (cons div-class (unbox classes-b)))
    (define-syntax name (div (quote-syntax div-function)
                             (quote-syntax div-class)))))

(define-syntax-rule (define-div name desc ...)
  (define-styled div name desc ...))

(define-syntax-rule (define-a name desc ...)
  (define-styled a name desc ...))

(define-syntax-rule (define-span name desc ...)
  (define-styled span name desc ...))

(define centered
  `((text-align center)
    (margin-left auto)
    (margin-right auto)))
