#lang racket

(require net/url
         net/http-client)

(provide url-str-response)
;; ensure links are live

;; responses:
;; - 'okay
;; - 'not-found
;; - 'forbidden
;; - 'bad-request
;; returns 'okay or 'not-found or signals an error?
(define (url-str-response url-str #:silent [silent? #f])
  (define the-url (string->url url-str))
  (define fragment (url-fragment the-url))
  (when (and fragment (not silent?))
    (eprintf "ignoring fragment portion of URL: ~e\n" fragment))
  (define get-port
    (head-impure-port
     (eliminate-fragment the-url)))
  (define first-line (regexp-match #px"^([^\r]+)\r\n" get-port))
  (define response-line
    (match first-line
      [(list _ first-line)
       first-line]
      [other
       (error 'response-line "no response line... more info here")]))

  (match response-line
    [(regexp #px#"^HTTP/1.[[:digit:]] ([[:digit:]]{3}) " (list _ code))
     (match code
       ;; I bet this list appears somewhere else, sigh...
       [#"200" 'okay]
       [#"400" 'bad-request]
       [#"403" 'forbidden]
       [#"404" 'not-found]
       [other (error 'uhoh "unexpected response: ~e" response-line)])]))

;; wow.... okay, much investigation later, it appears that
;; get-impure-port and friends deliver the "fragment" portion of
;; the URL as part of the GET line, which I believe is not correct at
;; all, so here's a function that discards the fragment...
(define (eliminate-fragment input-url)
  (url (url-scheme input-url)
       (url-user input-url)
       (url-host input-url)
       (url-port input-url)
       (url-path-absolute? input-url)
       (url-path input-url)
       (url-query input-url)
       #f))

(module+ test
  (require rackunit)

  (check-equal?
   (url-str-response
    "https://www.google.com/index.html#bogus-fragment-portion"
    #:silent #t)
   'okay)
  (check-equal?
   (url-str-response
    "https://www.google.com/ireallyhopethisisa404")
   'not-found)
  (check-equal?
   (url-str-response
    "https://www.example.com/")
   'okay)
  (check-exn
   #px"Missing protocol"
   (λ ()
     (url-str-response "otuhn.th")))

  )