#lang at-exp racket

(require rackunit)

(provide bullet
         sub-bullet
         link
         txt-render-bullet
         md-render-bullet
         bullet-links
         display-lines horizontal-bar render-contributors)

;; yeah, I probably had better things to do than write a little string-reflowing
;; function, sigh.

;; two less than the bar length to allow for the hyphen-space:
(define max-width (make-parameter 68))

(define bar-length 70)

(define horizontal-bar (apply string (for/list ([i bar-length]) #\-)))

;; a break-loc is a number representing a break before the nth character of a string.

;; a bulletS contains a possible url and ... a flow? I don't pretend to know
;; exactly what that means. I should read the docs!
(struct bulletS (maybe-url depth flow) #:transparent)

(struct linkS (text url) #:transparent)

;; given a list of contributors, return a list of strings suitable for inclusion
;; in the release notes
(define (render-contributors contributors)
  (string-reflow
   (apply
    string-append
    (append
     (add-between (all-but-last contributors) ", ")
     (list
      ", and "
      (last contributors)
      ".")))))

(define (all-but-last l)
  (reverse (rest (reverse l))))

;; given a string with no leading or trailing whitespace, reformat as a list of
;; strings subject to some fairly standard line-breaking rules. Doesn't really
;; deal with paragraphs in any particularly sane way.
(define (string-reflow str)
  (cond [(< (string-length str) (max-width))
          (list str)]
        [else
         (match (find-break/leftward str (max-width))
           [(list begin end)
            (cond [(< 0 begin)
                   (cons (substring str 0 begin)
                         (string-reflow (substring str end)))]
                  [else ;; must be (list 0 0), if input was trimmed
                   (match (find-break/rightward str (add1 (max-width)))
                     [(list begin end)
                      (cond [(< end (string-length str))
                             (cons (substring str 0 begin)
                                   (string-reflow (substring str end)))]
                            [else ;; give up
                             (list str)])])])])]))

;; given a string and a break-location, search leftward to find a break-location pair containing
;; whitespace at which
;; it's okay to break a string. Note that because of whitespace, it's difficult to fence out
;; results that begin at zero or end at the end; this should be a post-check.
(define (find-break/leftward str break-loc)
   (cond [(<= break-loc 0)
          (list 0 0)]
         [(space-adjacent? str break-loc)
          (grow-space str break-loc)]
         [(breakable-location str break-loc)
          (list break-loc break-loc)]
         [else (find-break/leftward str (sub1 break-loc))]))

(define (find-break/rightward str break-loc)
   (cond [(> break-loc (string-length str))
          (list (string-length str) (string-length str))]
         [(space-adjacent? str break-loc)
          (grow-space str break-loc)]
         [(breakable-location str break-loc)
          (list break-loc break-loc)]
         [else (find-break/rightward str (add1 break-loc))]))

;; is this break-location adjacent to a space?
(define (space-adjacent? str break-loc)
  (define l (string-length str))
  (or (and (< 0 break-loc)
           (is-space-char? (string-ref str (sub1 break-loc))))
      (and (< break-loc l)
           (is-space-char? (string-ref str break-loc)))))

;; is this character a whitespace character?
(define (is-space-char? ch)
  (set-member? (set #\space #\newline #\tab) ch))



;; given a space-adjacent break, return the leftmost and rightmost
;; break-locations that bound that space
(define (grow-space str break-loc)
  (define left-bound
    (let loop ([i (sub1 break-loc)])
      (cond [(< i 0) 0]
            [(is-space-char? (string-ref str i))
             (loop (sub1 i))]
            [else (add1 i)])))
  (define right-bound
    (let loop ([i break-loc])
      (cond [(= i (string-length str)) i]
            [(is-space-char? (string-ref str i))
             (loop (add1 i))]
            [else i])))
  (list left-bound right-bound))

;; is this location breakable because it's next to the correct side
;; of a paren?
(define (breakable-location str break-loc)
  (and (< 0 break-loc)
       (< break-loc (string-length str))
       (or (equal? (string-ref str (sub1 break-loc)) #\))
           (equal? (string-ref str break-loc) #\())))



;; given a list of strings representing text, replace
;; single-char newlines (as emitted from scribble) with
;; spaces and apply string-reflow to the text
(define (reflow text)
  (cond [(andmap string? text)
         (map (λ (s) (cond [(equal? s "\n") " "]
                           [else s]))
              text)
         ;; just checking so far, not done:
         (string-reflow (string-trim
                         (regexp-replace* #px"\n"
                                          (apply string-append text)
                                          " ")))]
        [else (error 'reflow "surprised by non-list-of-strings: ~a"
                     text)]))

;; an at-exp function that creates a bullet
(define (bullet #:url [url #f] . args)
  (bulletS url 0 args))

;; an at-exp function that creates a sub-bullet
(define (sub-bullet #:url [url #f] . args)
  (bulletS url 1 args))

;; an at-exp function that creates a link
(define (link url text)
  (linkS text url))

;; given a bullet, return a list of strings representing
;; lines suitable for inclusion in a text file
(define (txt-render-bullet bullet)
  (match bullet
    [(bulletS maybe-url depth strs)
     (define maybe-docref
       (cond [maybe-url (list " (See " maybe-url " .)")]
             [else (list)]))
     (txt-bullet-format
      depth
      (reflow
       (map
        txt-render-flow-element
        (append strs maybe-docref))))]))

;; given a bullet, return a list containing just one string,
;; representing a line suitable for inclusion in a markdown
;; file
(define (md-render-bullet bullet)
  (match bullet
    [(bulletS maybe-url depth strs)
     (define maybe-docref
       (cond [maybe-url (list " (See " maybe-url " .)")]
             [else (list)]))
     (md-bullet-format
      depth
      (map
       md-render-flow-element
       (append strs maybe-docref)))]))


;; render a flow element as it should appear in a text file
(define (txt-render-flow-element elt)
  (match elt
    [(? string? s) s]
    [(linkS text url) text]
    [other (error 'render-flow-element "unexpected flow element: ~e\n"
                  other)]))

;; render a flow element as it should appear in a markdown file
(define (md-render-flow-element elt)
  (match elt
    [(? string? s) s]
    [(linkS text url) (string-append "[" text "](" url ")")]
    [other (error 'render-flow-element "unexpected flow element: ~e\n"
                  other)]))

;; apply the bullet formatting to the lines appearing in a text file,
;; including indentation (reflowing already done)
(define (txt-bullet-format depth strs)
  (define pad-str (apply string-append (for/list ([i depth]) "    ")))
  (cons (string-append pad-str "- " (first strs))
        (map (λ (s) (string-append pad-str "  " s))
             (rest strs))))

;; apply the bullet formatting to the strings corresponding to
;; a bullet in a markdown file (add a bullet and also glue them
;; all together into a single line)
(define (md-bullet-format depth strs)
  ;; nb 4 spaces req'd for compliant bullet nesting:
  (define pad-str (apply string-append (for/list ([i depth]) "    ")))
  (define no-newlines-strs (map (λ (s) (cond [(equal? s "\n") " "]
                                             [else s]))
                                strs))
  (list (string-append pad-str "- " (apply string-append no-newlines-strs))))

;; list-of-strings -> string
(define (display-lines los)
  (map displayln los))

;; return the links associated with a bullet
(define (bullet-links bullet)
  (match bullet
    [(bulletS maybe-url _ flow)
     (append
      (cond [maybe-url (list maybe-url)]
            [else '()])
      (filter-map flow-link flow))]
    [other (error 'bullet-links "unexpected argument to bullet-links")]))

;; return the link associated with a flow (or #f if none).
(define (flow-link flow)
  (match flow
    [(? string? s) #f]
    [(linkS text url) url]))

(module+ test
  (require rackunit)

  (check-equal?
   (bullet-links
    (bulletS
     #f
     1
     (list "a big" "\n" (linkS "potato" "https://potatoze.com/") " is in"
           "\n" "my " (linkS "refrigerator right now"
                             "http://insecure.example.com/"))))
   '("https://potatoze.com/"
     "http://insecure.example.com/"))
  (check-equal?
   (bullet-links (bulletS
                  "https://one.example.com/"
                  0
                  (list "zz" (linkS "x" "http://two.example.com/"))))
   '("https://one.example.com/"
     "http://two.example.com/"))
  
  (check-equal? (grow-space "abcdef" 3) (list 3 3))
  (check-equal? (grow-space "ab def" 3) (list 2 3))
  (check-equal? (grow-space "a  def" 3) (list 1 3))
  (check-equal? (grow-space "   def" 3) (list 0 3))
  (check-equal? (grow-space "    ef" 3) (list 0 4))
  (check-equal? (grow-space "     f" 3) (list 0 5))
  (check-equal? (grow-space "      " 3) (list 0 6))
  (check-equal? (grow-space "abcd" 4) (list 4 4))
  (check-equal? (grow-space "abc " 4) (list 3 4))
  (check-equal? (grow-space "ab  " 4) (list 2 4))
  (check-equal? (grow-space "abcd" 0) (list 0 0))
  (check-equal? (grow-space " bcd" 0) (list 0 1))
  (check-equal? (grow-space "  cd" 0) (list 0 2))

  (check-equal? (space-adjacent? "abc def" 2) #f)
  (check-equal? (space-adjacent? "abc def" 3) #t)
  (check-equal? (space-adjacent? "abc def" 4) #t)
  (check-equal? (space-adjacent? "abc def" 5) #f)
  (check-equal? (space-adjacent? "abc" 0) #f)
  (check-equal? (space-adjacent? "abc" 3) #f)
  (check-equal? (space-adjacent? " bc" 0) #t)
  (check-equal? (space-adjacent? "ab " 3) #t)

  (check-equal? (find-break/leftward "abc  abc abc abcd ebc" 16)
                (list 12 13))
  (check-equal? (find-break/leftward "abc  abc abc abcd ebc" 6)
                (list 3 5))
  (check-equal? (find-break/leftward "abc  abc abc abcd ebc" 5)
                (list 3 5))
  (check-equal? (find-break/leftward "abc  abc abc abcd ebc" 4)
                (list 3 5))
  (check-equal? (find-break/leftward "abc  abc abc abcd ebc" 3)
                (list 3 5))
  (check-equal? (find-break/leftward "abc  abc abc abcd ebc" 2)
                (list 0 0))
  (check-equal? (find-break/leftward " abc" 2) (list 0 1))
  (check-equal? (find-break/leftward " abc" 1) (list 0 1))
  (check-equal? (find-break/leftward " abc" 0) (list 0 0))

  (check-equal? (find-break/leftward "a)cde  abc abc abcd ebc" 3)
                (list 2 2))
  (check-equal? (find-break/leftward "a)cde  abc abc abcd ebc" 2)
                (list 2 2))
  (check-equal? (find-break/leftward "ab(de  abc abc abcd ebc" 3)
                (list 2 2))
  (check-equal? (find-break/leftward "ab(de  abc abc abcd ebc" 2)
                (list 2 2))

  (check-equal? (find-break/rightward "abc  abc abc abcd ebc" 16)
                (list 17 18))
  (check-equal? (find-break/rightward "abc  abc abc abcd ebc" 17)
                (list 17 18))
  (check-equal? (find-break/rightward "abc  abc abc abcd ebc" 18)
                (list 17 18))
  (check-equal? (find-break/rightward "abc  abc abc abcd ebc" 19)
                (list 21 21))
  (check-equal? (find-break/rightward "abc  abc abc abcd ebc" 1)
                (list 3 5))
  (check-equal? (find-break/rightward "abc  abc abc abcd ebc" 4)
                (list 3 5))
  (check-equal? (find-break/rightward "abc " 2) (list 3 4))
  (check-equal? (find-break/rightward "abc " 3) (list 3 4))
  (check-equal? (find-break/rightward "abc " 4) (list 3 4))

  (check-equal? (find-break/rightward "aa)cde"  0)
                (list 3 3))
  (check-equal? (find-break/rightward "aa)cde"  1)
                (list 3 3))
  (check-equal? (find-break/rightward "aa)cde"  2)
                (list 3 3))
  (check-equal? (find-break/rightward "aab(d"  0)
                (list 3 3))
  (check-equal? (find-break/rightward "aab(de" 1)
                (list 3 3))
  (check-equal? (find-break/rightward "aab(de" 2)
                (list 3 3))
  (check-equal? (find-break/rightward "aab(de" 3)
                (list 3 3))


  (check-equal? (parameterize ([max-width 10])
                  (string-reflow "abc abc uhonth huuht othsthaaot)uht"))
                (list
                 "abc abc"
                 "uhonth"
                 "huuht"
                 "othsthaaot)"
                 "uht"))

  (check-equal? (parameterize ([max-width 20])
                  (render-contributors '("Matthew Flatt"
                                         "Stephen Chang"
                                         "Robby Findler")))
                (list "Matthew Flatt,"
                      "Stephen Chang, and"
                      "Robby Findler."))
  )



