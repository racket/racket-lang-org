#lang at-exp racket/base
(require xml
         "lib.rkt")

(define-div column
  ,@centered
  [width "45em"])

(define-div banner
  ,@centered)

(define-div title-append
  [display inline-block]
  (margin-left auto)
  (margin-right auto)
  [text-align left]
  [margin-bottom "1em"])

(define-div pagetitle
  [font-size 64]
  [font-family "'Source Code Pro', monospace"])

(define-div main
  [font-family "'Raleway', sans-serif"])

(define header-font
  `([font-weight "bold"]))

(define-div subtitle
  ,@centered
  [font-size 40]
  ,@header-font)

(define-div section
  [margin-top "3em"])
(define-div sectionHeader
  [font-size 24]
  [margin-bottom "1em"]
  ,@header-font)

(define-div speaker
  [font-size 24]
  [color "firebrick"])
(define-div talk
  [font-style "italic"]
  [font-size 24]
  [margin-top "0.25em"]
  [margin-bottom "1em"])

(define-div abstract
  [text-align "left"]
  [margin-left "5em"]
  [margin-right "5em"])

(define-div paragraph)

(define-div plain)

(define-div larger
  [font-size 24])

(define-span faded
  [color "gray"])

(define-div talk-time
  [font-weight bold]
  [position absolute]
  [color "gray"])

(define-div live-link
  [position absolute]
  [right 0]
  [top 0])

(define-div speech
  [position relative])

;; ------------------------------------------------------------

(define (lecture #:when when
                 #:who who
                 #:what what
                 #:more [more ""])
  (speech when
          who
          (live-link (a #:href "tbd" "talk link"))
          what
          more))

;; ------------------------------------------------------------

(define page
  (html
   (head
    (link #:href fonts-url
          #:rel "stylesheet")
    (style (cdata #f #f (classes->string)))
    (style (cdata #f #f "a { text-decoration: none; } "))
    @title{(chaperone (tenth RacketCon))}
    (body
     #:class "main"
     (banner
      (title-append
       @pagetitle[(faded "(chaperone")
                  (img #:style "width:80px; float: right"
                       #:src "https://racket-lang.org/img/racket-logo.svg")]
       @pagetitle[@'nbsp "(tenth RacketCon)" (faded ")")])
      @subtitle{October 16-17, 2020}
      @subtitle[@faded{Online}])
     (column

      (section
       @sectionHeader{Keynote}
       @lecture[#:when @talk-time{Friday, 8:00am}
                #:who @speaker{Kathi Fisler}
                #:what @talk{Let's Talk About Bootstrap}
                #:more
                @abstract{The Bootstrap project is really great and Kathi will tell us all
                              about it. She'll also have an abstract that is way better than
                              this, but it will look something like this one the page.
                              Probably it will carry on for several lines.}])
      
      (section
       @sectionHeader{Talks}

       @lecture[#:when @talk-time{Friday, 9:00am}
                #:who @speaker{Alice Apple}
                #:what @talk{That Time I My Progarm Crashed}]

       @lecture[#:when @talk-time{Friday 10:00am}
                #:who @speaker{Bob Banana}
                #:what @talk{My Program Workd This Time}
                #:more @abstract{Hey, Bob provided an abstract! Sounds like it will be a great talk.}]

       )

      (section
       @sectionHeader{Register}
       @paragraph{Just show up.})

      (section
       @sectionHeader{Previous RacketCons}
       @(apply larger
               (cdr
                (apply
                 append
                 (for/list ([year '(2019 2018 2017 2016 2015 2014 2013 2012 2011)])
                   (list " ∙ "
                         (a #:href (format "https://con.racket-lang.org/~a/" year)
                            (format "~a" year)))))))))))))

;; ------------------------------------------------------------

(provide make)
(define (make p)
  (with-output-to-file
    (build-path p "index.html")
    #:exists 'replace
    (λ ()
      (write-xexpr page))))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path here ".")
  (make here))
