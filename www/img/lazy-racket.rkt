#lang at-exp racket

(require pict pict/color)
(provide (all-defined-out))

(module utils racket
  (provide (all-defined-out))
  (require (except-in scribble/core table)
           scribble/base
           racket/draw
           pict
           pict/code
           ppict/tag)

  (define (write-png file-name the-image)
    (define pdf-dc (new pdf-dc%
                        [interactive #f]
                        [output file-name]
                        [width (pict-width the-image)]
                        [height (pict-height the-image)]))
    (send pdf-dc start-doc "")
    (send pdf-dc start-page)
    (send pdf-dc set-smoothing 'aligned)
    (draw-pict the-image pdf-dc 0 0)
    (send pdf-dc end-page)
    (send pdf-dc end-doc)
    (void))

  (define (exact . items)
    (make-element (make-style "identity" '(exact-chars))
                  items))

  (define (m . items)
    (make-element (make-style "identity" '(exact-chars))
                  `("$" ,@items "$")))

  (define (mm . items)
    (make-element (make-style "identity" '(exact-chars))
                  `("\\[" ,@items "\\]")))

  (define (paragraph title)
    (make-element (make-style "identity" '(exact-chars))
                  `("\\paragraph{" ,title "}")))

  (define (->text . text)
    (make-element (make-style "identity" '(exact-chars))
                  `("\\begin{tikzpicture}"
                    "\\node (0) {};"
                    "\\node [left of=0, xshift=3.2cm] (1) {};"
                    "\\draw[->,transform canvas={yshift=-1mm}] (0) -- node[yshift=1mm]"
                    ,(format "{\\tiny ~a} (1);" (apply string-append text))
                    "\\end{tikzpicture}")))

  (define at-char "@")

  (define dot (find-executable-path "dot"))

  (define (dot->pict . graph)
    (define-values (in out) (make-pipe))
    (with-input-from-string (apply string-append graph)
      (λ () (parameterize ([current-output-port out])
              (system* dot "-Tpng"))))
    (define b (make-object bitmap% 1 1))
    (send b load-file in)
    (bitmap b))

  (define matthias-suffix (if (regexp-match #px"matthias" (current-directory)) " O" ""))
  (define code-font (list "Linux Libertine Mono" "Linux Libertine Mono O"))
  (define text-font (list "Linux Libertine" "Linux Libertine O"))
  (define small-font-size 10)
  (define font-size 15)
  (define small-scale-factor 0.8)
  (define code-line-sep 10)

  ; Test to make sure fonts are installed:
  (let ([tmp (set-intersect (get-face-list) code-font)])
    (if (set-empty? tmp)
        (raise-user-error 'paper "Please install '~a' font: http://www.linuxlibertine.org" (first code-font))
        (set! code-font (set-first tmp))))
  (let ([tmp (set-intersect (get-face-list) text-font)])
    (if (set-empty? tmp)
        (raise-user-error 'paper "Please install '~a' font: http://www.linuxlibertine.org" (first text-font))
        (set! text-font (set-first tmp))))

  (define-syntax-rule (mod->pict modname lang content ...)
    (mod->pict* #:codeblock? #f modname lang content ...))

  (define-syntax-rule (modblock->pict modname lang content ...)
    (mod->pict* #:codeblock? #t modname lang content ...))

  (define-syntax-rule (mod->pict* #:codeblock? codeblock? modname lang content ...)
    (let ()
      (define buffer 10)
      (define c1
        (scale (vl-append 2
                          (hbl-append (colorize ((current-code-tt) "#lang ") (current-keyword-color))
                                      (colorize ((current-code-tt) lang) (current-id-color)))
                          (if codeblock?
                              (codeblock-pict
                               #:keep-lang-line? #f
                               (~a "#lang" lang "\n" content ...))
                              (code content ...)))
               0.75))
      (define title (text modname null 7))
      (vl-append
       (cc-superimpose title (rectangle (+ buffer (pict-width title))
                                        (+ buffer (pict-height title))
                                        #:border-color "dim gray"))
       (cc-superimpose c1 (rectangle (+ buffer (pict-width c1))
                                     (+ buffer (pict-height c1))
                                     #:border-color "dim gray")))))

  (define (vsplit-figure a b #:space [space 25])
    (vc-append
     a
     (blank space)
     (linewidth 0
                (hline (max (pict-width a) (pict-width b)) 1))
     (blank space)
     b))

  (define (make-playlist-timeline #:distance [distance 4]
                                  #:end [end #f]
                                  #:font-size [font-size small-font-size]
                                  . trace)
    (define frames
      (apply hc-append distance trace))
    (vc-append
     15
     frames
     (let ([p (hc-append (pict-width frames)
                         (tag-pict (vline 1 10) 'start)
                         (tag-pict (if end (vline 1 10) (blank)) 'end))])
       (pin-arrow-line 5 p #:label (text "time" text-font font-size)
                       (find-tag p 'start) cc-find
                       (find-tag p 'end) cc-find))))

  (define (ellipses #:offset [offset 3]
                    #:size [size 2])
    (hc-append
     offset
     (disk size)
     (disk size)
     (disk size)))

  (define (clip-scale p)
    (scale-1080p p 30))

  (define (scale-1080p p w-size)
    (define w (pict-width p))
    (define h (pict-height p))
    (define h-size (* w-size 9/16))
    (scale p
           (/ w-size w)
           (/ h-size h)))

  (define (code-pict code)
    (nested #:style (style 'code-inset '(never-indents))
            code))

  (define (split-minipage a b #:split-location [split-location 0.5]
                          #:direction [direction "c"]
                          #:indent-offset [indent-amt 0.1])
    (centered
     (list
      @exact{\vspace{0.5em}\begin{minipage}[@direction]{@(number->string indent-amt)\textwidth}%
 \end{minipage}%
 \begin{minipage}[@direction]{@(number->string (- split-location indent-amt))\textwidth}}
      a
      @exact{\end{minipage}\begin{minipage}[@direction]{@(number->string (- 1 split-location))\textwidth}}
      b
      @exact{\end{minipage}\vspace{0.5em}})))

  (define (3split-minipage a b c
                           #:size-a [size-a 1/3]
                           #:size-b [size-b 1/3]
                           #:size-c [size-c 1/3]
                           #:direction [direction "c"])
    (centered
     (list
      @exact{\begin{minipage}[@direction]{@(number->string size-a)\textwidth}}
      a
      @exact{\end{minipage}\begin{minipage}[@direction]{@(number->string size-b)\textwidth}}
      b
      @exact{\end{minipage}\begin{minipage}[@direction]{@(number->string size-c)\textwidth}}
      c
      @exact{\end{minipage}})))

  (define (minipage #:size [size 1] . a)
    (append (list @exact{\begin{minipage}{@(number->string size)\textwidth}})
            a
            (list @exact{\end{minipage}})))

  (define (TODO . content)
    (elem #:style (style #f (list (color-property "red")))
          content))

  (define (type-table . table)
    (make-element (make-style "identity" '(exact-chars))
                  `("\\begin{align*}"
                    ,@(add-between
                       (for/list ([i (in-list table)])
                         (format "\\textit{~a} :&\\ ~a" (first i) (type->latex-str (second i))))
                       "\\\\")
                    "\\end{align*}")))

  (define parenize (make-parameter #f))
  (define (type->latex-str type)
    (match type
      [`(-> ,type* ... ,ret-type)
       (format (if (parenize) "(~a \\rightarrow ~a)" "~a \\rightarrow ~a")
               (parameterize ([parenize #t])
                 (string-join (map type->latex-str type*)
                              "\\; "))
               (type->latex-str ret-type))]
      [`(U ,type* ...)
       (format (if (parenize) "(~a)" "~a")
               (parameterize ([parenize #t])
                 (string-join (map type->latex-str type*)
                              " \\mid ")))]
      [`(U/man ,type* ...)
       (format (if (parenize) "(~a)" "~a")
               (parameterize ([parenize #t])
                 (map type->latex-str type*)))]
      [`(X ,type* ...)
       (format (if (parenize) "(~a)" "~a")
               (parameterize ([parenize #t])
                 (string-join (map type->latex-str type*)
                              " \\times ")))]
      [`(List ,type)
       (format "[~a\\: \\cdots]"
               (type->latex-str type))]
      [`(Ghost ,text)
       (format "\\phantom{~a}" text)]
      [`(Const ,text)
       text]
      [#\newline "\\\\ & "]
      [(? keyword?) (format "\\texttt{\\~a}" type)]
      [_ (format "\\mathsf{~a}" type)]))


  )

(require 'utils)

(define hello-green
  (vc-append
   10
   (mod->pict "green.vid" "video" (clip "green"))
   (hline 200 0)
   (scale (bitmap "res/sample.png") 0.08)))

(define nlve-sample
  (scale (bitmap "res/nlve-demo.png") 0.14))

(define blank-rect
  (clip-scale (blank 1 1)))

(define circ-image
  (cc-superimpose blank-rect
                  (disk 10 #:color "yellow")))

(define t#
  (case-lambda
    [(n) (t# "clip" n)]
    [(t n)
     (clip-frame (bitmap (build-path "rconframes"
                                     (format "~a~a.png" t (~a n
                                                              #:left-pad-string "0"
                                                              #:min-width 5
                                                              #:max-width 5
                                                              #:align 'right)))))]))

(define rect-clip-frames 10)
(define rect-clip
  (build-list rect-clip-frames
              (λ (f#)
                (cc-superimpose blank-rect
                                (rotate (filled-rectangle 15 15 #:color "red")
                                        (/ (* f# pi) rect-clip-frames 2))))))

(define (clip-frame clip)
  (cc-superimpose (clip-scale (rectangle 50 50))
                  (clip-scale clip)))

(define rcon-timeline
  (make-playlist-timeline
   #:end #t
   (clip-scale (bitmap "res/rcon.png"))
   (ellipses)
   (clip-scale (bitmap "res/geoffrey.jpg"))
   (ellipses)
   (t# "nlpip" 3)
   (ellipses)
   (t# "nlpip" 6)
   (ellipses)
   (t# "nlpip" 9)
   (ellipses)
   (t# "nlpip" 12)
   (ellipses)
   (t# "nlpip" 15)
   (ellipses)
   (t# "nlpip" 18)
   (ellipses)
   (clip-scale (bitmap "res/alexis.jpg"))
   (ellipses)
   (clip-scale (bitmap "res/rcon.png"))))

(define talk-list
  (if (directory-exists? "rconframes")
      (sort
       (append*
        (for/list ([f (directory-list "rconframes"
                                      #:build? #t)])
          (if (equal? (path-get-extension f) #".jpg")
              (list f)
              (list))))
       <=
       #:key (λ (key)
               (string->number (car (regexp-match #rx"[0-9]+"
                                                  (path->string key))))))
      (make-list 10000 "missing-file.png")))


(define elided
  (text "⟨⋯elided⋯⟩" text-font small-font-size))

(define splash
  (clip-frame (bitmap "res/rcon.png")))

(define splash2
  (clip-frame (vc-append
               (hc-append (bitmap "res/racket.png")
                          (bitmap "res/racket.png")
                          (bitmap "res/racket.png"))
               (hc-append (bitmap "res/racket.png")
                          (bitmap "res/racket.png")
                          (bitmap "res/racket.png")))))


(define (language-tower s)
  (define (t str (delta 0)) (text str text-font (- font-size delta)))
  (define *no 0)
  (define (L w h str)
    (set! *no (+ *no 1))
    (define language-no (t (number->string *no) 2))
    (define language-circle (cc-superimpose (ghost (rectangle 20 20)) (circle 16) language-no))
    (define language-box   (rectangle (+ w 20) h))
    (define language-label (t str))
    (rt-superimpose (cc-superimpose language-box language-label) language-circle))
  (define ((δ rt-find delta-w) a b)
    (define-values (w h) (rt-find a b))
    (values (- w delta-w) h))

  (define video     (L  90 30 "Video"))
  (define video-doc (L  90 30 "Video Docs"))
  (define video-ffi (L  90 30 "Video FFI"))
  (define t-video   (L  90 30 "Typed Video"))
  (define turnstyle (L  90 30 "Turnstile"))
  (define scribble  (L 120 30 "Scribble"))
  (define syn-parse (L 170 30 "Syntax Parse"))
  (define racket    (L 370 30 "Racket"))

  (define builds-on (t "builds on"))
  (define extends   (t "extends"))
  
  (let* ([acc (vc-append 200
                         (hc-append 60 (blank 20)
                                    (vc-append 130
                                               video-doc
                                               scribble)
                                    (vc-append 50
                                               (vc-append 50
                                                          (hc-append 70 video t-video)
                                                          (hc-append 70 video-ffi turnstyle))
                                               syn-parse) (blank 20))
                         racket)]
         [pin (λ (l1 find1 l2 find2 label (x-adjust 0) (sa #f) (ea #f) (sp 1/4) (ep 1/4))
                (set! acc
                      (pin-arrow-line 8 acc l1 find1 l2 find2
                                      #:x-adjust-label x-adjust #:label label
                                      #:start-angle sa #:end-angle ea #:start-pull sp #:end-pull ep))
                acc)]
         
         [acc (pin syn-parse cb-find racket    (δ ct-find -90) builds-on -40)]
         [acc (pin video-ffi cb-find syn-parse (δ lt-find  -5) builds-on  25)]
         [acc (pin turnstyle cb-find syn-parse (δ rt-find   5) builds-on -40)]
         [acc (pin video-ffi lc-find racket    (δ ct-find  50) builds-on  15 pi (* pi -1/2) 3/5 3/5)]
         [acc (pin video     cb-find video-ffi ct-find         builds-on  25)]
         [acc (pin t-video   cb-find turnstyle ct-find         builds-on -40)]
         [acc (pin t-video   lc-find video     rc-find         extends)]
         [acc (pin video-doc rc-find video     lc-find         extends)]
         [acc (pin video-doc cb-find scribble  ct-find         builds-on 25)]
         [acc (pin scribble  cb-find racket    (δ ct-find 155) builds-on 25)]
         [δ-rb (δ rb-find 20)]
         [acc (pin turnstyle δ-rb  racket      (δ rt-find 50)  builds-on  52 (* pi -1/4) (* pi 7/6))])
    (scale acc s)))

(define pict (language-tower 1.))
(define bmap (pict->bitmap pict))
(send bmap save-file "lazy-racket.png" 'png #:unscaled? #t)