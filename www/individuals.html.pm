#lang pollen

◊(require (only-in "index.html.pm" top)
          racket/list racket/string
          racket/format)

◊(define-values (first-half second-half)
(let ()
(define anonymous-count 37)

(define names
  '("Alex Knauth"
"Geoffrey Knauth"
"Laurent Orseau"
"Winston Weinert"
"Huma Zafar"
"Jesse Alama"
"David Alkire"
"Andrew Blinn"
"Bogdan Popa"
"Andrei Formiga"
"Martin Connolly"
"Mario Rodas"
"Joel Dueck"
"Sam Tobin-Hochstadt"
"John Donnellan"
"Sean Jensen-Grey"
"Brian Adkins"
"James Emmott"
"Xu Xue"
"Dyllon Gagnier"
"Stephen DeGabrielle"
"Ross McKinlay"
"Fred Fu"
"Jack Firth"
"S Furnace"
"Markéta Lisová"
"Robert Postill"
"Alex Harsányi"
"Hari Krish"
"Aaron Ang"
"Byron Davies"
))

(define sorted
  (append
   (sort (sort names string<?)
         string<?
         #:key (λ (x) (second (string-split x))))
   (list (~a "and "
             anonymous-count
             " anonymous"
             " individual"
             (if (= anonymous-count 1) "" "s")))))

(define (->html lst)
  (for/list ([e (in-list lst)])
        `(div ,e)))

(define first-column-size
  ;; use floor here so if there is
  ;; an odd number, the anonymous
  ;; line will be on its own
  (floor (/ (length sorted) 2)))

(define first-column (->html (take sorted first-column-size)))
(define second-column (->html (drop sorted first-column-size)))
(values first-column second-column)))

◊(top)

◊section[#:class "one-column-body-text"]{
Individual Supporters

◊p[#:style "font-size: 80%;margin-top: 1rem;color:gray;width:80%;line-height:1.5"]{
Thank you to the many individuals who for their generous support over the years.}

◊p[#:style "font-size: 80%;margin-top: 1rem;color:gray;width:80%;line-height:1.5"]{
Donations are used for hosting community infrastructure, administration, educational 
outreach, and community events such as RacketCon and Racket School.}

◊p[#:style "font-size: 80%;margin-top: 1rem;color:gray;width:80%;line-height:1.5"]{See 
◊link["sfc.html"]{Software Freedom Conservancy} to make a tax-deductible contribution 
to support our work.
}

◊section{

◊div[#:id "individual"]{Individual Supporters

◊p[#:style "font-size: 80%;margin-top: 1rem;color:gray;width:80%;line-height:1.5"]{
Thank you to the many individuals who for their generous support over the years.}

◊p[#:style "font-size: 80%;margin-top: 1rem;color:gray;width:80%;line-height:1.5"]{
Donations are used for hosting community infrastructure, administration, educational 
outreach, and community events such as RacketCon and Racket School.}

◊p[#:style "font-size: 80%;margin-top: 1rem;color:gray;width:80%;line-height:1.5"]{See 
◊link["sfc.html"]{Software Freedom Conservancy} to make a tax-deductible contribution 
to support our work.
}}

◊div[#:class "individuals-outer-div"]{
 ◊(apply div #:class "individuals-left-div" first-half)
 ◊(apply div #:class "individuals-right-div" second-half)
}
}
