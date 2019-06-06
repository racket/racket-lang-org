#lang racket

(define-syntax-rule
  (provide-colors x ...)
  (provide
   (contract-out
    [x css-color/c] ...)))

(provide-colors
 tab-heading-color
 tab-heading-text-color
 tab-heading-text-hover-color
 link-color
 plain-text-color
 download-button-text-color
 site-background-color)

(define css-color/c
  (and/c string?
         (or/c "maroon" "red" "orange" "yellow" "olive" "green" "purple"
               "fuchsia" "lime" "teal" "aqua" "blue"
               "navy" "black" "gray" "silver" "white"

               #rx"^[#][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$"

               ;; this one is only an approximation
               #rx"^rgb[(][0-9]+ *, *[0-9]+ *, *[0-9]+[)]$")))

(define css/c
  (and/c string? #rx";$"))
(provide
 (contract-out
  [selected-tab-css css/c]
  [selected-tab-hover-css css/c]
  [unselected-tab-css css/c]
  [unselected-tab-hover-css css/c]))



;; Matthias's colors
#;
(begin
  (define tab-heading-color "#9c27b0")
  (define tab-heading-text-color "white")
  (define tab-heading-text-hover-color tab-heading-text-color)

  (define selected-tab-color "#9c27b0")
  (define selected-tab-text-color "white")
  (define selected-tab-css
    (~a "background-color: " selected-tab-color "; color: " selected-tab-text-color ";"))
  (define selected-tab-hover-color "#cccccc")
  (define selected-tab-hover-text-color "black")
  (define selected-tab-hover-css
    (~a "background-color: " selected-tab-hover-color "; color: " selected-tab-hover-text-color ";"))

  (define unselected-tab-color "white")
  (define unselected-tab-text-color "black")
  (define unselected-tab-css
    (~a "background-color: " unselected-tab-color
        "; color: " unselected-tab-text-color ";"))
  (define unselected-tab-hover-color "#cccccc")
  (define unselected-tab-hover-text-color "black")
  (define unselected-tab-hover-css
    (~a "background-color: " unselected-tab-hover-color
        "; color: " unselected-tab-hover-text-color ";"))

  (define link-color "rgb(6, 121, 167)")
  (define download-button-text-color "#fafaff")
  (define plain-text-color "gray")
  (define site-background-color "white"))

;; the original MB colors (but perhaps with more blue than he would have used)
(begin

  (define link-color "rgb(6, 121, 167)")
  (define download-button-text-color "#fafaff")
  (define plain-text-color "gray")

  (define tab-heading-color link-color)
  (define tab-heading-text-color "white")
  (define tab-heading-text-hover-color tab-heading-text-color)

  (define unselected-tab-css (~a "background-color: white; color: " link-color "; weight: bold;"))
  (define unselected-tab-hover-css "opacity: 0.6;")

  (define selected-tab-css (~a "background-color: "link-color "; color: white; weight: bold;"))
  (define selected-tab-hover-css "opacity: 0.6;")

  (define site-background-color "white"))

#|
from https://hookagency.com/website-color-schemes/
|#

#;
(begin
  ;; 2. Extra Snug
  (define French-Laundry-Blue "#3a4660")
  (define Comfortably-Tan "#c9af98")
  (define Peachy-Kreme "#ed8a63")
  (define Brown-Bonnet "#845007")

  (define site-background-color Comfortably-Tan)
  (define tab-heading-color French-Laundry-Blue)
  (define tab-heading-text-color Comfortably-Tan)
  (define tab-heading-text-hover-color Peachy-Kreme)
  (define unselected-tab-color Comfortably-Tan)
  (define unselected-tab-text-color French-Laundry-Blue)
  (define unselected-tab-hover-color Comfortably-Tan)
  (define unselected-tab-hover-text-color "black")
  (define selected-tab-color French-Laundry-Blue)
  (define selected-tab-text-color Comfortably-Tan)
  (define selected-tab-hover-color French-Laundry-Blue)
  (define selected-tab-hover-text-color Peachy-Kreme)
  (define link-color French-Laundry-Blue)
  (define download-button-text-color French-Laundry-Blue)
  (define plain-text-color Brown-Bonnet)

  (define selected-tab-css
    (~a "background-color: " selected-tab-color "; color: " selected-tab-text-color ";"))
  (define selected-tab-hover-css
    (~a "background-color: " selected-tab-hover-color "; color: " selected-tab-hover-text-color ";"))
  (define unselected-tab-css
    (~a "background-color: " unselected-tab-color
        "; color: " unselected-tab-text-color ";"))
  (define unselected-tab-hover-css
    (~a "background-color: " unselected-tab-hover-color
        "; color: " unselected-tab-hover-text-color ";")))

#|
3. dark horse
Are ya yellow?!: #feda6a
Silver Fox: #d4d4dc
Deep Matte Grey: #393f4d
Dark Slate: #1d1e22

9. sunny and calm
Morning Sky: #CAE4DB – Never underestimate a color palette created by a photograph to set the tone of your design
Honey: #DCAE1D – In this case the palette is set with the photo and then echoed in the subtitle.
Cerulean: #00303F – Cerulean is incredibly classy as a black or dark grey alternative if used consistently throughout
Mist: #7A9D96 – This clean, natural color is established in the photo but could be used on a lower full-width block or buttons as well.

14. goldifox

Golden wheat: #a39274
Soft Wheat: #dfd8c8
Deep gray: #252523

|#
