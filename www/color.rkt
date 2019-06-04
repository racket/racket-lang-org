#lang racket

(define-syntax-rule
  (provide-colors x ...)
  (provide
   (contract-out
    [x css-color/c] ...)))

(provide-colors
 tab-heading-color
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
(begin
  (define tab-heading-color "#9c27b0")

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

;; the original MB colors (but perhaps applied in a different way than others might?)
#;
(begin

  (define link-color "rgb(6, 121, 167)")
  (define download-button-text-color "#fafaff")
  (define plain-text-color "gray")

  (define tab-heading-color link-color)

  (define unselected-tab-css (~a "background-color: white; color: " link-color "; weight: bold;"))
  (define unselected-tab-hover-css "opacity: 0.6;")

  (define selected-tab-css (~a "background-color: "link-color "; color: white; weight: bold;"))
  (define selected-tab-hover-css "opacity: 0.6;")

  (define site-background-color "white"))
