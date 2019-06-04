#lang racket

(define-syntax-rule
  (provide-colors x ...)
  (provide
   (contract-out
    [x css-color/c] ...)))

(provide-colors
 tab-heading-color
 selected-tab-color
 selected-tab-text-color
 unselected-tab-color
 unselected-tab-text-color
 selected-tab-hover-color
 unselected-tab-hover-color
 selected-tab-hover-text-color
 unselected-tab-hover-text-color
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


(define tab-heading-color "#9c27b0")

(define selected-tab-color "#9c27b0")
(define selected-tab-text-color "white")
(define selected-tab-hover-color "#cccccc")
(define selected-tab-hover-text-color "black")

(define unselected-tab-color "white")
(define unselected-tab-text-color "black")
(define unselected-tab-hover-color "#cccccc")
(define unselected-tab-hover-text-color "black")

(define link-color "rgb(6, 121, 167)")
(define download-button-text-color "#fafaff")
(define plain-text-color "gray")
(define site-background-color "white")
