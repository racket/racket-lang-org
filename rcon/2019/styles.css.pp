#lang pollen
◊(require racket/string css-tools)
◊(define dark-color "#000")
◊(define light-color "#eee")
◊(define max-width "1000px")
◊(define anchor-color "#cfc")
◊(define anchor-bright-color "#efe")
◊(define wide-only "@media all and (min-width:1000px)")
◊(define mobile-only "@media all and (max-width:580px)")

◊(ffd "plex-mono" "fonts/IBMPlexMono-Light.ttf" #:base64 #t)
◊(ffd "plex-mono" "fonts/IBMPlexMono-LightItalic.ttf" #:font-style "italic" #:base64 #t)
◊(ffd "plex-mono" "fonts/IBMPlexMono-Bold.ttf" #:font-weight "bold" #:base64 #t)


* {
  padding: 0;
  margin: 0;
  border: 0;
  box-sizing: border-box;
  font-weight: inherit;
  font-style: inherit;
  font-size: inherit;
  text-decoration: inherit;
  color: inherit;
}

html { font-size: 2.4vw; }
◊|wide-only| { html { font-size: 24px; } }
◊|mobile-only| { html{ font-size: 18px; } }

body {
   font-family: plex-mono;
    background: black;
 color: ◊|light-color|;
    margin: 0 auto;
    padding: 3vw;
    width: 100%;
    max-width: ◊|max-width|;
}

p {
  margin-bottom: 1rem;
  line-height: 1.45rem;
  text-shadow:  0 0 0.5em black;
}

.buy-button {
background:none;
font-size:1.4rem;
letter-spacing: 0.05rem;
padding:0.5rem 0.7rem;
display:inline-block;
border: 2px solid;
border-radius: 0.5rem;
font-family:plex-mono;
cursor:pointer;
}

a, .buy-button, .speaker .title {
  font-family: plex-mono;
  font-style: italic;
   font-feature-settings: "ss01", "ss02"; /* two-story a, g */
  color: ◊|anchor-color|;
  border-color: ◊|anchor-color|;
  text-decoration: none;
  transition: color 0.15s, border-bottom 0.15s;
    text-shadow:  0 0 1.00em #fff, 0 0 1.25em #fff;
}

a:hover, .buy-button:hover {
  color: ◊|anchor-bright-color|;
  border-color: ◊|anchor-bright-color|;
  transition: color 0.15s, border-color 0.15s, border-bottom 0.15s;

}

em {
  font-style: italic;
}

strong {
  font-weight: bolder;
}


.bg {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    /* Photo by Wil Stewart https://unsplash.com/photos/T26KCgCPsCI */
    background: black url(stars3b.jpg);
    background-size: 100% 100%;
    opacity: 0.4;
    z-index: -10;
}


.logo {
    text-align: center;
    margin: 2rem;
}

div.svg {
  margin-top: 2rem;
}

div.svg + div.svg {
  margin-top: 0rem;
}

svg {
    opacity: 0.9;
    mix-blend-mode: lighten;
}

◊(define stroker "4")

.pixel-on .top-shape {
  fill: ◊|light-color|;
  stroke-width: ◊|stroker|;
  stroke: ◊|dark-color|;
}

.pixel-on .lower-shape {
  fill: ◊|dark-color|;
  stroke-width: ◊|stroker|;
  stroke: ◊|dark-color|;
}

.pixel-on .lower-shape.layer-0 {
    fill: ◊|light-color|;
    stroke-width: ◊|stroker|;
    stroke: ◊|light-color|;
}

.bg-line {
  stroke: ◊|light-color|;
  stroke-width: 1;
}

.menu-container {
  display: flex;
  display: -webkit-flex;
  flex-direction: row;
  flex-wrap: wrap;
}

h2 {
  padding: 0.5rem;
  font-size: 110%;
  border-top: 1px solid white;
  border-bottom: 1px solid white;
  white-space: nowrap;
  margin-bottom: 1rem;
}

h2 + h2 {
  margin-left: 0.5rem;
}

h2 + p {
    margin-top: 1rem;
}


.speaker-name {
  font-size: 1.6rem;
  font-weight: 200;
  margin-right: 0.5rem;
  line-height: 1.4;
}


.keynote-speaker .speaker-name {
  font-size: 2.5rem;
  line-height: 1;
}

.keynote-speaker > span.title {
  border: 0;
  display: block;
  font-size: 2rem;
  margin-top: 0.5rem;
  line-height: 1.2;
}

.bio {
  margin-top: 0.5rem;
  margin-bottom: 1rem;
}

.time {
  display: none;
  font-size: 1rem;
}

.conlist {
  display: flex;
  display: -webkit-flex;
  flex-wrap: wrap;
  -webkt-flex-wrap: wrap;
}

.conlist div {
  padding: 0.4rem;
}


.opacity-control {
  padding: 0.5rem;
  opacity: 0;
  background: white;
}

.opacity-control:hover {
  opacity: 1;
}

.opacity-control, .opacity-control:hover {
  transition: opacity 0.25s;
}



grid {
  display: flex;
  display: -webkit-flex;
  flex-direction: column;
  -webkit-flex-direction: column;
}

grid > row {
  display: flex;
  display: -webkit-flex;
  flex-direction: row;
  -webkit-flex-direction: row;
  padding-top: 0.4rem;
  padding-bottom: 0.4rem;
}

grid#register > row > at {
  width: 50%;
}

grid#schedule > row > at {
  width: 30%;
}

grid#schedule > row > at + desc {
  width: 65%;
}

grid > row + row {
  border-top: 1px solid #ccc;
}

.foldable {
  cursor: pointer;
}

.speaker-desc {
  margin-bottom: 1rem;
}

.speaker .speaker-name,
.speaker .title {
  display: block;
}

.two-col {
  column-count: 2;
;
}

.two-col .speaker {
  margin-bottom: 0.5rem;
  -webkit-column-break-inside: avoid;
  page-break-inside: avoid;
  break-inside: avoid;
}
