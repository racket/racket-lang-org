#lang pollen
◊(require css-tools)

/* Charter license */
/* (c) Copyright 1989-1992, Bitstream Inc., Cambridge, MA. You are hereby granted permission under all Bitstream propriety rights to use, copy, modify, sublicense, sell, and redistribute the 4 Bitstream Charter (r) Type 1 outline fonts and the 4 Courier Type 1 outline fonts for any purpose and without restriction; provided, that this notice is left intact on all copies of such fonts and that Bitstream's trademark is acknowledged as shown below on all unmodified copies of the 4 Charter Type 1 fonts. BITSTREAM CHARTER is a registered trademark of Bitstream Inc. */

◊; these charter woffs are from manual-fonts.css in the Racket docs
◊(ffd "charter-web" "fonts/charter_regular.woff" #:base64 #t)
◊(ffd "charter-web" "fonts/charter_bold.woff" #:font-weight "bold" #:base64 #t)
◊(ffd "charter-web" "fonts/charter_italic.woff" #:font-style "italic" #:base64 #t)

◊(ffd "bungee-regular" "fonts/bungee_regular.woff" #:base64 #t)


◊(define max-viewport-width 1100)

◊(define wide-overrides (format "@media all and (min-width:~apx)" max-viewport-width))
◊(define mobile-overrides "@media all and (max-width:580px)")

* {
  padding: 0;
  margin: 0;
  border: 0;
  box-sizing: border-box;
  font-weight: inherit;
  font-style: inherit;
  text-decoration: inherit;
  color: inherit;
}

◊(define default-vw-font-size 2.4)
html { font-size: ◊(format "~avw" ◊|default-vw-font-size|); }
◊|wide-overrides| { html { font-size: ◊(format "~a" (* default-vw-font-size max-viewport-width .01))px; } }
◊|mobile-overrides| { html{ font-size: 18px; } }

◊(define anchor-color "purple")
◊(define anchor-bright-color "pink")


/* links */
a, a:visited {
  color: ◊|anchor-color|;
  text-decoration: none;
  border-bottom: 1px dotted purple;
  font-weight: bold;
}

a:hover {
  color: ◊|anchor-bright-color|;
  border-bottom-color: ◊|anchor-bright-color|;
  transition: color 0.15s, border-bottom 0.15s;
}


p, li{
  margin-bottom: 1rem;
  mix-blend-mode: darken;
}

li {
  margin-left: 2rem;
  padding-right: 2rem;
}

div.subhead {
  margin-top: 0.5rem;
}

div.payload {
  padding-bottom: 1rem;
  margin-bottom: 1rem;
  font-size: 90%;
  width: 100%;
}



.bio {
  font-size: 90%;
}

.bio a{
  font-weight: bolder;
}

.bio img {
  width: 6rem;
  float: left;
  margin-right: 1rem;
}


body {
  font-family: charter-web;
  margin: 0 auto;
  width:100%;
  color: ◊|dark-gray|;
  max-width:◊|max-viewport-width|px;
  min-height: 2000px;
  -webkit-font-smoothing: subpixel-antialiased; /* corrects safari rendering */
  font-feature-settings: 'kern' 1, 'liga' 1;
  background: ◊|dark-gray|;
  line-height: 1.6;
}

em {
  font-style: italic;
}

strong {
  font-weight: bold;
}

div#doc {
  position: relative;
  background: white;
  padding-left: 4rem;
  padding-right: 4rem;
  padding-bottom: 6rem;
}

 #top-images {
      position: sticky;
      height: 14rem;
      width: 100%;
      top: 0;
      background-image: url("craig-whitehead-546355-unsplash-sm.jpg");
      background-repeat: repeat;
      background-size: 100% 14rem;
  }


.head {
  font-size: 5rem;
  height: 4.5rem;
  line-height: 1;
  transform: skew(-6deg, -12deg);
  ~filter:blur(2px);
  position: relative;
  top: -1rem;
  left: -2rem;

}

◊(define dark-gray "#333")

h1 {
  color: ◊|dark-gray|;
  font-size:2.85rem;
font-family: bungee-regular;
line-height: 0.9;
background: white;
mix-blend-mode: lighten;
padding: 1.5rem 4rem 0.5rem 4rem;
display: block;
}

h1 + h1 {
font-size:2rem;
font-family: bungee-regular;
color: white;
background: none;
padding: 0.5rem 4rem 0.5rem 4rem;
mix-blend-mode: lighten;
display: block;
}

h2 {
  display: inline;
  font-size: 1.1rem;
  line-height: 1.4;
}

h2 + p {
    margin-top: 1rem;
}


h3 {
  background: none;
  font-size: 1.5rem;
  font-weight: 200;
  font-family: bungee-regular;
  color: ◊|dark-gray|;
  border-top: 2px solid ◊|dark-gray|;
  line-height: 1;
  margin-top: 2rem;
  padding-top: 0.2em;
  margin-bottom: 1rem;
  width:102%;
}

.speaker-name {
  font-size: 1.6rem;
  font-weight: 200;
  margin-right: 0.5rem;
  line-height: 1.4;
}

span.title {
  border-bottom: 0.08rem solid ◊|anchor-color|;
}

a:hover span.title {
    border-bottom-color: ◊|anchor-bright-color|;    
   transition: border-bottom-color 0.15s;
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




.time {
  display: none;
  font-size: 1rem;
}

sponsor+sponsor:before, con+con:before, h2+h2:before {
  content: " • ";
  padding: 0.25rem;
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



schedule {
  display: flex;
  display: -webkit-flex;
  flex-direction: column;
  -webkit-flex-direction: column;
}

schedule > row {
  display: flex;
  display: -webkit-flex;
  flex-direction: row; 
  -webkit-flex-direction: row;
  padding-top: 0.2rem; 
  padding-bottom: 0.2rem; 
}

schedule > row > at,
schedule > row > at + desc {
  width: 50%;
}

schedule > row + row {
  border-top: 1px solid #ccc;
}

.two-col {
  ◊(make-css-columns #:count 2);
}

.two-col .speaker {
  margin-bottom: 0.5rem;
  -webkit-column-break-inside: avoid;
  page-break-inside: avoid;
  break-inside: avoid;
}


/* Set a size for our map container, the Google Map will take up 100% of this container */
#map {
    max-width: 900px;
    width: 80vw;
    height: 40vw;
}
#map * {
    width: auto;
}

#map a {
    border: none;
    font-weight: normal;
}

◊|mobile-overrides| {
    div#doc, h1, h1+h1 {
      padding-left: 1.5rem;
      padding-right: 1.5rem;
    }

    .head {
        font-size: 2.5rem;
        height: 2rem;
        top: 0rem;
        left: 1rem;
    }

    h2 {
      display: list-item;
      list-style-type: none;
    }

    h2+h2:before {
      content: "";
      padding: 0;
    }

  .two-col {
    ◊(make-css-columns #:count 1);
  }
}

