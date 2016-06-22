#lang pollen
◊(require css-tools)

◊(define max-width 1100)
◊(make-media-query 24 10.5 max-width 40 .25)   

◊(define fgbg "#778")
◊(define fgbg-darker "#556")

@font-face{
    font-family: 'cooper-hewitt';
    font-weight: 200;
    font-style: normal;
    font-stretch: normal;
    src: url('fonts/CooperHewitt-Book.woff') format('woff');
}


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
  width: 100%;
}


◊(define anchor-color "#900")
◊(define anchor-bright-color "rgb(135, 206, 250)")


a, a:visited {
  color: ◊|anchor-color|;
  text-decoration: none;
  border-bottom: 0.1rem solid ◊|anchor-color|;
  transition: color 0.15s, border-bottom 0.15s;
}

a:hover {
  color: ◊|anchor-bright-color|;
  border-bottom-color: ◊|anchor-bright-color|;
  transition: color 0.15s, border-bottom 0.15s;
}


p {
  margin-bottom: 1rem;
}

div.payload {
  padding: 0.25rem 2rem 0rem 1.7rem;
  border-left: 0.2rem solid #ccc;
  margin-bottom: 1rem;
  font-size: 85%;
  width: 90%;
}


div.bio {
  font-size: 85%;
}

div.bio a{
  font-weight: bolder;
}


body {
  font-family: cooper-hewitt;
  margin-left:auto;
  margin-right:auto;
  width:100%;
  max-width:◊|max-width|px;
  min-height: 2000px;
  text-rendering: optimizeLegibility;
  font-feature-settings: 'kern' 1;
  font-feature-settings: 'liga' 1;

  background: white url('stripe.svg') fixed;
  background-size: 8px; 
  line-height: 1.6;
}

div#doc {
  background: white;
  padding: 2rem;
  padding-left: 6rem;
  padding-top: 3rem;
  padding-bottom: 10rem;
  border: 0px solid black;
  border-top: 0px;
}

h1 {
  display: inline;
  font-size: 2rem;
  font-weight: 200;
  margin-right: 0.25rem;
  position: relative;
  left: -0.25rem; ◊; this impliedly creates space on the right side
}

h2 {
  display: inline;
}

h2 + p {
    margin-top: 1rem;
}


h3 {
  font-size: 1rem;
  font-weight: 300;
  text-transform: uppercase;
  letter-spacing: 0.05rem;
  margin-left: 0;
  margin-top: 1rem;
  margin-bottom: 0.3rem;
}

.speaker-name {
  font-size: 1.6rem;
  font-weight: 200;
  margin-right: 0.5rem;
  line-height: 1.4;
}

span.title {
  border-bottom: 1px solid ◊|anchor-color|;
}

span.title:hover {
    border-bottom: 1px solid ◊|anchor-bright-color|;
}

.keynote-speaker .speaker-name {
  font-size: 2.5rem;
  line-height: 1;
}

.keynote-speaker > span.title {
  border: 0;
  display: block;
  font-size: 2rem;
}




.time {
  display: none;
  font-size: 1rem;
}

sponsor+sponsor:before, con+con:before, h2+h2:before {
  content: "\00A0\00A0·\00A0\00A0";
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

schedule > row > at {
  width: 25%;
}

schedule > row + row {
  border-top: 1px solid #ccc;
}

.two-col {
  ◊(make-css-columns #:count 2);
}
