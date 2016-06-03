#lang pollen
◊(require css-tools)

◊(make-media-query 24 10.5 1050 40 .25)   

◊(define fgbg "#778")
◊(define fgbg-darker "#556")

@font-face{
    font-family: 'source-serif';
    font-weight: 200;
    font-style: normal;
    font-stretch: normal;
    src: url('fonts/ssp-extralight.woff') format('woff');
}

@font-face{
    font-family: 'source-serif';
    font-weight: 300;
    font-style: normal;
    font-stretch: normal;
    src: url('fonts/ssp-light.woff') format('woff');
}

@font-face{
    font-family: 'source-serif';
    font-weight: 400;
    font-style: normal;
    font-stretch: normal;
    src: url('fonts/ssp-regular.woff') format('woff');
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


◊(define anchor-color "#606")
◊(define anchor-bright-color "#c0c")

a:hover, a:hover span {
  color: ◊|anchor-bright-color|;
}

p a:hover {
    border-bottom: 1px solid ◊|anchor-bright-color|;
}

p a, div a {
  color: ◊|anchor-color|;
  border-bottom: 1px solid ◊|anchor-color|;
}

p {
  margin-bottom: 0.7rem;
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
  line-height: 1.45;
}

div.bio a{
  font-weight: bolder;
}


body {
  font-family: source-serif;
  font-weight: 300;
  margin-left:auto;
  margin-right:auto;
  width:100%;
  max-width:1050px;
  text-rendering: optimizeLegibility;
  font-feature-settings: 'kern' 1;
  font-feature-settings: 'liga' 1;
  background: ◊|fgbg| url('cubit.png') fixed;
  line-height: 1.4;
}

div#doc {
  background: white;
  padding: 2rem;
  padding-top: 3rem;
  border: 1px solid #ccc;
  border-top: 0px;
}

img {
  margin-top: 0.5rem;
  margin-bottom: 0.5rem;
  border: 1px solid ◊|fgbg|;
  border-left: 0;
  border-right: 0;
}

h1 {
  display: inline;
  font-size: 2rem;
  font-weight: 200;
  color: ◊|fgbg|;
  margin-right: 0.25rem;
  position: relative;
  left: -0.25rem; ◊; this impliedly creates space on the right side
}

h2 {
  display: inline;
  color: ◊|fgbg|;
}


h3 {
  font-size: 0.8rem;
  font-weight: 300;
  text-transform: uppercase;
  letter-spacing: 0.05rem;
  color: ◊|fgbg|;
  margin-left: 0;
  margin-top: 1rem;
  margin-bottom: 0.3rem;
}

.speaker-name {
  font-size: 1.6rem;
  font-weight: 200;
  color: ◊|fgbg|;
  margin-right: 0.5rem;
  line-height: 1.4;
}

.keynote-speaker .speaker-name {
  font-size: 2rem;
  line-height: 1.2;
}

h3+div.speaker, h3+div.keynote-speaker {
  margin-top: -0.25rem;
}

.time {
  display: none;
  font-size: 1rem;
}

sponsor+sponsor:before, con+con:before, h2+h2:before {
  content: "\00A0\00A0·\00A0\00A0";
}

.codebox {
display: flex;
  margin-top: 0.5rem;
  margin-bottom: 0.5rem;
  border: 1px solid ◊|fgbg|;
  border-left: 0;
  border-right: 0;
  line-height: 1.2;

  white-space: pre;
  font-family: "Menlo", "Consolas", "Courier New", monospace;
  font-size: 0.75rem;
  height: 20rem;
  overflow: auto;
  background-image: url("eero.svg");
  background-size: cover;
  background-repeat:no-repeat;
  width: 38rem;
  color: ◊|fgbg|;
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

span.title {
  border-bottom: 1px solid ◊|anchor-color|;
}

span.title:hover {
    border-bottom: 1px solid ◊|anchor-bright-color|;
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