#lang pollen
◊(require css-tools)

◊(make-media-query 24 10.5 1000 40 .25)   

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

a:hover {
  color: #c0c;
}


body {
  font-family: source-serif;
  font-weight: 300;
  margin-left:auto;
  margin-right:auto;
  width:100%;
  max-width:1000px;
  text-rendering: optimizeLegibility;
  font-feature-settings: 'kern' 1;
  font-feature-settings: 'liga' 1;
  background: ◊|fgbg| url('cubit.png') fixed;
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

h2+h2:before {
  content: "\00A0·\00A0\00A0";
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

h4 {
  display: inline;
  font-size: 2rem;
  font-weight: 200;
  color: ◊|fgbg|;
  margin-right: 0.5rem;
}

h3+div.speaker {
  margin-top: -0.25rem;
}

.time {
  display: none;
  font-size: 1rem;
}

sponsor+sponsor:before, con+con:before {
  content: "\00A0\00A0·\00A0\00A0";
}
