#lang pollen
◊(require css-tools)

◊(define max-width 1100)
◊(make-media-query 24 10.5 max-width 40 .25)   

◊(define mobile-overrides "@media all and (max-width:520px)")


◊(define fgbg "#778")
◊(define fgbg-darker "#556")

@font-face{
    font-family: 'cooper-hewitt';
    font-weight: 200;
    font-style: normal;
    src: url('fonts/CooperHewitt-Book.woff') format('woff');
}

@font-face{
    font-family: 'cooper-hewitt';
    font-weight: 200;
    font-style: italic;
    src: url('fonts/CooperHewitt-BookItalic.woff') format('woff');
}

@font-face{
    font-family: 'bungee-shade';
    font-weight: normal;
    font-style: normal;
    src: url('fonts/Bungee-Shade.woff') format('woff');
}

@font-face{
    font-family: 'bungee-regular';
    font-weight: normal;
    font-style: normal;
    src: url('fonts/Bungee-Regular.woff') format('woff');
}

@font-face{
    font-family: 'bungee-outline';
    font-weight: normal;
    font-style: normal;
    src: url('fonts/Bungee-Outline.woff') format('woff');
}

@font-face{
    font-family: 'bungee-hairline';
    font-weight: normal;
    font-style: normal;
    src: url('fonts/Bungee-Hairline.woff') format('woff');
}

@font-face{
    font-family: 'bungee-inline';
    font-weight: normal;
    font-style: normal;
    src: url('fonts/Bungee-Inline.woff') format('woff');
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

tt {
  font-family: "Source Code Pro", monospace;
  font-size: 90%;
}

◊(define anchor-color "magenta")
◊(define anchor-bright-color "cyan")


a, a:visited {
  color: ◊|anchor-color|;
  text-decoration: none;
  border-bottom: 0.08rem solid ◊|anchor-color|;
  transition: color 0.15s, border-bottom 0.15s;
}

a:hover {
  color: ◊|anchor-bright-color|;
  border-bottom-color: ◊|anchor-bright-color|;
  transition: color 0.15s, border-bottom 0.15s;
}


p {
  margin-bottom: 1rem;
  mix-blend-mode: darken;
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



div.bio {
  font-size: 90%;
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

em {
  font-style: italic;
}

div#doc {
  background: white;
  padding-left: 6rem;
  padding-right: 2rem;
  padding-top: 2rem;
  padding-bottom: 10rem;
  border: 0px solid black;
  border-top: 0px;
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

.head div {
  position: absolute;
  z-index: -1000;
}

.head#a {
  font-family: bungee-outline;
}

.head#b {
  font-family: bungee-regular;
}

.head#b .w { font-family: bungee-outline; }

.head#c {
  font-family: bungee-outline;
}

.head#c .w { font-family: bungee-regular; 
text-shadow: 4px 4px grey;}

.c, .m, .y {mix-blend-mode: multiply;}
.c {color: cyan;}
.m {color: magenta;}
.y {color: yellow;}
.w {color: white;}

.movable { transition: transform 6s linear; }

h2 {
  display: inline;
}

h2 + p {
    margin-top: 1rem;
}


h3 {
  font-size: 2rem;
  font-weight: 300;
  font-family: bungee-shade;
  line-height: 1;
  text-transform: uppercase;
  letter-spacing: 0.05rem;
  margin-left: 0;
  margin-top: 1rem;
  margin-bottom: 0.5rem;
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

.two-col .speaker {
  margin-bottom: 0.5rem;
}


◊|mobile-overrides| {
    html{
      font-size: 18px;
    }

    div#doc {
      padding-top: 1.5rem;
      padding-left: 1.5rem;
      padding-right: 0.75rem;
    }

    .head {
        font-size: 2.5rem;
        height: 2rem;
        top: 0rem;
        left: 1rem;
    }

  .two-col {
    ◊(make-css-columns #:count 1);
  }
}

