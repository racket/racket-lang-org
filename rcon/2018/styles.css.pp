#lang pollen
◊(require css-tools)

◊(define max-width 1100)
◊(make-media-query 24 10.5 max-width 40 .25)   

◊(define mobile-overrides "@media all and (max-width:520px)")

◊(define bg "#fdf6e3")
◊(define fg "#657b83")

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

◊(define anchor-color rcon-blue)
◊(define anchor-bright-color rcon-red)

/* links */
a, a:visited {
  color: ◊|anchor-color|;
  text-decoration: none;
  border-bottom: 0.15rem solid ◊|anchor-color|;
  transition: color 0.15s, border-bottom 0.15s;
  font-weight: bold;
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
  font-family: "Source Sans Pro", sans-serif;
  margin-left:auto;
  margin-right:auto;
  width:100%;
  max-width:◊|max-width|px;
  min-height: 2000px;
  text-rendering: optimizeLegibility;
  font-feature-settings: 'kern' 1;
  font-feature-settings: 'liga' 1;

  line-height: 1.6;

  color: ◊|fg|;
  background-color: ◊|bg|;
}

em {
  font-style: italic;
}

strong {
  font-weight: bold;
}

div#doc {
  padding-left: 6rem;
  padding-right: 2rem;
  padding-top: 2rem;
  padding-bottom: 10rem;
  border: 0px solid black;
  border-top: 0px;

  color: ◊|fg|;
  background-color: ◊|bg|;
}

.head {
  font-family: "Source Code Pro", monospace;
  font-size: 60%;
  font-weight: 900;
  line-height: 0.9;
  white-space: pre;
  color: ◊|fg|;
  margin-bottom: 0em;
}

h2 {
  display: inline;
}

h2 + p {
    margin-top: 1rem;
}

h3 {
  font-size: 2rem;
  font-weight: 300;
  font-family: "Source Code Pro", monospace; 
  line-height: 1;
  text-transform: uppercase;
  letter-spacing: 0.05rem;
  margin-left: 0;
  margin-top: 2rem;
  margin-bottom: 0.8rem;
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

.time {
  display: none;
  font-size: 1rem;
}

sponsor {
    margin-right: 0.5em;
}

sponsor span {
    margin-left: 0.5em;
    color: ◊|fg|;
}

sponsor+sponsor:before, con+con:before, h2+h2:before {
  content: "·";
  padding: 0.5rem;
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
  -webkit-column-break-inside: avoid;
  page-break-inside: avoid;
  break-inside: avoid;
}

dont ◊|mobile-overrides| {
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

    h2 {
      display: list-item;
    }

    h2+h2:before {
      content: "";
      padding: 0;
    }

  .two-col {
    ◊(make-css-columns #:count 1);
  }
}

