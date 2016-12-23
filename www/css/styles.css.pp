#lang pollen
◊(require css-tools)
◊(define debug-width 0)
◊(define max-width 1000)
◊(define link-color "rgb(6, 121, 167)")

◊(make-media-query 16 8 max-width 60 .5)   

* {
  margin: 0;
  padding: 0;
  line-height: 1.3;
  box-sizing: border-box;
  color: inherit;
  text-decoration: inherit;
}

body {
  ◊(make-css-kerning)
  ◊(make-css-ligatures)
  font-family: "cooper-hewitt";
  font-size: 1.4rem;
  padding: 3rem;
  padding-top: 1rem;
  max-width: ◊|max-width|px;
  margin: auto;
}

p {
  margin-bottom: 1em;
  line-height: 1.4;
}

li > p:last-child {
  margin-bottom: 0em;
}

#doc {
  display: flex;
  flex-direction: column;
  flex-wrap: nowrap;
     border: ◊|debug-width|px solid blue
}


section, top-section {
  width: 100%;
  display: flex;
  flex-direction: row;
  flex-wrap: nowrap;
  align-items: baseline;
}

top-section {
  justify-content: space-between;
  margin-bottom: 0.5rem;
}

.expander {
    display:flex;
    flex-direction: column;
    justify-content: flex-start;
    align-content: flex-start;
    align-items: flex-start;
    border: 1px solid white;
}

.feature {
    height: 100%;
}

.lang {
    ~height: 5.5rem;
    height: 10rem;
}


.expander > div {
    width: 100%;
}

.expander:hover {
    border-color: gray;
}

.expander > .inner {
    flex: 1;
    overflow: hidden;
    background: white;
    color: black;
    transition: opacity 0.3s ease;
    font-size: 90%;
    line-height: 1.5;
    pointer-events: none; /* to prevent it from being clickable while invisible */
}

.expander > .inner,
.expander > p {
    letter-spacing: 0.02em;
}

.feature > .inner {
    opacity: 0;
}

.lang > .inner {
    padding: 0.5rem;
    padding-top: 0;
    padding-bottom: 0.25rem;
    opacity: .3;
}

.lang > .inner tt {
    font-size: 85%;
    white-space: pre;
}


.lang .click-here {
  opacity: 0;
  font-size: 80%;
  margin-left: 1rem;
}

.lang:hover .click-here {
  opacity: 0.8;
}

.lang.active_expander:hover .click-here {
  opacity: 0;
}

.feature:hover > .inner,
.active_expander:hover > .inner {
    opacity: 1;
    pointer-events: auto;
}

.active_expander:hover {
    border: 1px solid gray;
}

.active_expander.feature {
    height: 100%;
}

.active_expander.lang {
    height: 100%;
}

.expander > .name {
    color: black;
    background: rgba(100%, 100%, 100%, 0.7);
    border-bottom: none;
}

.feature > div { ◊;{nested title and content divs}
    padding: 0.8rem;
    padding-top: 0.5rem;
    padding-bottom: 0.25rem
}

.lang > .name {
    padding: 0.25rem;
    font-size: 90%;
  font-family: source-code-pro;
}

ul.doclinks {
◊make-css-columns[#:count 2]
}

ul.doclinks li {
  ◊; padding-bottom or margin-bottom ruins vert alignment, within the column, for mysterious reasons
  display: inline-block;
  width: 100%;
  margin-bottom: 0.25em;
}


div#f1 {
    background: white url("../img/illos/batteries.svg");
    background-size: 100%;
    background-repeat: no-repeat;
}

div#f2 {
    background: white url("../img/illos/oss.svg");
    background-size: 100%;
    background-repeat: no-repeat;
}

div#f3 {
    background: white url("../img/illos/drracket.svg");
    background-size: 100%;
    background-repeat: no-repeat;
}

div#f4 {
    background: white url("../img/illos/scheme.svg");
    background-size: 100%;
    background-repeat: no-repeat;
}

div#f5 {
    background: white url("../img/illos/lang.svg");
    background-size: 100%;
    background-repeat: no-repeat;
}

div#f6 {
    background: white url("../img/illos/platform.svg");
    background-size: 100%;
    background-repeat: no-repeat;
}


.top-button {
  border: 0.1rem solid ◊|link-color|;
  color: ◊|link-color|;
  border-radius: 0.3rem;
  background: #fafaff;
  padding: 0rem 0.6rem;
  font-family: source-code-pro;
  letter-spacing: 0.05em;
  font-size: 80%;
  text-transform: uppercase;
  position: relative;
  top: -0.2rem;
}

.top-button + .top-button {
  margin-left: 0.3rem;
}

.top-button#download:before {
  content: "\e069";
  font-family: feather;
  padding-right: 0.5em;
  position: relative;
  font-size: 90%;
  top: -0.12em;
}

section#samples {
   align-items: flex-start;
}

section.one-column-body-text section-content > li {
  width: 100%;
}

section#logo * {
  margin-bottom: 0;
  padding-bottom: 0;
}

section#bottom {
  font-size: 70%;
}

section#bottom li a {
  display: inline;
}

section#bottom a {
  color: ◊|link-color|;
}

section#book {
   align-items: flex-start;
  }

section {
  padding-top: 0.5rem;
  border-top: 1px solid #ccc;
}

section-title, section-content {
  display: block;
  border: ◊|debug-width|px solid green;
  margin-bottom: 0.5rem;
}

section-title + section-content {
  margin-left: 1rem;
}

section-title {
  width: 24rem;
  font-size: 105%;
  text-align: left;
  padding-right: 1rem;
}

section-content {
 display: flex;
 flex-direction: row;
 justify-content: space-between;
 flex-wrap: wrap;
 width: 100%;
  color:gray;
  font-size: 90%;
}

section-content > li {
  display: block;
  width: 49%;
  padding-bottom: 1rem;
  margin-bottom: 0rem;

}

/* selects lis in sections with a title */
section-title + section-content > li {
  width: 47%; 
}


section#book section-content {
  
  flex-direction: row;
  flex-wrap: wrap;
}

img.logo {
  border: 0px solid green;
  display: inline-block;
  width: 1.15em;
  transform: translate(0,0.15em);
  margin-right: 0.25rem;
}

img.cover {
  width: 5rem;
  height: 6.2rem;
  float: left;
  margin-right: 1rem;
  opacity: 0.6;
}

div.book {
  display: flex;
  width: 50%;
}

pre {
  font-size: 1.1rem;
  line-height: 1.5rem;
}

tt, pre {
  font-family: "source-code-pro";
}

pre a {
  font-weight: bolder;
}

download-button {
  display:inline-block;
  background: ◊|link-color|;
  color: white;
  border: 0.5rem solid ◊|link-color|;
  padding: 0.4rem;
  padding-bottom: 0rem;
}

.arrow-icon {
  background: ◊|link-color|;
  color: white;
  display: inline-block;
}

empty-button {
 display:inline-block;
  border-top: 0.5rem solid white;
 padding-top: 0.4rem;
} 
}

download-button:hover {
  opacity: 0.6;
}

download-button:active {
  opacity: 1;
}

a {
  color: ◊|link-color|;
  font-weight: bolder;
}

#logo a {
  color: inherit;
  font-weight: inherit;
}

a:hover {
  opacity: 0.6;
}

a:active {
  opacity: 1;
}

a, a:hover {
  transition: color 0.2s;
}


@media all and (max-width:650px) {
@media all and (max-width:650px){html {font-size: 16px;}}
@media all and (max-width:590.0px){html {font-size: 15px;}}
@media all and (max-width:530.0px){html {font-size: 14px;}}
  body {padding: 1rem;}
  section {flex-direction: column;}
  section-title {width: 100%; margin-bottom: 1rem;}
  section-content li {width: 100%;}
  .disappearing {display: none;}
  section-title + section-content > li {width: 100%;}
  li > p {width: 95%}
  .lang .click-here { opacity: 0.8;}
  .lang.active_expander .click-here { opacity: 0;}
}

/* smartphones only */
@media all and (max-width:460px) { 
/* set max size for feature box to keep them all the same size */
  .feature { height : 8.5rem; }
  .feature > .inner {display: none;} /* prevents passthrough clicks */
  .active_expander.feature > .inner { display: block; }
  .disappearing-late {display: none;}
  /* smartphones don't do hover effects, so provide fallback behavior */
  .active_expander > .inner {opacity: 1;}
  .expander, .expander:hover, .active_expander:hover {border-color: none;}
  .active_expander {border-color: gray;}

}