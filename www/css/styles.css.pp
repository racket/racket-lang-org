#lang pollen
◊(require css-tools racket-lang-org/www/color)
◊(define debug-width 0)
◊(define max-width 1050)

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
  background-color: ◊|site-background-color|;
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
  flex-wrap: nowrap;
  align-items: baseline;
}

top-section {
  justify-content: space-between;
  margin-bottom: 0.5rem;
}

section {
 flex-direction: column;
}

/* these two are to get the button layout at
   the top of the page to look reasonable */
@media not all and (max-width:460px) {
 top-section {
   flex-direction: row;
 }
}

@media all and (max-width:460px) {
 top-section {
   flex-direction: column;
 }
}



.selected-tab {
  ◊|selected-tab-css|
}
.selected-tab:hover {
  ◊|selected-tab-hover-css|
}
.unselected-tab {
  ◊|unselected-tab-css|
}
.unselected-tab:hover {
  ◊|unselected-tab-hover-css|
}

.frontpage-card {
  box-shadow:0 2px 5px 0 rgba(0,0,0,0.16),0 2px 10px 0 rgba(0,0,0,0.12)
}

.frontpage-button {
  border:none;
  display:inline-block;
  padding:8px 16px;
  vertical-align:middle;
  overflow:hidden;
  text-decoration:none;
  text-align:center;
  cursor:pointer;
  white-space:nowrap;
  -webkit-touch-callout:none;
  -webkit-user-select:none;
  -khtml-user-select:none;
  -moz-user-select:none;
  -ms-user-select:none;
  user-select:none;
}
.frontpage-button:disabled{cursor:not-allowed;opacity:0.3}

.frontpage-bar {
    display: flex;
    justify-content: space-between;
}

.frontpage-bar-heading {
  cursor:pointer;
  font-size:130%;
  color:◊|tab-heading-text-color|;
  text-align:center;
}

.frontpage-bar-heading:hover {
  color: ◊|tab-heading-text-hover-color|;
}
.frontpage-bar-item {
    font-family: "cooper-hewitt";
    font-size: 100%;
    text-align: center;
    overflow: hidden;
    padding-left: 0;
    padding-right: 0;
    flex: auto;
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
    ~height: 11.5rem;
    height: 21rem;
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
    opacity: .5;
}

.lang .click-here {
  opacity: 0;
  font-size: 80%;
  margin-left: 1rem;
}

.mitem {
  font-size: 50%;
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
    font-family: fira-mono;
    font-weight: bolder;
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
    height: 14rem;
    background: white
    background-size: 100%;
    background-repeat: no-repeat;
}

div#f2 {
    height: 14rem;
    background: white
    background-size: 100%;
    background-repeat: no-repeat;
}

div#f3 {
    height: 14rem;
    background: white
    background-size: 100%;
    background-repeat: no-repeat;
}

div#f4 {
    height: 14rem;
    background: white
    background-size: 100%;
    background-repeat: no-repeat;
}

div#f5 {
    height: 14rem;
    background: white
    background-size: 100%;
    background-repeat: no-repeat;
}

div#f6 {
    height: 14rem;
    background: white
    background-size: 100%;
    background-repeat: no-repeat;
}


.top-button {
  border: 1px solid ◊|link-color|;
  color: ◊|link-color|;
  border-radius: 0.3rem;
  padding: 0.2rem 0.4em;
  font-family: fira-mono;
  letter-spacing: 0.05em;
  font-size: 80%;
  text-transform: uppercase;
  position: relative;
  top: -0.2rem;
}

a.top-button {
  font-weight: inherit;
}

.top-button + .top-button {
  margin-left: 0.3rem;
}

.top-button#download{
  background: ◊|link-color|;
  color: ◊|download-button-text-color|;  
}

section#samples {
   align-items: flex-start;
}

section.one-column-body-text section-content > li {
  width: 100%;
}

section#pull-quote section-content > li {
  padding: 0.5em 0;
  color: #555;
  font-size: 120%;
}

section#logo * {
  margin-bottom: 0;
  padding-bottom: 0;
}

section#bottom, p#bottom {
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
  border-top: 1px solid ◊|tab-heading-color|;
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
  color:◊|plain-text-color|;
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

code, pre {
  font-family: fira-mono;
}

pre {
  margin-bottom: 1em;
  font-size: 85%;
  line-height: 1.65;
}

.narrow {
  width: 400px;
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

.langwww{
  color: rgb(115,115,115);
}

.block-with-1em-margin {
    margin: 1em;
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

.contributormap {
  justify-content: space-around; align-items: center;
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
  .selected-block { display: block; }
  .unselected-block { display: block; }
  .center-if-smartphone{ text-align:center; }
  .disappear-if-smartphone { display:none; }
  .scale-if-smartphone { width: 100% }
  .vertical-in-smartphones-horizontal-otherwise{ display:flex; flex-direction: column; }
  .smartphone-subheading { font-size: 125%; font-weight: bolder; }
  .top-section { flex-direction: column; }
  .contributormap {
     margin: 0 auto;
     width:300px;
     height:320px;
    }
  img.lop-image-rc8 {
    margin-left: auto;
    margin-right: auto;
    max-width: 100%;
    height:auto;
  }
  img.lop-image {
    max-width: 100%;
    height:auto;
  }
  .margin-above-if-smartphone {
    margin: 6px 0 0 0;
  }
  .top-button {
    font-size: 60%;
  }
  .translations-code-example-container { display:block; align-items: center; }
  .big-logo {
    margin-left: auto; margin-right: auto;
    margin-top: 20px; margin-bottom: 30px;
    width: 275px; height: 275px;
  }
}

/* non smartphones */
@media not all and (max-width:460px) {
  /* this causes the non-initial menu content to be hidden
     in regular mode, visible in smartphone mode */
  .full-mode-menu-content { display: none; }
  .selected-block { display: block; }
  .unselected-block { display: none; }
  .frontpage-bar-nowrap-unless-smartphone {
     white-space:nowrap;
  }
  .smartphone-subheading { display:none; }
  .vertical-in-smartphones-horizontal-otherwise{ display:flex; flex-direction: row; }
  .contributormap {
    margin: 0 auto;
    width:640px;
    height:480px;
  }
  img.lop-image-rc8 {
    margin-left: auto;
    margin-right: auto;
    width: 550px;
    height: 318px;
  }
  .translations-code-example-container {
    display:flex;
    align-items: center;
    justify-content:center;
    flex-direction: row;
  }
  .big-logo {
    display: flex;
    justify-content: center;
    flex-direction: column;
    margin-right: 50px; margin-top: 20px; width: 275px; height: 275px;
  }
  .code-to-right-of-big-logo {
    display: flex;
    justify-content: center;
    flex-direction: column;
  }
}

}
