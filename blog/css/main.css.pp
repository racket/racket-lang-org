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
  font-size: 1.5rem;
  padding: 3rem;
  padding-top: 0rem;
  max-width: ◊|max-width|px;
  margin: auto;
}

#doc {
  display: flex;
  flex-direction: column;
  flex-wrap: nowrap;
     border: ◊|debug-width|px solid blue
}


row {
  width: 100%;
  display: flex;
  flex-direction: row;
  flex-wrap: nowrap;
  border: ◊|debug-width|px solid red;
  align-items: baseline;
}

row#logo {
  font-size: 140%;
  border: none;
  margin: 0;
  padding: 0;
  margin-top: 0.5rem;
  margin-bottom: -0.5rem;
}

div#logo-container {
  white-space: nowrap; /* keeps logo and word on same line */
}

row#samples {
   align-items: flex-start;
}

row.one-column col-2 > li {
  width: 100%;
}

row#logo * {
  margin-bottom: 0;
  padding-bottom: 0;
}

row#logo a {
  color: inherit;
}

row#logo a:hover {
  text-decoration: inherit;
}

row#bottom {
  padding-top: 0.75rem;
  font-size: 70%;
}

row#bottom li a {
  display: inline;
}

row#bottom a {
  color: ◊|link-color|;
}

row#book {
   align-items: flex-start;
  }


◊(define row-border-top "1px solid #ccc")
row {
  margin-top: 0.5rem;
  padding-top: 0.5rem;
  border-top: ◊|row-border-top|;
}



col-1, col-2 {
  display: block;
  border: ◊|debug-width|px solid green;
  margin-bottom: 0.5rem;
}

col-1 + col-2 {
  margin-left: 1rem;
}

col-1 {
  width: 20rem;
  font-size: 115%;
  text-align: right;
  padding-right: 1rem;
}

col-2 {
 display: flex;
 flex-direction: row;
 flex-wrap: wrap;
 width: 100%;
}

article#home col-2 div.truncate {
  opacity: 0.6;
}



row#book col-2 {
  
  flex-direction: row;
  flex-wrap: wrap;
}

img#logo {
  display: inline-block;
  width: 2.7rem;
  transform: translate(0,0.15em);
  margin-right: 0.25rem;
}


img.cover {
  width: 5rem;
  height: 6.2rem;
  float: left;
  margin-right: 1rem;
  opacity: ◊|hover-opacity|;
}

div.book {
  display: flex;
  width: 50%;
}

pre, div.pygments, .SCodeFlow {
  font-size: 1rem;
  line-height: 1.5rem;
}

tt, pre, code, .RktSym, .RktMod, .RktVal {
  font-family: "source-code-pro";
}


code, .RktSym, .RktMod, .RktVal {
    font-size: 90%;
    background: #fcfcfc;  
    padding: 0rem 0.2rem 0rem 0.2rem;
    color: #555;
    font-weight: bolder;
}

div.pygments, .SCodeFlow {
    background: #fcfcfc;
    padding: 0.5rem;
    border-top: 1px solid #ebebeb;
    border-bottom: 1px solid #ebebeb;
    margin-bottom: 1.2rem;
}

pre a {
  font-weight: bolder;
}

.gallery {
  height: 18rem;
  width: 100%;
}

.gallery button {
  position: absolute;
  top: inherit;
  bottom: -2rem;
  width: 3rem;
  opacity: 0.3;
}

.gallery button.previous {
  left: -1rem;
}

.gallery button.next {
  right: -1rem;
}

.gallery-cell {
  width: 100%;
  height: 100%;
}




download-button {
  display:inline-block;
  background: ◊|link-color|;
  color: white;
  border: 0.5rem solid ◊|link-color|;
  padding: 0.4rem;
  padding-bottom: 0rem;
}

empty-button {
 display:inline-block;
  border-top: 0.5rem solid white;
 padding-top: 0.4rem;
} 
}

◊(define hover-opacity "0.6")
download-button:hover {
  opacity: ◊|hover-opacity|;
}

download-button:active {
  opacity: 1;
}

p, li, div.pygments { 
    margin-bottom: 1rem;
    line-height: 1.4;
}

a {
  color: ◊|link-color|;
 /* font-weight: bolder; */ /* reinstate for home css */
}

a:hover {
  opacity: ◊|hover-opacity|;
  text-decoration: underline;
}

a:active {
  opacity: 1;
}

a, a:hover {
  transition: color 0.2s;
}


h1, h2, h3, h4, h5, h6 {
    font-family: cooper-hewitt;
    font-weight: inherit;
    margin: 0;
    padding: 0;
}

hr {
  border: 0;
  height: 1.5rem;
}

hr + p {
  border-top: 0.1rem solid #ddd;
  padding-top: 1rem;
}

/* overrides for blog */

article h1 { /* same as anchor color */
  color: ◊|link-color|;
}

article h1 code, article h2 code { ◊; for code blocks in headlines
  background: none;
}

article h2 {
  margin-top: 1rem;
  margin-bottom: 0.25rem;
}


article a > code { ◊; proper coloring of links [`notated like this`](url.com)
    color: ◊|link-color|;
}

article {
    font-family: charter;
    font-size: 1.2rem;
    margin-bottom: 1.5rem;
}

article blockquote {
  margin-left: 2rem;
  font-style: italic;
}

article.index {
  position: relative;
  margin-bottom: 0.5rem;
}


article div.truncate {
  position: relative;
  height: 25rem;
  overflow: hidden;
  padding-right: 2rem;
  margin-bottom: 0.5rem;
}


article div.truncate:after {
  content: "";
  text-align: right;
  position: absolute;
  bottom: 0;
  left: 0;
  width: 100%;
  height: 10rem;
  background: linear-gradient(to bottom, rgba(255, 255, 255, 0), rgba(255, 255, 255, 1) 70%);
}

article .date-and-tags {
  font-family: cooper-hewitt;
}

article col-2 {
 flex-direction: column;
}

article row {
  padding-top: 1rem;
}

article footer h2 + h2 {
  margin-top: 1rem;  
}

article footer h2 .label {
  font-size: 80%;
  opacity: 0.8;
  padding-right: 0.5em;
}

article.index a.more {
  display: inline-block;
  position: absolute;
  bottom: 1.5rem;
  border: 1px solid ◊|link-color|;
  font-family: cooper-hewitt;
  width: 5rem;
  padding: 0.5rem;
  padding-bottom: 0.25rem;
}

article.index a.more:hover {
  background: ◊|link-color|;
  color: white;
  text-decoration: inherit;
}

article ul {
  margin-left: 1.5rem;
}

ul ul { ◊; nested list
  margin-left: 2rem;
}


/* images notated in markdown */
/* are inside div.figure and have a p.caption afterward */

.figure {
  text-align: center;
  margin-bottom: 1rem;
}

.figure img {
  width: 35rem;  
}

.figure p.caption {
  display: none;
}


/* frog seems to hard-code the paginator on index page */
ul.pagination {
  display: flex;
  flex-direction: row;
  flex-wrap: nowrap;
  border-top: ◊|row-border-top|;
}

ul.pagination li {
  display: inline-block;
  flex-grow: 1;
  text-align: center;
  margin: 0;
  padding: 0;
}

ul.pagination li a {
  display: inline-block;
  width: 100%;
  height: 100%;
  padding-top: 0.75rem;
  padding-bottom: 0.5rem;
  
}

ul.pagination li a:hover, ul.pagination li.active a {
  background: ◊|link-color|;
  color: white;
  text-decoration: none;
}

ul.pagination li.active a {
  opacity: ◊|hover-opacity|;
}

.pagination .disabled {
  opacity: 0.2;
}


footer + row#bottom {
  margin-top: 0;
}

/* phone & tablet overrides */
@media all and (max-width:550px) {
  html { font-size: 15px; }
  body { padding: 0.75rem; }
  row { flex-direction: column; }
  row#logo { flex-direction: row; }
  col-1 + col-2{ margin: 0; }
  col-1 { text-align: left; }
}



◊; syntax colors adapted from Racket documentation
◊define[paren-color]{#888}
◊define[comment-color]{#c97}
◊define[keyword-color]{#07a}
◊define[name-color]{#444}
◊define[literal-color]{#275}

◊; styling classes for Pygments
.p { color: ◊|paren-color|;}
.c { color: #998; font-style: italic; } /* Comment */
.err { color: #a00; font-style: italic; } /* Error */
.o { color: #000; } /* Operator */
.cm { color: ◊|comment-color|; font-style: italic } /* Comment.Multiline */
.cp { color: ◊|comment-color|; font-style: italic } /* Comment.Preproc */
.c1 { color: ◊|comment-color|; font-style: italic } /* Comment.Single */
.cs { color: ◊|comment-color|; font-style: italic } /* Comment.Special */
.gd { color: #000; background-color: #ffdddd } /* Generic.Deleted */
.ge { color: #000; font-style: italic } /* Generic.Emph */
.gr { color: #a00; } /* Generic.Error */
.gh { color: #999; } /* Generic.Heading */
.gi { color: #000; background-color: #ddffdd } /* Generic.Inserted */
.go { color: #888; } /* Generic.Output */
.gp { color: #555; } /* Generic.Prompt */
.gs { } /* Generic.Strong */
.gu { color: #aaa; } /* Generic.Subheading */
.gt { color: #a00; } /* Generic.Traceback */
.k { color: ◊|keyword-color|; } /* Keyword */
.kc { color: ◊|keyword-color|; } /* Keyword.Constant */
.kd { color: ◊|keyword-color|; } /* Keyword.Declaration */
.kn { color: ◊|name-color|; } /* Keyword.Namespace */ ◊; captures "#lang"
.kp { color: ◊|keyword-color|; } /* Keyword.Pseudo */
.kr { color: ◊|keyword-color|; } /* Keyword.Reserved */
.kt { color: ◊|keyword-color|; } /* Keyword.Type */
.na { color: ◊|name-color|; } /* Name.Attribute */
.nb { color: ◊|keyword-color|; } /* Name.Builtin */
.nc { color: ◊|name-color|; } /* Name.Class */
.no { color: ◊|name-color|; } /* Name.Constant */
.nd { color: ◊|name-color|; } /* Name.Decorator */
.ni { color: ◊|name-color|; } /* Name.Entity */
.ne { color: ◊|name-color|; } /* Name.Exception */
.nf { color: ◊|name-color|; } /* Name.Function */
.nl { color: ◊|name-color|; } /* Name.Label */
.nn { color: ◊|name-color|; } /* Name.Namespace */
.nt { color: ◊|name-color|; } /* Name.Tag */
.nv { color: ◊|name-color|; } /* Name.Variable */
.ow { color: ◊|name-color|; } /* Operator.Word */
.w { color: #bbb; } /* Text.Whitespace */
.m { color: ◊|literal-color|; } /* Literal.Number */
.mf { color: ◊|literal-color|; } /* Literal.Number.Float */
.mh { color: ◊|literal-color|; } /* Literal.Number.Hex */
.mi { color: ◊|literal-color|; } /* Literal.Number.Integer */
.mo { color: ◊|literal-color|; } /* Literal.Number.Oct */
.s { color: ◊|literal-color|;} /* Literal.String */
.sb { color: ◊|literal-color|; } /* Literal.String.Backtick */
.sc { color: ◊|literal-color|; } /* Literal.String.Char */
.sd { color: ◊|literal-color|; } /* Literal.String.Doc */
.s2 { color: ◊|literal-color|; } /* Literal.String.Double */
.se { color: ◊|literal-color|; } /* Literal.String.Escape */
.sh { color: ◊|literal-color|; } /* Literal.String.Heredoc */
.si { color: ◊|literal-color|; } /* Literal.String.Interpol */
.sx { color: ◊|literal-color|; } /* Literal.String.Other */
.sr { color: ◊|literal-color|; } /* Literal.String.Regex */
.s1 { color: ◊|literal-color|; } /* Literal.String.Single */
.ss { color: ◊|literal-color|; } /* Literal.String.Symbol */
.bp { color: #999; } /* Name.Builtin.Pseudo */
.vc { color: #077; } /* Name.Variable.Class */
.vg { color: #077; } /* Name.Variable.Global */
.vi { color: #077; } /* Name.Variable.Instance */
.il { color: #099; } /* Literal.Number.Integer.Long */