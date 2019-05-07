#lang pollen

◊top-section{
◊span[#:id "logo" #:style
"font-size:2.3rem;white-space:nowrap;"]{
  ◊img[#:src "img/racket-logo.svg" #:class "logo"]
   Racket 
  ◊span[#:id "tagline" #:class "disappearing" #:style
"font-size:40%;color:gray;white-space:nowrap;margin-left:0.2rem;"]{the language-oriented programming language}}

◊div{
◊link[#:class "top-button disappearing-late" #:id "packages" "sfc.html"]{donate}
◊link[#:class "top-button disappearing-late" #:id "docs" "https://docs.racket-lang.org/"]{docs}
◊link[#:class "top-button disappearing-late" #:id "packages" "https://pkgs.racket-lang.org/"]{packages}
◊link[#:class "top-button disappearing-late" #:id "download" "https://download.racket-lang.org/"]{download}
}}

◊special-section{

◊feature["Powerful Macros, Rich DSLs" #:id "f6"]{
Racket is the first general-purpose programming language that empowers programmers to make domain-specific languages as if they were plain libraries. No external tools, no make files required.

◊doclinks{
◊link["https://docs.racket-lang.org/guide/macros.html"]{intro to macros}
◊link["https://docs.racket-lang.org/reference/Macros.html"]{macros in depth}
◊link["https://docs.racket-lang.org/guide/hash-languages.html"]{making new languages}
◊link["languages.html"]{sample #langs}
}
}

◊feature["Software Contracts, Gradual Typing" #:id "f4"]{Racket is the first language to support higher-order software contracts and safe gradual typing. Programmers can easily deploy these tools to harden their software. 

◊doclinks{
◊link["https://docs.racket-lang.org/guide/contracts.html"]{software contracts}
◊link["https://docs.racket-lang.org/ts-guide/index.html"]{the Typed Racket guide}
◊link["https://www2.ccs.neu.edu/racket/pubs/typed-racket.pdf"]{gradual typing}
}
}

◊feature["Mature, Stable, Cross-Platform" #:id "f2"]{
Racket is a mature and stable product. From the beginning, it has  supported cross-platform graphical programming (Windows, macOS, Linux). 
 
◊doclinks{
◊link["https://docs.racket-lang.org/pkg/index.html"]{package system}
◊link["https://docs.racket-lang.org/framework/index.html"]{GUI framework}
◊link["https://docs.racket-lang.org/raco/exe.html"]{standalone binaries}
◊link["https://docs.racket-lang.org/foreign/index.html"]{foreign interface}
}
}

◊feature["Batteries Included" #:id "f1"]{
Racket includes a rich set of libraries, covering the full range from web server apps to mathematics and scientific simulation software. 

◊doclinks{
◊doclink["web-server"]{web applications}
◊doclink["db"]{database}
◊doclink["math"]{math & statistics}
◊link["https://docs.racket-lang.org"]{full list ◊begin['rarr]}
}
}

◊feature["IDE Support, Integrated Documentation" #:id "f3"]{
Racket comes with support for major editors. The main bundle includes an innovative and extensible interactive development environment that has inspired other IDE projects.
                                                            
◊doclinks{
◊doclink["drracket"]{DrRacket guide}
◊doclink["drracket-tools"]{DrRacket tools}
◊link["https://docs.racket-lang.org/guide/Vim.html"]{vim}
◊link["https://docs.racket-lang.org/guide/Emacs.html"]{emacs}
}
}

◊feature["Vibrant, Open-Source Community" #:id "f5"]{
Newcomers describe the on-line Racket community as extremely friendly and helpful. Everyone is welcome to ask any question and everybody is welcome to contribute to the code base. 

◊doclinks{
◊link["https://lists.racket-lang.org/"]{mailing list} 
◊link["https://blog.racket-lang.org/2017/09/tutorial-contributing-to-racket.html"]{contributing} 
◊link["https://twitter.com/racketlang"]{twitter}
◊link["https://github.com/racket/racket/"]{github}
}
}
}

◊special-section[#:class "one-column-body-text" #:style "background:white;padding:0.8rem" #:id "pull-quote"]{
 ◊div[#:style "font-size:1.0rem"]{
◊div[#:class "container"]{
 ◊div[#:class "row"]{
  ◊div[#:id "tabbed-interface"]{
    ◊ul[#:role "tablist" #:class "nav nav-tabs nav-justified"]{
      ◊li[#:class "nav-item"]{
	◊a[#:data-toggle "tab"
           #:role "tab" 
           #:class "nav-link"
           #:aria-selected "false" 
           #:id "little-macros-tab"
	   #:aria-controls "little-macros-content-panel"
           #:style "font-weight:normal"]{Plain Macros}
	  }
      ◊li[#:class "nav-item"]{
	◊a[#:data-toggle "tab"
           #:role "tab" 
           #:class "nav-link"
           #:aria-selected "false" 
           #:id "general-purpose-tab"
	   #:aria-controls "general-purpose-content-panel"
           #:style "font-weight:normal"]{General Purpose}
	  }
      ◊li[#:class "nav-item"]{
	◊a[#:data-toggle "tab"
           #:role "tab" 
           #:class "nav-link"
           #:aria-selected "false" 
           #:id "big-macros-tab"
	   #:aria-controls "big-macros-content-panel"
           #:style "font-weight:normal"]{Powerful Macros}
	  }
      ◊li[#:class "nav-item"]{
	◊a[#:data-toggle "tab"
           #:role "tab" 
           #:class "nav-link"
           #:aria-selected "false" 
           #:id "hash-lang-tab"
	   #:aria-controls "hash-lang-content-panel"
           #:style "font-weight:normal"]{Languages from Macros}
	  }
      ◊li[#:class "nav-item"]{
	◊a[#:data-toggle "tab"
           #:role "tab" 
           #:class "nav-link"
           #:aria-selected "false" 
           #:id "ide-tab"
	   #:aria-controls "ide-content-panel"
           #:style "font-weight:normal"]{IDE Support}
	 } 
      ◊li[#:class "nav-item"]{
	◊a[#:data-toggle "tab"
           #:role "tab" 
           #:class "nav-link"
           #:aria-selected "false" 
           #:id "ugly-syntax-tab"
	   #:aria-controls "ugly-syntax-content-panel"
           #:style "font-weight:normal"]{"Ugly" Syntax}
	  }
}
}

◊special-section[#:class "one-column-body-text" #:id "little-macros-content-panel" #:role "tabpanel" #:aria-labelledby "little-macros-tab" #:style "display: none;"]{
 ◊h3[#:class "tab-title"]{Plain Macros}
 ◊img[#:src "img/little-macros.png" #:class "pb-2 ecosystem-image" #:style "width:250px"]{}
 ◊div[#:class "container-fluid"]{
 ◊h4{Plain Macros}
 ◊p{Everybody should be afraid of C macros, and nobody should
  be afraid of Racket macros. Since the Racket world deals
  with concrete syntax trees, macros are tree-rewriting
  rules, which avoids many of the syntactic problems of
  text-based macros in ordinary programming languages.}

 ◊p{Plain macros allow programmers to abstract over patterns
  when functions and procedures don't. For example, no
  programmer should be forced to write ◊docs{lambda} to
  introduce a new binding form; and in some situations, say,
  ◊docs{match} patterns, doing so is nearly impossible.}
}}

◊special-section[#:class "one-column-body-text" #:id "general-purpose-content-panel" #:role "tabpanel" #:aria-labelledby "general-purpose-tab" #:style "display: none;"]{
 ◊h3[#:class "tab-title"]{General Purpose}
 ◊img[#:src "img/general-purpose.png" #:class "pb-2 ecosystem-image" #:style "width:400px; height:470px"]{}
 ◊div[#:class "container-fluid"]{
 ◊h4{General Purpose}
 ◊p{Racket is a general-purpose programming language. 
 It comes with a range of built-in libraries, including 
 a comprehensive GUI toolbox. The GUI programs are highly 
 portable among the major platforms.}

 ◊p{When the built-in libraries don't get the job done, 
 look through the on-line catalog of user-contributed 
 packages. In all likelihood someone has already created 
 a library for scientific simulations or video scripting 
 or something else you might need. If all else fails, 
 Racket's FFI makes it easy to program in Racket as if it 
 were C---and, that's how you link in foreign libraries.}
}}

◊special-section[#:class "one-column-body-text" #:id "big-macros-content-panel" #:role "tabpanel" #:aria-labelledby "big-macros-tab" #:style "display: none;"]{
 ◊h3[#:class "tab-title"]{Big Macros}
 ◊img[#:src "img/big-macros.png" #:class "pb-2 ecosystem-image" #:style "width:600px; height:480px"]{}
 ◊div[#:class "container-fluid"]{
 ◊h4{Macros Work Together}
 ◊p{Racket's macro are more powerful than Scheme's or Lisp's. 
  If you know the latter, learning how ◊a[#:href "#little-macros"]{Plain Macros} 
  work in Racket will feel familiar. Getting to know the rest 
  of Racket's macro system will feel liberating, empowering, 
  dazzling---in short, it will provide a whole new level of 
  enlightenment for the discerning programmer.}

 ◊p{Like in Scheme, a module can export a macro. Another module 
  can import this macro. A Racket struct can act as a macro. Macros
  can define macros. Several macros can conspire to achieve a 
  common goal to which end they convey extra information 
  via structs. And these are only the beginnings of how 
  Racket macros differ from those in other languages.}
 }}

◊special-section[#:class "one-column-body-text" #:id "hash-lang-content-panel" #:role "tabpanel" #:aria-labelledby "hash-lang-tab" #:style "display: none;"]{
 ◊h3[#:class "tab-title"]{Hash Lang}
 ◊img[#:src "abstract.png" #:class "pb-2 ecosystem-image" #:style "width:400px; height:280px"]{}
 ◊div[#:class "container-fluid"]{
 ◊h4{Languages from Macros}
 ◊p{
 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
 }}}

◊special-section[#:class "one-column-body-text" #:id "ide-content-panel" #:role "tabpanel" #:aria-labelledby "ide-tab" #:style "display: none;"]{
 ◊h3[#:class "tab-title"]{IDE Support}
 ◊img[#:src "abstract.png" #:class "pb-2 ecosystem-image" #:style "width:400px; height:280px"]{}
 ◊div[#:class "container-fluid"]{
 ◊h4{IDE Support}
 ◊p{
 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
 }}}

◊special-section[#:class "one-column-body-text" #:id "ugly-syntax-content-panel" #:role "tabpanel" #:aria-labelledby "ugly-syntax-tab" #:style "display: none;"]{
 ◊h3[#:class "tab-title"]{"Ugly" Syntax}
 ◊img[#:src "abstract.png" #:class "pb-2 ecosystem-image" #:style "width:400px; height:280px"]{}
 ◊div[#:class "container-fluid"]{
 ◊h4{"Ugly" Syntax}
 ◊p{
 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
	 foo bar moo foo bar moo foo bar moo foo bar moo 
 }}}


}}}}

◊section[#:style "background:lightgray;padding:0.5rem"]{
Software

◊link["https://download.racket-lang.org/"]{Download Racket v7.2}

◊link["https://github.com/racket/racket/"]{Source code}

◊link["https://github.com/racket/racket/issues"]{Bug reports}

◊link["https://pre.racket-lang.org/installers/"]{Nightly snapshot builds}

◊link["https://pkgs.racket-lang.org/"]{Packages}
}

◊section{
Documentation & tutorials

◊link["https://docs.racket-lang.org/quick/"]{Quick introduction}

◊link["https://docs.racket-lang.org/more/"]{Systems programming}

◊link["https://docs.racket-lang.org/guide/"]{The Racket guide}

◊link["https://docs.racket-lang.org/reference/"]{The Racket reference}

◊link["https://docs.racket-lang.org/continue/"]{Web applications}

◊link["https://docs.racket-lang.org"]{All documentation}
}

◊section[#:style "background:lightgray;padding:0.5rem"]{
News

◊link["https://blog.racket-lang.org/2019/01/racket-v7-2.html"]{Racket version 7.2} is available.

◊link["https://school.racket-lang.org"]{Racket School 2019} and ◊link["https://con.racket-lang.org"]{ninth RacketCon} will happen in Salt Lake City in July 2019.

◊link["https://devswag.com/products/racket-t-shirt"]{Racket t-shirts} — the perfect way to meet friends, influence people, and stay warm.

◊link["https://devswag.com/products/racket"]{Racket stickers} — the indispensable accessory for laptops and textbooks.

}

◊section{
Community

◊link["https://lists.racket-lang.org/"]{Mailing list} and ◊link["https://blog.racket-lang.org/"]{blog}

◊link["https://botbot.me/freenode/racket/"]{#racket IRC} on freenode.net

◊link["https://racket.slack.com/"]{Slack channel} (visit ◊link["http://racket-slack.herokuapp.com/"]{this link} to sign up)

◊link["https://twitter.com/racketlang"]{@racketlang} on Twitter

◊link["team.html"]{Team}
Racket's development benefits from a large distributed pool of
contributors. 

◊link["sfc.html"]{Software Freedom Conservancy}
Make a tax-deductible contribution to support our work.

}


◊section[#:style "background:lightgray;padding:0.5rem"]{
Books


◊link["https://www.realmofracket.com/"]{Realm of Racket}
Learn to program with Racket, one game at a time.

◊link["https://beautifulracket.com/"]{Beautiful Racket}
Make your own programming languages with Racket.

◊link["http://serverracket.com"]{Server: Racket}
Develop a web application with Racket.


◊link["books.html"]{All Racket books}


}


◊section{
Education

◊link["https://www.htdp.org/"]{How to Design Programs}
A principled approach to programming.


◊link["http://programbydesign.org/"]{Program by Design}
A workshop to train teachers using ◊link["http://htdp.org/" #:style "color:gray"]{How to Design Programs} in the classroom

◊link["http://www.bootstrapworld.org/"]{Bootstrap}
A curriculum for middle-school students

}



◊section[#:id "bottom" #:class "one-column-body-text" #:style "background:lightgray;padding:0.5rem"]{
Thank you

To ◊link["http://www.nsf.gov/"]{the NSF}, ◊link["http://www.darpa.mil/"]{DARPA}, the ◊link["http://www.ed.gov/FIPSE/"]{Fund for the Improvement of Postsecondary Education (FIPSE)} at the ◊link["http://www.ed.gov/"]{US Department of Education}, the ◊link["http://www.exxonmobil.com/Corporate/community_foundation.aspx"]{Exxon Foundation}, CORD, partners of the Academy of Information Technology, ◊link["http://microsoft.com/"]{Microsoft}, ◊link["http://mozilla.org/"]{Mozilla}, and ◊link["http://google.com/"]{Google} for their generous support over the years.}

◊script[#:src "js/jquery.min.js"]{}

◊script{
var tabs = document.querySelectorAll('[role=tab]');
for (i = 0; i < tabs.length; i++) { 
 tabs[i].addEventListener("click", showTabPanel); 
}

//runs when tab is clicked
function showTabPanel(el) { 
 var tabs2 = document.querySelectorAll('[role=tab]'); 
 for (i = 0; i < tabs2.length; i++) {
   tabs2[i].setAttribute('aria-selected','false');
   tabs2[i].setAttribute('style','font-weight:normal');}
  
 showTabPanelAux(el.target);
}

function showTabPanelAux(elt) {
 elt.setAttribute('aria-selected', 'true'); 
 elt.setAttribute('style', 'font-weight:bold'); 

 var tabPanelToOpen = elt.getAttribute('aria-controls'); 
 var tabPanels = document.querySelectorAll('[role=tabpanel]'); 

 //hide all tabpanels, except for the clicked one
 for (i = 0; i < tabPanels.length; i++) { tabPanels[i].style.display = "none"; } 
 document.getElementById(tabPanelToOpen).style.display = "block"; 
}

showTabPanelAux(tabs[0]);

$('[role=tablist]').keydown(function(e) {
  if (e.keyCode==37) {
    $("[aria-selected=true]").prev().click().focus();
		    e.preventDefault(); 
  }
  if (e.keyCode==38) {
    $("[aria-selected=true]").prev().click().focus();
		    e.preventDefault();
  }
  if (e.keyCode==39) {
    $("[aria-selected=true]").next().click().focus();
		    e.preventDefault();
  }
  if (e.keyCode==40) {
    $("[aria-selected=true]").next().click().focus();
		    e.preventDefault();
  }
  });
}


