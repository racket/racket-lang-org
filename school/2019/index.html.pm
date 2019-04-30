#lang pollen
◊(require racket/file pollen/cache)



◊gap[1]

◊(define cacm "https://cacm.acm.org/magazines/2018/3/225475-a-programmable-programming-language/fulltext")
◊(define uutah "https://www.cs.utah.edu/")
◊(define slc "https://www.visitsaltlake.com/")
◊(define gov "https://travel.utah.gov/")
◊(define com "https://utah.com/")
◊(define visit "https://www.visitutah.com/")

◊h2{◊xlink{Overview}}
◊h2{◊xlink["htdl"]{Track 1: How to Design Languages}}
◊h2{◊xlink["brw"]{Track 2: Beautiful Racket Workshop}}
◊h2{◊xlink{Location}}
◊h2{◊xlink{Schedule}}
◊h2{◊xlink{Accommodation}}
◊h2{◊xlink{Tickets}}
◊h2{◊xlink{Housing}}
◊h2{◊xlink["previous"]{Previous Racket Schools}}

◊h3{◊xtarget["overview"]{Domain-Specific Languages from Racket}}

◊link["https://racket-lang.org/" ◊img[#:style "float:left;width:5rem;padding-right:0.5rem" #:src "https://racket-lang.org/img/racket-logo.svg"]] The ◊link["https://racket-lang.org/"]{Racket} team has spent over thirty years developing and refining for creating domain-specific languages (DSLs). Racket School will introduce you to these tools and the underlying philosophy on language-oriented programming (LOP), ◊link[cacm]{an approach to software development through the lenses of DSLs} (CACM, Feb 2018).

This year, Racket School is divided into two tracks, using two different pedagogical approaches. 

For the first time, Racket School will be immediately followed by ◊link["https://con.racket-lang.org"]{RacketCon}, the annual Racket conference, on July 13 & 14. ◊strong{All Racket School tickets include admission to RacketCon.} We hope you'll stay for both events!



◊h3{◊xtarget["htdl"]{Track 1: How to Design Languages}}

A five-day class that will run July 8–12 (Monday–Friday). 

This track will be taught by ◊link["https://www.ccs.neu.edu/home/matthias/"]{Matthias Felleisen}, ◊link["https://www.eecs.northwestern.edu/~robby/"]{Robby Findler}, ◊link["https://www.cs.utah.edu/~mflatt/"]{Matthew Flatt}, and ◊link["https://jeapostrophe.github.io/home/"]{Jay McCarthy}.

◊em{This track will cover:}

◊ul[

◊li{the spectrum of DSLs within a programming language;}

◊li{the Racket tools for creating DSLs;}

◊li{the process of creating these various DSLs with Racket;}

◊li{a meta-DSL for creating typed embedded DSLs;}

◊li{the research challenges in language-oriented programming.}

]

◊em{This track is good for:}

◊ul[

◊li{academic researchers at all levels who wish to understand LOP research,}

◊li{professionals who wish to learn in depth how to rapidly build DSLs,} 

◊li{students who wish to explore a potential area of research for a PhD, and}

◊li{faculty at all levels who wish to explore an approach to teaching LOP.}

]


◊em{Prerequisites:}

◊ul[

◊li{Course-based or self-directed Racket programming experience beyond ◊link["https://htpd.org/"]{How to Design Programs} and ◊link["https://realmofracket.com"]{Realm of Racket} is needed.} 

]

Still don't know which track is right for you? ◊link["https://goo.gl/forms/TpHfA42zNvnpnzI23"]{Click here for help}.


◊h3{◊xtarget["brw"]{Track 2: Beautiful Racket Workshop}}

A three-day class that will run July 10–12 (Wednesday–Friday).

This track will be taught by ◊link["https://beautifulracket.com/about-the-author.html"]{Matthew Butterick}, author of ◊link["https://beautifulracket.com/"]{Beautiful Racket} and ◊link["https://pollenpub.com/"]{Pollen}.


◊em{This track will cover:}

◊ul[

◊li{How to make programming languages in Racket, starting from the techniques described in ◊link["https://beautifulracket.com/"]{Beautiful Racket}.}

◊li{Racket's tools for language-oriented programming, including the macro system, libraries, and the DrRacket IDE.}

]

◊em{This track is good for:}

◊ul[

◊li{Those new to language-oriented programming generally or Racket in particular.}

◊li{Those who have already worked through ◊link["https://beautifulracket.com/"]{Beautiful Racket} and want more practice.}

◊li{Working programmers looking for practical techniques.}

◊li{Academic researchers outside of computer science.}

]

◊em{Prerequisites:}

◊ul[

◊li{Some programming experience: you've moved beyond beginner level in at least one modern programming language.}

◊li{No Racket experience required.}

◊li{No computer-science training required.}

]

Still don't know which track is right for you? ◊link["https://goo.gl/forms/TpHfA42zNvnpnzI23"]{Click here for help}.



◊h3{◊xtarget{Location}}

Racket School 2019 happpens at the ◊link[uutah]{University of Utah}. The University is located in lovely ◊link[slc]{Salt Lake City}, Utah, USA.

Utah is home to several US National Parks, Monuments, and Sites, the Sundance Film Festival, the Bonneville Salt Flats, and much more. Learn more ◊link[gov]{here}, ◊link[com]{here}, or ◊link[visit]{here}. 

The summer school will be held at the ◊link["https://goo.gl/maps/bpWGz93472s"]{Warnock Engineering Building (WEB)}. To get to campus, one option is ◊link["https://www.rideuta.com/Services/TRAX"]{TRAX light rail} to either the ◊link["https://goo.gl/maps/xQuXUA2ovtP2"]{Medical Center}, Fort Douglas, or Stadium stop. Walk to WEB from there, or a ◊link["http://www.uofubus.com/"]{free campus shuttle} stops at TRAX stations and near WEB.

◊h3{◊xtarget["schedule"]{Schedule}}

Each teaching day will run roughly ◊strong{9am to 5pm},  divided into four sections of about 75 min each. Some sections will be organized as traditional ◊strong{lectures}. In ◊strong{lab sessions}, the summer school participants will practice the lecture material with hands-on exercises; teaching assistants will be on hand to assist. Participants will also have time to consult with the faculty and teaching assistants on their own language projects. 

See the ◊link["https://school.racket-lang.org/2019/plan/"]{detailed schedule here}.

◊;{

◊schedule[
◊row{◊at{8:30–9:30} ◊desc{Breakfast & Registration}}
◊row{◊at{9:30} ◊desc{Welcome}}
◊row{◊at{9:30–10:30} ◊desc{Keynote: Dan Friedman & Will Byrd: The Reasoned Racketeers}}
]
}


◊h3{◊xtarget["accommodation"]{Accommodation}}

◊;{Option 1: Racket School attendees can take advantage of our fantastic ◊link["https://gc.synxis.com/rez.aspx?Hotel=76675&Chain=10069&arrive=10/6/2017&depart=10/8/2017&adult=1&child=0&group=4AM98Y"]{group rate} at the beautiful & convenient ◊link["https://www.hoteldeca.com/"]{Hotel TBD}.}

◊;{Option 2: }We've arranged for subsidized lodging in dorms at the University of Utah. If you're staying for ◊link["https://con.racket-lang.org"]{RacketCon}, the dorms are available through the weekend. RacketCon will be held downtown, a short ride from the dorms on ◊link["https://www.rideuta.com/Services/TRAX"]{TRAX}.



◊h3{◊xtarget["costs"]{Tickets}}

◊schedule[
◊row{◊at{◊xlink["brw"]{Beautiful Racket workshop} + ◊link["https://con.racket-lang.org"]{RacketCon}} ◊desc{$299 base 
$499 patron
$149 academic (students can ◊link["https://goo.gl/forms/xgOx51hXvVYYIYXf2"]{apply for financial aid})}}

◊row{◊at{◊xlink["htdl"]{How to Design Languages} + ◊link["https://con.racket-lang.org"]{RacketCon}} ◊desc{$499 base
$699 patron
$249 academic (students can ◊link["https://goo.gl/forms/xgOx51hXvVYYIYXf2"]{apply for financial aid})}}

]


◊style[#:type "text/css"]{
.buy-button {
background:purple;
font-size:1.4rem;
letter-spacing: 0.05rem;
padding:0.5rem 0.7rem;
color:white;
font-family:bungee-regular;
display:inline-block;
border-radius: 0.5rem;
}
.buy-button:hover { opacity: 0.7 }
}

◊button[#:id "eventbrite-widget-modal-trigger-55663521090" #:type "button" #:class "buy-button"]{Buy ticket at Eventbrite}

◊strong{Base} tickets are available to all. 

◊strong{Patron} tickets cover our full cost of offering these events, plus a little extra to help support Racket development at large. Racket is part of the ◊link["https://sfconservancy.org/"]{Software Freedom Conservancy}.

◊strong{Academic} tickets are available to participants from academic institutions that need a subsidy. Tuition and travel reimbursement available for qualifying students. ◊link["https://goo.gl/forms/xgOx51hXvVYYIYXf2"]{Click here to apply}.

Eventbrite fees are added during checkout.

◊h3{◊xtarget{Housing}}

Subsidized housing is available in the University of Utah campus dorms. The cost is $40/night. ◊link["https://forms.gle/spaMYg1LL4CK3J8p9"]{Register here.}



◊h3{◊xtarget["previous"]{Previous Racket Schools}}

◊link["https://summer-school.racket-lang.org/2018/"]{2018} · ◊link["https://summer-school.racket-lang.org/2017/"]{2017} · ◊link["https://redex.racket-lang.org/summer-school.html"]{2015}
