#lang pollen
◊(require racket/file pollen/cache)



◊head["a"]{Seventh}
◊head["b"]{RacketCon}
◊head["c"]{  Seattle 2017}

◊gap[3]

◊(define this-rc-date "7–8 October 2017")

◊h2{◊xlink["register"]{◊this-rc-date}}
◊h2{◊xlink{Venue}}
◊h2{◊xlink{Speakers}}
◊h2{◊◊xlink{Register}}
◊;{◊h2{◊xlink{Schedule}}}


(seventh RacketCon) is the meeting for everyone interested in ◊link["http://racket-lang.org"]{Racket} — a ◊link["https://docs.racket-lang.org/quick/index.html"]{general-purpose programming language} that's also the ◊link["https://www.ccs.neu.edu/home/matthias/manifesto/"]{world’s first ecosystem} for developing and deploying new languages. 

RacketCon is for developers, contributors, programmers, educators, and bystanders. It's an opportunity for all of us to share plans, ideas, and enthusiasm, and help shape the future of Racket.


◊gap[0.5]

◊h3{◊xtarget["speakers"]{◊head["a"]{Keynote}}}

◊head["b"]{Dan Friedman}
◊head["b"]{& Will Byrd}

◊gap[2.5]

◊;{`div` appears at reg size; change this to `bio` once we have desc}
◊div{◊link["https://www.cs.indiana.edu/~dfried/"]{Dan Friedman} is a professor at Indiana University in the Department of Computer Science, where he studies programming languages. His books include ◊em{The Little Schemer} (with Matthias Felleisen) and ◊em{Essentials of Programming Languages} (Mitchell Wand, co-author).

◊link["http://webyrd.net/"]{Will Byrd} is an assistant professor at the University of Utah in the School of Computering, where he studies programming languages and program synthesis. For over ten years he has worked on ◊link["http://minikanren.org/"]{ miniKanren}, an embedded domain-specific language for constraint logic programming. miniKanren is described in his book ◊em{The Reasoned Schemer} (Dan Friedman and Oleg Kiselyov, co-authors).}

◊h3{◊xtarget["venue"]{Venue}}

For the first time this year, RacketCon happens in Seattle. We'll be at Mary Gates Hall on the University of Washington campus. We thank ◊link["https://homes.cs.washington.edu/~emina/index.html"]{Emina Torlak} and the UW ◊link["https://www.cs.washington.edu/"]{School of Computer Science & Engineering} for hosting us.

◊;iframe[#:src "https://www.google.com/maps/embed?pb=!1m14!1m8!1m3!1d2314.149470473195!2d-122.3093303405857!3d47.656477572249656!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x549014f2ab81c35f%3A0x6dda64382ddc87a4!2sMary+Gates+Hall%2C+1851+NE+Grant+Ln%2C+Seattle%2C+WA+98105!5e0!3m2!1sen!2sus!4v1496964066706" #:width "600" #:height "450" #:frameborder "0" #:style "border:0" #:allowfullscreen "true"]

◊div[#:id "map"]

◊gap[1]


◊h3{◊xtarget["register"]{Register}}

◊schedule[
◊row{◊at{◊span[#:style "font-weight: bolder"]{Early bird}} ◊desc{◊strong{$75 individual · $45 student (until September 16)}}}
◊row{◊at{Standard} ◊desc{$100 individual · $60 student (after September 16)}}
]

Buy your ticket at ◊link["https://www.eventbrite.com/e/racketcon-2017-tickets-34928590354"]{Eventbrite}.

RacketCon attendees also get a fantastic ◊link["https://gc.synxis.com/rez.aspx?Hotel=76675&Chain=10069&arrive=10/6/2017&depart=10/8/2017&adult=1&child=0&group=4AM98Y"]{group rate} at the beautiful & convenient ◊link["http://www.hoteldeca.com/"]{Hotel Deca}.



◊h3{◊xtarget["speakers"]{Speakers}}

◊gap[0.5]

◊;{ save this when we have speaker bios. For now they look overlarge}
◊;{
◊div[#:class "two-col"]{
◊speaker["" "Leif Andersen" ""]{}
◊speaker["" "Dan Anderson" ""]{}
◊speaker["" "Jack Firth" ""]{}
◊speaker["" "Jay McCarthy" ""]{}
◊speaker["" "Mangpo Phitchaya Phothilimthana" ""]{}
◊speaker["" "Prabhakar Ragde" ""]{}
◊speaker["" "Mark Wunsch" ""]{}
◊speaker["" "Vishesh Yadav" ""]{}
◊speaker["" "and more to come!" ""]{}
}
}

◊;{ for now, just a quick display }

◊div[#:class "two-col" #:style "font-size: 130%"]{
◊link["https://github.com/LeifAndersen"]{Leif Andersen}
◊link["https://www.viewpoint.org/"]{Dan Anderson}
◊link["https://github.com/jackfirth"]{Jack Firth}
◊link["https://jeapostrophe.github.io/home/"]{Jay McCarthy}
◊link["https://people.eecs.berkeley.edu/~mangpo/www/home.html"]{Mangpo Phitchaya Phothilimthana}
◊link["https://cs.uwaterloo.ca/~plragde/"]{Prabhakar Ragde}
◊link["http://www.markwunsch.com/"]{Mark Wunsch}
◊link["https://www.twistedplane.com/"]{Vishesh Yadav}
… and more to come!
}





◊h3{◊xtarget["schedule"]{Day 1}}

◊schedule[
◊row{◊at{9:00–9:30} ◊desc{Registration}}
◊row{◊at{9:30} ◊desc{Welcome}}
◊row{◊at{9:30–10:30} ◊desc{Keynote: Dan Friedman & Will Byrd}}
◊row{◊at{10:30–11:00} ◊desc{Break}}
◊row{◊at{11:00–12:00} ◊desc{
Speaker 1
Speaker 2
Speaker 3}}
◊row{◊at{12:00–14:00} ◊desc{Lunch}}
◊row{◊at{14:00} ◊desc{Contributor Awards}}
◊row{◊at{14:00–15:00} ◊desc{
Speaker 4
Speaker 5
Speaker 6}}
◊row{◊at{15:00–15:15} ◊desc{Break}}
◊row{◊at{15:15–16:15} ◊desc{
Speaker 7
Speaker 8
Speaker 9}}
◊row{◊at{16:15–16:30} ◊desc{Break}}
◊row{◊at{16:30–17:30} ◊desc{
Speaker 10
Speaker 11
Speaker 12}}
◊row{◊at{17:30–17:45} ◊desc{Break}}
◊row{◊at{17:45–} ◊desc{Town Hall Meeting / General Q&A}}
]




◊h3{Night 1}



◊schedule[
◊row{◊at{19:00–23:00} ◊desc{Food & drink at local brewpub ◊link["http://www.elysianbrewing.com/locations/elysian-fields/"]{Elysian Fields}}}
]



◊h3{Day 2}

New for (seventh RacketCon) — Racketeer Office Hours, a day devoted to collaborating on existing projects, planning new ones, and getting help and advice from other members of the community. Racket's core developers & many others will be there — bring your questions & projects!


◊div[#:class "two-col"]{
◊link["http://www.cs.utah.edu/~mflatt/"]{Matthew Flatt} (Racket core)
◊link["https://www.eecs.northwestern.edu/~robby/"]{Robby Findler} (Racket core, DrRacket)
◊link["http://www.ccs.neu.edu/home/samth/"]{Sam Tobin-Hochstadt} (Racket core, Typed Racket)
◊link["https://jeapostrophe.github.io/home/"]{Jay McCarthy} (Racket core, web)
◊link["http://users.eecs.northwestern.edu/~stamourv/"]{Vincent St-Amour} (Typed Racket, optimizer, Racket releases)
◊link["http://beautifulracket.com"]{Matthew Butterick} (Pollen)
}

◊;{
◊schedule[
◊row{◊at{9:00–9:30} ◊desc{Registration}}
◊row{◊at{9:30} ◊desc{Welcome}}
◊row{◊at{9:30–10:30} ◊desc{Keynote: Emina Torlak: Synthesis and Verification for All}}
]
}

◊h3{Sponsors}

◊inline-list['sponsor]{
◊link["pollenpub.com"]{Matthew Butterick}}

◊gap[1]


Sponsorship slots are still available! If you or your company would like to sponsor RacketCon, please ◊a[#:href "mailto:con@racket-lang.org"]{get in touch}.



◊h3{Previous RacketCons}

◊(define (conlink year) 
  (link (format "con.racket-lang.org/~a" year) year))

◊inline-list['con]{
◊conlink{2016}
◊conlink{2015}
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}


◊gap[2]

◊div[#:style "font-style:italic"]{"Everyone has a universe of beautiful things in their head. Maintaining a nurturing environment for conflicting interests is important. And Racket has it. So if you worry that you do weird and insignificant stuff, I tell you that the world has taught you wrong and Racket is your refugee shelter. Please do not hesitate."}
— Satisfied Customer, RacketCon 2016

