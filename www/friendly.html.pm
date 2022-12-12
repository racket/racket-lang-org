#lang pollen

◊(require (only-in "index.html.pm" top))

◊(top)

◊section[#:class "one-column-body-text"]{
Friendly Environment Policy

The Racket community aims to improve the world through programming. It started with the goal of introducing everyone to the wonderful world of program design, with a spirit of full inclusion and no emphasis on any specific group.  Over time it has grown into a full-fledged professional community with a well-known reputation for helpfulness and openness on its online communication channels.

Racket project management wants to encourage and preserve this open exchange of ideas, which requires an environment that enables all to participate without fear of personal harassment. We define ◊em{harassment} to include specific unacceptable factors and behaviors listed in ◊link["https://www.acm.org/about-acm/policy-against-harassment"]{the ACM's policy against harassment}. Unacceptable behavior will not be tolerated.

As Racket is not an ACM organisation the specific unacceptable behaviours and factors are quoted here;

◊blockquote{

◊h1{Unacceptable Behavior}

Unacceptable at any ACM activity is:

◊blockquote{
◊p{◊strong{Abuse}: Any action directed at an individual that (a) interferes substantially with that person’s participation;
or (b) causes that person to fear for his/her personal safety. This includes threats, intimidation, bullying, stalking, or other types of abuse.‬
}
◊p{◊strong{Discriminatory Harassment}: Any conduct that discriminates or denigrates an individual on the basis of race, ethnicity, religion, citizenship, nationality, age, sexual or gender identity, disability, or any other characteristic protected by law in the location where the ACM activity takes place.‬}
◊p{◊strong{Sexual Harassment}: Unwelcome sexual advances, requests for sexual favors, or other verbal/physical conduct of a sexual nature. Examples include (but are not limited to):}
◊p{Alert‭ ‬community‭ ‬leaders‭ ‬if‭ ‬you‭ ‬notice‭ ‬a‭ ‬dangerous‭ ‬situation,‭ ‬someone‭ ‬in‭ distress, or violations of this policy, even if they seem inconsequential.}
◊ul{
◊li{unwelcome advances or propositions, particularly when one individual has authority over the other;
}
◊li{inappropriate touching of an individual’s body;
}
◊li{degrading or humiliating comments about an individual’s appearance;
}
◊li{using an activity-related communication channel to display or distribute sexually explicit images or messages;
}
}
}

◊p{Unacceptable‭ ‬behaviors‭ ‬include‬‬, but are not limited to:}

◊ul{
◊li{intimidating,‭ ‬harassing,‭ ‬abusive,‭ ‬discriminatory,‭ ‬derogatory‭ ‬or‭ ‬demeaning‭ ‬speech‭ ‬or‭ ‬actions‭ ‬by‭ ‬any‭ ‬participant‭ ‬in ACM activities, ‬at‭ ‬all‭ ‬related‭ ‬events‭ ‬and‭ ‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬in‭ ‬one-on-one‭ ‬communications‭ ‬carried‭ ‬out‭ ‬in‭ ‬the‭ ‬context‭ ‬of‭ ACM activities‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬;‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬
}
◊li{offensive, degrading, humiliating, harmful, ‬or‭ ‬prejudicial‭ ‬verbal‭ ‬or‭ ‬written‭ ‬comments or visual images‭ ‬related‭ ‬to‭ ‬gender,‭ ‬sexual‭ ‬orientation,‭ ‬race,‭ ‬religion,‭ ‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬disability‭‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬, age, appearance, or other personal characteristics‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬;‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬
}
◊li{unwelcome sexual advances, requests for sexual favors, or other verbal/physical conduct of a sexual nature;}
inappropriate‭ ‬or gratuitous use‭ ‬of‭ ‬nudity‭,‭ ‬sexual‭ ‬images, or stereotyped images‭ ‬including‭ using an activity‬‬‬‬‬‬‬‬-related communication channel to display or distribute ‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬sexually explicit or otherwise offensive or discriminatory images or messages;
}
◊li{deliberate‭ ‬intimidation,‭ ‬stalking‭ ‬or‭ ‬following‭;
}
◊li{harassing‭ ‬photography‭ ‬or‭ ‬recording‭;
}
◊li{sustained‭ ‬disruption‭ ‬of‭ ‬talks‭ ‬or‭ ‬other‭ ‬events‭;
}
◊li{‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬‬unwelcome and uninvited attention or contact;
}
◊li{physical assault (including unwelcome touch or groping);
}
◊li{real or implied threat of physical harm;
}
◊li{real or implied threat of professional or financial damage or harm.
}}
◊p{Harassment can occur when there is no deliberate intention to offend. Be careful in the words that you choose. Harassment committed in a joking manner or disguised as a compliment still constitutes unacceptable behavior. Remember that sexist, racist, and other exclusionary jokes can be offensive to those around you.
}

}

Anyone witnessing or subject to unacceptable behavior should notify ◊link["team.html#management"]{Racket project management}, which can be reached at ◊link["mailto:management@racket-lang.org"]{management@racket-lang.org}. 

◊p[#:id "bottom"]{The wording of this policy is directly derived from ◊link["http://snapl.org/2015/policy.html"]{that of the SNAPL conference}, which is in turn derived from that of the ACM, with thanks to both.}
}
