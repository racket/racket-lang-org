#lang pollen

◊top-section{
◊span[#:id "logo" #:style "font-size:2.3rem;white-space:nowrap;"]{◊link["index.html"]{◊img[#:src "img/racket-logo.svg" #:class "logo"] Racket} ◊span[#:id "tagline" #:class "disappearing" #:style "font-size:70%;color:gray;white-space:nowrap;margin-left:0.2rem;"]{}}

◊div{
◊link[#:class "top-button disappearing-late" #:id "docs" "https://docs.racket-lang.org/"]{docs}

◊link[#:class "top-button disappearing-late" #:id "packages" "https://pkgs.racket-lang.org/"]{packages}

◊link[#:class "top-button disappearing-late" #:id "download" "https://download.racket-lang.org/"]{download}
}
}


◊special-section[#:class "one-column-body-text" #:id "pull-quote"]{In June 2018, Racket ◊link["https://sfconservancy.org/news/2018/jun/12/racketjoins/"]{became a member} of ◊link["https://sfconservancy.org/"]{Software Freedom Conservancy}, a nonprofit organization that promotes open-source software. 
}

◊section[#:class "one-column-body-text"]{

◊div{Donate
◊p[#:style "font-size: 80%;margin-top: 1rem;color: gray;width:80%;line-height:1.5"]{
Support Racket with a tax-deductible donation}}

Best option: donate via PayPal by clicking the button below.

◊html->xexpr{
<form action="https://www.paypal.com/cgi-bin/webscr" name="paypal_form" method="post" target="_blank" onsubmit="try {return window.confirm(&quot;You are submitting information to an external page.\nAre you sure?&quot;);} catch (e) {return false;}">
<input type="hidden" name="cmd" value="_s-xclick">
<input type="hidden" name="hosted_button_id" value="URMNGBCTB96G2">
<img alt="" border="0" src="https://www.paypalobjects.com/en_US/i/scr/pixel.gif" width="1" height="1">
<a style="cursor:pointer;font-size:110%;display:inline-block;margin:1em 0;" onclick="document.paypal_form.submit()" class="top-button" id="download">Click here to donate via PayPal</a>
</form>
}


We also accept checks drawn in US dollars from US banks. Make your check payable to "Software Freedom Conservancy, Inc." and put "Directed donation: Racket" as the memo. Mail to:

Software Freedom Conservancy
137 Montague St #380
Brooklyn NY 11201

Thank you for supporting Racket!
}


◊section[#:class "one-column-body-text"]{

◊div{2019 Racket Week housing}

Those attending ◊link["https://school.racket-lang.org/"]{Racket School} qualify for subsidized housing in the University of Utah campus dorms. 

Please register for housing ◊link["https://docs.google.com/forms/d/e/1FAIpQLSeLS_bAY_S16TFBv0CS61tFyDjI-EmJo1YFr_Th3s1g0JKNXg/viewform"]{with this form}, and then use the link below to pay for your stay.

◊html->xexpr{
<form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_top" name="housing_form" >
<input type="hidden" name="cmd" value="_s-xclick">
<input type="hidden" name="hosted_button_id" value="Q963289YNE97Q">
<input type="hidden" name="on0" value="How many nights?">

<div>
<select name="os0"
style="font-size:1.5rem">
    <option value="3 Nights">3 nights $120</option>
    <option value="4 Nights">4 nights $160</option>
    <option value="5 Nights">5 nights $200</option>
    <option value="6 Nights">6 nights $240</option>
    <option value="7 Nights">7 nights $280</option>
    <option value="8 Nights">8 nights $320</option>
</select> 
</div>

<input type="hidden" name="currency_code" value="USD">
<a style="cursor:pointer;font-size:110%;display:inline-block;margin:1em 0;" onclick="document.housing_form.submit()" class="top-button" id="download">Click here to pay via PayPal</a>
</form>
}


}




