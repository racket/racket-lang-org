#lang pollen

◊(require (only-in "index.html.pm" top))

◊(top)

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

EIN: 41-2203632

Thank you for supporting Racket!
}




