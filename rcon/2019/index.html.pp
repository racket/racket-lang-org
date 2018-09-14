#lang pollen
<!DOCTYPE html>
<html lang="en">
◊(require css-tools)
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta name="format-detection" content="telephone=no">
<title>Racket Week 2019</title>
<style type="text/css">

◊(ffd/rp "barcode-128" "Libre_Barcode_128/LibreBarcode128-Regular.ttf" #:base64 #t)
◊(ffd/rp "plex-light" "IBM-Plex-Mono/IBMPlexMono-LightItalic.ttf" #:base64 #t)
◊(ffd/rp "plex-dark" "IBM-Plex-Mono/IBMPlexMono-BoldItalic.ttf" #:base64 #t)

body {
    background: #333;
    margin: 10vw 0;
}

.barcode {
    font-family: "barcode-128";
    font-size: 10vw;
    opacity: 0.5;
    line-height: 0.6;
    padding-top: 0.4em;
    margin-top: 2.5vw;
    margin-bottom: 1.5vw;
    transform: scale(1, 0.5);
    text-shadow: none;
}

* {
    font-family: "plex-light";
    font-size: 5.5vw;
    color: white;
    background: none;
    text-align: center;
    ◊;{ss01=two story a, ss02 = two story g }
    ◊make-css-ot-features['("ss01" "ss02") '(1 1)];
    margin: 0;
    padding: 0;

}

strong {
    font-family: "plex-dark";
    text-shadow:  0 0 1.00em #fff, 0 0 1.50em #fff;
}

body > div, input {
    text-shadow:  0 0 1.00em #fff;
    mix-blend-mode: lighten; 
    border: none;
}

.bg {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    mix-blend-mode: normal;
    /* Photo by Wil Stewart https://unsplash.com/photos/T26KCgCPsCI */
    background: black url(stars3b.jpg) repeat top center;
    background-size: 100% 100%;
    opacity: 0.3;
}

.hidden {
    opacity: 0;
}


</style>

<script type="text/javascript">

function append_barcode(el) {
    const barcode_div = document.createElement("div");
    barcode_div.append(el.innerText);
    barcode_div.classList.add("barcode");
    el.append(barcode_div);
}

function select_all() { 
    document.execCommand('selectAll', false, null); 
}

function update_clock() {
    const registration_opens = new Date("February 1, 2019");
    const now = new Date();
    const diff = registration_opens.getTime() - now.getTime();
    const new_inner_text = `in ${Math.floor(diff)}ms`;
    document.getElementById("clock_barcode").innerText = new_inner_text;
    document.getElementById("clock").innerText = new_inner_text;
    const next_timeout = Math.pow(5, 1 + Math.random() * 4);
    window.setTimeout(update_clock, next_timeout);
}

function finish_setup() {
    Array.from(document.getElementsByClassName("barcoded")).map(append_barcode);

    const email_field = document.getElementById("tlemail");
    function sync_email_field() {
        document.getElementById("email_barcode").innerText = email_field.value || "x";
    }
    email_field.addEventListener("input", sync_email_field);
    email_field.addEventListener("focus", event => {setTimeout(select_all);});
    email_field.setAttribute("size", email_field.value.length);
    sync_email_field();

    update_clock();

    document.getElementById("content").classList.remove("hidden");
}

window.addEventListener("load", finish_setup);

</script>

</head>

<body>
<div class="bg"></div>

<div id="content" class="hidden">
<div style="margin-bottom:8vw"><img style="width: 10vw" src="http://racket-lang.org/img/racket-logo.svg"></div>
<div class="barcoded"><strong>Racket Week</strong> 8-14 July 2019</div>
<div class="barcoded">Salt Lake City UT USA</div>
<div id="rs" class="barcoded"><strong>Racket School</strong>: two tracks</div>
<div class="barcoded">How to Design Languages (5 day intensive)</div>
<div class="barcoded">Beautiful Racket Workshop (3 day gentle)</div>
<div class="barcoded">followed by <strong>RacketCon</strong></div>
<div class="barcoded">Registration opens</div>
<div id="clock">in 12386797s</div>
<div id="clock_barcode" class="barcode">foo</div>


<form action="https://tinyletter.com/racketweek" method="post" onsubmit="window.open('https://tinyletter.com/racketweek', 'popupwindow', 'scrollbars=yes,width=800,height=600');return true" target="popupwindow">

<input id="tlemail" name="email" style="border-bottom:1px solid #ccc" type="input" value="enter email for updates">
<div id="email_barcode" class="barcode">Enter email for updates</div>
<input name="embed" type="hidden" value="1"/>
<input style="padding:0.10em 0.50em;cursor:pointer;border:2px solid white" type="submit" value="submit" id="submit">
</form>
</div>


</body>
</html>