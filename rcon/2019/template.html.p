<!DOCTYPE html>
<html lang="en">
◊;{The Cloudflare caching system serves a fresh copy 
of a CSS or JS file only for each request with a distinct query string.
Therefore, append a timestamp string to CSS requests, 
to ensure that after an S3 sync, the most recent version is served.}
◊(local-require racket/file)
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta name="format-detection" content="telephone=no">
<title>ninth RacketCon</title>
<link rel="stylesheet" href="styles.css?hash=◊|(equal-hash-code (file->string styles-source))|" media="all" />
<script type="text/javascript">
function toggle_div(id) {
    e = document.getElementById(id);
    if (e.style.display == 'block') {
        e.style.display = 'none';
    }
    else if (e.style.display == 'none') {
        e.style.display = 'block';
    }
}
</script>

<script src="https://www.eventbrite.com/static/widgets/eb_widgets.js"></script>

<script type="text/javascript">
    var exampleCallback = function() {
        console.log('Order complete!');
    };

    window.EBWidgets.createWidget({
        widgetType: 'checkout',
        eventId: '55663521090',
        modal: true,
        modalTriggerElementId: 'eventbrite-widget-modal-trigger-55663521090',
        onOrderComplete: exampleCallback
    });
</script>

</head>

<body>
<div class="bg"></div>
<div class="doc">
<div class="logo"><img style="width:100px" src="https://racket-lang.org/img/racket-logo.svg"></div>
◊(->html doc #:splice #t)</div>

</body>

</html>
