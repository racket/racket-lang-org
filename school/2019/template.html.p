<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Racket School 2019</title>
    <link rel="stylesheet" type="text/css" media="all" href="styles.css" />
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
<body onLoad="">
<div id="top-images"></div>
◊(->html ◊h1{Racket School 2019})
◊(->html ◊h1{8–12 July · Salt Lake City})
◊(->html #:tag 'div #:attrs '((id "doc")) doc)
</body>
