<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>(seventh RacketCon)</title>
    <link rel="stylesheet" type="text/css" media="all" href="styles.css" />

        <script type="text/javascript" src="https://maps.googleapis.com/maps/api/js?key=AIzaSyCBUv3GrIsTLWr9dO8A_pUetfMsO0w0x6o"></script>
        
        <script type="text/javascript">
            // When the window has finished loading create our google map below
            google.maps.event.addDomListener(window, 'load', init);
        
            function init() {
                // Basic options for a simple Google Map
                // For more options see: https://developers.google.com/maps/documentation/javascript/reference#MapOptions
                var mapOptions = {
                    // How zoomed in you want the map to start at (always required)
                    zoom: 15,

                    // The latitude and longitude to center the map (always required)
                    center: new google.maps.LatLng(47.654839,-122.307209), // New York

                    // prevents map from catching scroll wheel events
                    scrollwheel:  false,

                    // How you would like to style the map. 
                    // This is where you would paste any style found on Snazzy Maps.
                    styles: [{"featureType":"administrative","elementType":"all","stylers":[{"saturation":"-100"}]},{"featureType":"administrative.province","elementType":"all","stylers":[{"visibility":"off"}]},{"featureType":"landscape","elementType":"all","stylers":[{"saturation":-100},{"lightness":65},{"visibility":"on"}]},{"featureType":"poi","elementType":"all","stylers":[{"saturation":-100},{"lightness":"50"},{"visibility":"simplified"}]},{"featureType":"road","elementType":"all","stylers":[{"saturation":"-100"}]},{"featureType":"road.highway","elementType":"all","stylers":[{"visibility":"simplified"}]},{"featureType":"road.arterial","elementType":"all","stylers":[{"lightness":"30"}]},{"featureType":"road.local","elementType":"all","stylers":[{"lightness":"40"}]},{"featureType":"transit","elementType":"all","stylers":[{"saturation":-100},{"visibility":"simplified"}]},{"featureType":"water","elementType":"geometry","stylers":[{"hue":"#ffff00"},{"lightness":-25},{"saturation":-97}]},{"featureType":"water","elementType":"labels","stylers":[{"lightness":-25},{"saturation":-100}]}]
                };



                // Get the HTML DOM element that will contain your map 
                // We are using a div with id="map" seen below in the <body>
                var mapElement = document.getElementById('map');

                // Create the Google Map using our element and options defined above
                var map = new google.maps.Map(mapElement, mapOptions);

                // Let's also add a marker while we're at it
                var marker = new google.maps.Marker({
                    position: new google.maps.LatLng(47.654839,-122.307209),
                    map: map,
                    title: 'Mary Gates Hall'
                });

                // recenter the map on resize
                google.maps.event.addDomListener(window, "resize", function() {
                    var center = map.getCenter();
                    google.maps.event.trigger(map, "resize");
                    map.setCenter(center); 
                });
            }
        </script>


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

    var movable_div_ids = [];

    function gather_movable_div_ids() {
        divs = document.getElementsByClassName("movable");
        for (var i = 0 ; i < divs.length ; i++) {
            movable_div_ids.push(divs[i].id);
        }
    }


    function random_int(range) {
        return Math.floor(Math.random() * range);
    }

    function move_div() {
        for (var i = 0 ; i < movable_div_ids.length ; i++) {
        var dist = 6
        var new_top = String((random_int(dist) - (dist/2))) + "rem";
        var new_left = String((random_int(dist) - (dist/2))) + "rem";
        var new_translate = "translate3d(" + new_top + "," + new_left + ", 0)";
        document.getElementById(movable_div_ids[i]).style.transform = new_translate;
    }
}
        

    </script>
</head>
<body onLoad="gather_movable_div_ids();move_div();setInterval(move_div, 3000)">
    ◊(->html #:tag 'div #:attrs '((id "doc")) doc)
</body>
