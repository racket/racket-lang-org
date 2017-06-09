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

                    // How you would like to style the map. 
                    // This is where you would paste any style found on Snazzy Maps.
                    styles: [{"featureType":"all","elementType":"geometry","stylers":[{"visibility":"on"}]},{"featureType":"all","elementType":"labels","stylers":[{"visibility":"on"}]},{"featureType":"administrative","elementType":"geometry.fill","stylers":[{"visibility":"off"}]},{"featureType":"administrative.country","elementType":"all","stylers":[{"visibility":"off"}]},{"featureType":"administrative.province","elementType":"all","stylers":[{"visibility":"off"}]},{"featureType":"administrative.locality","elementType":"labels","stylers":[{"visibility":"off"}]},{"featureType":"administrative.locality","elementType":"labels.text.fill","stylers":[{"color":"#2c2c2c"},{"weight":"2.00"}]},{"featureType":"administrative.neighborhood","elementType":"labels","stylers":[{"visibility":"off"}]},{"featureType":"administrative.neighborhood","elementType":"labels.text.fill","stylers":[{"color":"#696969"}]},{"featureType":"landscape","elementType":"geometry.fill","stylers":[{"color":"#ffffff"}]},{"featureType":"landscape.man_made","elementType":"geometry.fill","stylers":[{"visibility":"off"}]},{"featureType":"poi","elementType":"geometry.fill","stylers":[{"visibility":"off"}]},{"featureType":"poi.park","elementType":"geometry.fill","stylers":[{"color":"#f6f6f6"},{"visibility":"off"}]},{"featureType":"road","elementType":"geometry.fill","stylers":[{"weight":"0.50"}]},{"featureType":"road","elementType":"geometry.stroke","stylers":[{"visibility":"off"}]},{"featureType":"road.highway","elementType":"geometry.fill","stylers":[{"color":"#565656"},{"weight":"0.50"}]},{"featureType":"road.highway","elementType":"geometry.stroke","stylers":[{"visibility":"off"},{"weight":"0.01"}]},{"featureType":"road.highway.controlled_access","elementType":"geometry.fill","stylers":[{"weight":"0.50"},{"color":"#8e8e8e"}]},{"featureType":"road.highway.controlled_access","elementType":"geometry.stroke","stylers":[{"visibility":"off"}]},{"featureType":"road.arterial","elementType":"geometry.fill","stylers":[{"color":"#000000"}]},{"featureType":"road.arterial","elementType":"geometry.stroke","stylers":[{"visibility":"off"},{"weight":"0.50"}]},{"featureType":"road.local","elementType":"geometry.fill","stylers":[{"color":"#b7b7b7"}]},{"featureType":"road.local","elementType":"geometry.stroke","stylers":[{"visibility":"off"}]},{"featureType":"transit.line","elementType":"geometry.fill","stylers":[{"color":"#404040"}]},{"featureType":"transit.line","elementType":"geometry.stroke","stylers":[{"visibility":"off"}]},{"featureType":"transit.station","elementType":"geometry.fill","stylers":[{"visibility":"off"}]},{"featureType":"water","elementType":"geometry.fill","stylers":[{"color":"#dedede"}]}]
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
