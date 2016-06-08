<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>(sixth RacketCon)</title>
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


    var recolorable_paths = [];

    function random_int(range) {
        return Math.floor(Math.random() * (range + 1));
    }

    function add_to_recolorable_paths(id) {
        var paths = document.getElementById(id).getSVGDocument().getElementsByTagName("path");

        for (var i = 0 ; i < paths.length ; i++) {
            if (paths[i].id.indexOf("recolor") == 0) {
                recolorable_paths.push(paths[i]);
            }
        }
    }

    function gather_recolorable_paths() {
        add_to_recolorable_paths('rcon_svg');
        add_to_recolorable_paths('keynote_svg');
        add_to_recolorable_paths('speakers_svg');
    }

    function flip_color(path) {
        var next_color = (path.getAttribute("stroke") == "◊rcon-red") ? "◊rcon-blue" : "◊rcon-red" ;
        path.setAttribute("stroke", next_color);

        if (path.getAttribute("stroke-width") == "8.25" && random_int(2) == 1) {
            path.setAttribute("stroke-width", "32");
        }

        if (path.getAttribute("stroke-width") == "32" && random_int(2) == 1) {
            path.setAttribute("stroke-width", "8.25");
        }
    }

    function flip_color_of_random_paths() {
        for (var i = 0 ; i < 15 ; i++) {
            var which = random_int(recolorable_paths.length);
            flip_color(recolorable_paths[which]);
        }
    }
        

    </script>
</head>
<body onLoad="gather_recolorable_paths();setInterval(flip_color_of_random_paths, 1000)">
    ◊(->html #:tag 'div #:attrs '((id "doc")) doc)
</body>
