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
    var blue = "rgb(52.941176%,80.784314%,98.039216%)";
    var red = "rgb(86.27451%,7.843137%,23.529412%)";

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
        add_to_recolorable_paths('also_svg');

        for (var i = 0 ; i < recolorable_paths.length ; i++) {
            recolorable_paths[i].setAttribute("stroke", (random_int(2) == 0) ? red : blue);
        }
    }

    function flip_color(path) {
        var next_color = (path.getAttribute("stroke") == red) ? blue : red;
        path.setAttribute("stroke", next_color);
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
