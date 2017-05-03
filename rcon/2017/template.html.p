<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>(seventh RacketCon)</title>
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
