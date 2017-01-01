<!DOCTYPE html>
<html>
<head>
<title>Racket</title>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
◊;{170101: The Cloudflare caching system serves a fresh copy 
of a CSS or JS file only for each request with a distinct query string.
Therefore, append a timestamp string to these requests, 
to ensure that after an S3 sync, the most recent version is served.}
◊(define ts-str (timestamp-string))
<link rel="stylesheet" href="css/styles.css?ts=◊|ts-str|" media="screen"/>
<link rel="stylesheet" href="css/fonts/fonts.css?ts=◊|ts-str|" media="screen"/>
<script type="text/javascript" src="js/functions.js?ts=◊|ts-str|"></script>
</head>
<body>
◊(->html doc)
</body>
</html>
