
    Title:Multi-file code coverage viewing tool
    Date:2011-05-26T13:46:00.008-04:00
    Tags:

*posted by John Clements*




I'm very pleased to announce the availability of a multi-file code coverage viewer, written by Jonathan Walsh.

Torn between separating your test cases into another file and actually seeing the coverage? Well, go ahead and pull them apart, because the multi-file coverage tool displays coverage information for the files required by the present one, including both percentage covered (on a line-by-line basis) and optionally a list of uncovered lines (no more inching through your code, looking for the red highlighting.

[](http://3.bp.blogspot.com/-d4j23yei4rc/Td6l0wv3UCI/AAAAAAAAAF8/ptD2u9KwevE/s1600/coverage-button.png)
Back End:




One reason I expect this tool to be long-term robust is that it makes absolutely no changes to the back-end; that is, it just uses the existing code coverage framework. The only thing going on here is that the tool provides a way to store, load, and display this information. This means that the tool displays coverage for un-compiled files only. We thought about fiddling with this, but finally decided that the existing behavior was probably about as useful as anything else we'd come up with, and a lot more robust.

URL for docs:

[http://planet.racket-lang.org/package-source/jowalsh/code-coverage.plt/1/3/planet-docs/code-coverage/index.html](http://planet.racket-lang.org/package-source/jowalsh/code-coverage.plt/1/3/planet-docs/code-coverage/index.html)

As you might expect, it's a one-line install:

```racket
#lang racket

(require (planet jowalsh/code-coverage))
```


Please let us know about bugs you discover!